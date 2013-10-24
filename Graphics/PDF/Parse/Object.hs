{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of PDFs into objects.
module Graphics.PDF.Parse.Object
    ( object
    , streamDict
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec              as A hiding (word8)
import qualified Data.Attoparsec.Char8        as A8
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as L
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Unsafe       as B
import           Data.Char                    (ord)
import qualified Data.HashMap.Strict          as H
import           Data.List
import           Data.Monoid
import           Data.Ratio
import qualified Data.Vector                  as V

import           Graphics.PDF.Parse.Syntax
import           Graphics.PDF.Types

-- | @'manyUpTo' n p@ applies @p@ zero or more times, up to a maximum of
-- @n@ times.
manyUpTo :: Alternative f => Int -> f a -> f [a]
manyUpTo 0 _ = pure []
manyUpTo n p = (:) <$> p <*> manyUpTo (n - 1) p <|> pure []

-- | @'beginEndPy' p sep@ parses a list of @p@s, with @sep@ before, after,
-- and in between.
beginEndBy :: Parser a -> Parser b -> Parser [a]
beginEndBy p sep = sep *> many (p <* sep)

-- | Convert a 'Builder' to a string 'B.ByteString'.
toByteString :: Builder -> B.ByteString
toByteString = L.toStrict . toLazyByteString

-- | Parse an PDF object.
object :: Parser Object
object = indirectObject <|> numericObject <|> directObject

-- | Parse an indirect object or an indirect reference.
indirectObject :: Parser Object
indirectObject = do
    objID <- objectID
    skipSpace
    reference objID <|> indirect objID
  where
    objectID = ObjectID <$> A8.decimal <* skipSpace <*> A8.decimal

    reference objID = Reference objID <$ A8.char 'R'
    indirect  objID = Indirect objID <$ string "obj" <*
        skipSpace <*> directObject <* skipSpace <*
        string "endobj"

-- | Parse a numeric object.
numericObject :: Parser Object
numericObject = Number <$> number
  where
    number = F <$> A8.signed fixed
         <|> I <$> A8.signed A8.decimal

    fixed = do
        i  <- option 0 A8.decimal
        _  <- A8.char '.'
        ds <- A.takeWhile A8.isDigit_w8
        let f = case parseOnly A8.decimal ds of
                    Left  _ -> 0
                    Right d -> d % (10 ^ B.length ds)
        return $ fromRational (i % 1 + f)

-- | Parse a direct object.
directObject :: Parser Object
directObject = do
    c <- A8.satisfy (`B8.elem` "ntf(/[<")
    case c of
        'n' -> Null       <$  string "ull"
        't' -> Bool True  <$  string "rue"
        'f' -> Bool False <$  string "alse"
        '(' -> String     <$> literalString_
        '/' -> Name       <$> name_
        '[' -> Array      <$> array_
        '<' -> Dict       <$  A8.char '<' <*> dict_
           <|> String     <$> hexString_
        _   -> error "panic: impossible case"

-- | Parse a literal string, without the leading parenthesis.
literalString_ :: Parser B.ByteString
literalString_ = do
    s <- scan (0 :: Int, False) f <* A8.char ')'
    unescape '\\' stringEscape s
  where
    f (n, True) _  = Just (n, False)
    f (n, _   ) 40 = Just (n + 1, False)  -- open paren
    f (0, _   ) 41 = Nothing              -- close paren (final)
    f (n, _   ) 41 = Just (n - 1, False)  -- close paren
    f (n, _   ) 92 = Just (n, True)       -- backslash
    f s         _  = Just s

-- | Parse a hexidecimal string, without the leading angle bracket.
hexString_ :: Parser B.ByteString
hexString_ = decodeHexString <$> A8.takeWhile (/= '>') <* A8.char '>'

-- | Parse a name object.
name :: Parser B.ByteString
name = A8.char '/' *> name_

-- | Parse a name object, without the leading slash.
name_ :: Parser B.ByteString
name_ = do
    s <- takeTill (\w -> isSpace w || isDelim w)
    unescape '#' nameEscape s

-- | Parse an array, without the leading bracket.
array_ :: Parser Array
array_ = V.fromList <$> beginEndBy object skipSpace <* A8.char ']'

-- | Parse a dictionary, without the leading double angle bracket.
dict_ :: Parser Dict
dict_ = H.fromList <$> beginEndBy pair skipSpace <* string ">>"
  where
    pair = (,) <$> name <* skipSpace <*> object

-- | Parse a stream dictionary, swallowing the @stream@ token before the
-- actual stream content.
streamDict :: Parser Dict
streamDict = string ">>" *> dict_ <* skipSpace <* string "stream" <* eol
  where
    eol = optional (A8.char '\r') *> A8.char '\n'

-- | Decode a hexademical string, ignoring non-hexadecimal characters.
decodeHexString :: B.ByteString -> B.ByteString
decodeHexString = toByteString . go . B.filter isHexDigit
  where
    go b = case B.length h of
        0 -> mempty
        1 -> word8 (16 * digit 0)
        _ -> word8 (16 * digit 0 + digit 1) <> go t
      where
        (h, t) = B.splitAt 2 b
        digit  = fromHexDigit . B.unsafeIndex h

-- | @'unescape' c p b@ unescapes string @b@, where @c@ is the escape
-- character and @p@ parses an escape sequence.
unescape :: Char -> Parser Builder -> B.ByteString -> Parser B.ByteString
unescape c escapeParse input
    | escapeChar `B.elem` input = either fail (return . toByteString) $
                                  parseOnly (go mempty) input
    | otherwise                 = return input
  where
    escapeChar = fromIntegral (ord c)

    go acc = do
        h <- A.takeWhile (/= escapeChar)
        done <- atEnd
        if done
            then return (acc <> byteString h)
            else do _ <- anyWord8
                    e <- escapeParse
                    go (acc <> byteString h <> e)

-- | Parse literal string escape sequences.
stringEscape :: Parser Builder
stringEscape = octalEscape <|> charEscape
  where
    octalEscape = do
        s <- manyUpTo 3 (satisfy isOctalDigit)
        decodeOctal s <$ guard (not $ null s)

    charEscape = lookupEscape <$> anyWord8

    decodeOctal = word8 . foldl' (\acc d -> 8*acc + fromOctalDigit d) 0

    escapes = "nrtbf()\\"
    mapping = "\n\r\t\b\f()\\"

    lookupEscape t
        | t == 10   = mempty  -- newline
        | otherwise = word8 $ case B.findIndex (== t) escapes of
            Nothing -> t
            Just i  -> B.unsafeIndex mapping i

-- | Parse anem escape sequences.
nameEscape :: Parser Builder
nameEscape = do
    h <- A.take 2
    let digit = fromHexDigit . B.unsafeIndex h
    word8 (16 * digit 0 + digit 1) <$ guard (B.all isHexDigit h)
