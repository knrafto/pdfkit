{-# LANGUAGE OverloadedStrings #-}
-- | Low-level lexical conventions of PDF files.
module Graphics.PDF.Parse.Syntax
    ( -- * PDF lexical conventions
      isSpace
    , isDelim
    , isNormal
    , skipSpace
    , token
      -- * Octal digits
    , isOctalDigit
    , fromOctalDigit
      -- * Hexadecimal digits
    , isHexDigit
    , fromHexDigit
    ) where

import           Control.Applicative
import           Data.Attoparsec
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.ByteString       as B
import           Data.Word

-- | Select PDF white-space characters.
isSpace :: Word8 -> Bool
isSpace = (`B.elem` "\0\t\n\f\r ")

-- | Select PDF delimiters.
isDelim :: Word8 -> Bool
isDelim = (`B.elem` "()<>[]{}/%")

-- | Select normal PDF characters.
isNormal :: Word8 -> Bool
isNormal w = not $ isSpace w || isDelim w

-- | Skip white-space and comments.
skipSpace :: Parser ()
skipSpace = skipWhile isSpace <* optional (skipComment *> skipSpace)
  where
    skipComment = A8.char '%' *> A8.takeTill (== '\n')

-- | Parse a single token, which consists of only normal characters.
token :: Parser B.ByteString
token = takeWhile1 isNormal

-- | Select octal digits.
isOctalDigit :: Word8 -> Bool
isOctalDigit w = w >= 48 && w <= 55

-- | Unsafely convert an octal digit to a number.
fromOctalDigit :: Word8 -> Word8
fromOctalDigit w = w - 48

-- | Select hexadecimal digits (both lowercase and uppercase).
isHexDigit :: Word8 -> Bool
isHexDigit w = 48 <= w && w <= 57
            || 65 <= w && w <= 70
            || 97 <= w && w <= 102

-- | Unsafely convert a hexademinal digit to a number.
fromHexDigit :: Word8 -> Word8
fromHexDigit w
    | 65 <= w && w <= 70  = w - 55
    | 97 <= w && w <= 102 = w - 87
    | otherwise           = w - 48
