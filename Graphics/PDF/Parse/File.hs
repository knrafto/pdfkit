{-# LANGUAGE OverloadedStrings #-}
-- | Parsing the PDF file structure.
module Graphics.PDF.Parse.File
    ( version
    , xrefOffset
    ) where

import           Control.Applicative
import           Data.Attoparsec           as A hiding (word8)
import qualified Data.Attoparsec.Char8     as A8

import           Graphics.PDF.Parse.Syntax

-- | Skip zero or more.
many_ :: Alternative f => f a -> f ()
many_ p = go
  where
    go = p *> go <|> pure ()

-- | Parse the PDF header, containing the version.
version :: Parser (Int, Int)
version = (,) <$ string "%PDF-" <*> A8.decimal <* A8.char '.' <*> A8.decimal

-- | Search for an parse the cross-reference table offset (signalled by
-- the @xrefoffset@ token).
xrefOffset :: Parser Integer
xrefOffset = skipSpace *> go
  where
    go = do
        many_ (takeWhile1 isDelim <* skipSpace)
        t <- token
        if t == "startxref"
            then skipSpace *> A8.decimal <* skipWhile isSpace <* string "%%EOF"
            else xrefOffset
