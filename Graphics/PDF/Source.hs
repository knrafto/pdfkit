{-# LANGUAGE RankNTypes #-}
module Graphics.PDF.Source
    ( -- * Sources
      Source(..)
      -- * Construction
    , handleSource
    , byteStringSource
      -- * Parsing
    , parseStream
    ) where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Attoparsec
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import           Data.Int
import           Pipes
import           Pipes.Attoparsec           as P
import           Pipes.ByteString
import           System.IO

-- | A random-access streaming source.
data Source m = Source { streamFrom :: Int64 -> Producer B.ByteString m () }

-- | Turn a 'Handle' into a 'Source'. The handle must be seekable.
handleSource :: MonadIO m => Handle -> Source m
handleSource h = Source $ \pos -> do
    liftIO $ hSeek h AbsoluteSeek (fromIntegral pos)
    fromHandle h

-- | Turn a lazy 'L.ByteString' into a 'Source'.
byteStringSource :: Monad m => L.ByteString -> Source m
byteStringSource s = Source $ \pos -> fromLazy (L.drop pos s)

-- | Parse a stream, returning a result and the rest of the stream.
parseStream
    :: (Functor m, Monad m)
    => Parser a
    -> Producer B.ByteString m r
    -> m (Either P.ParsingError a, Producer B.ByteString m r)
parseStream parser = runStateT $ fmap snd <$> P.parse parser
