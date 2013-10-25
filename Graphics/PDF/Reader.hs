{-# LANGUAGE MultiParamTypeClasses, Rank2Types, DeriveDataTypeable #-}
-- | A PDF parsing monad.
module Graphics.PDF.Reader
    ( PDFReaderT(..)
    , Context(..)
    , PDFError(..)
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Monoid
import Data.Typeable

import Graphics.PDF.Source

-- | PDF errors. Needs love.
data PDFError = PDFError
    deriving (Show, Typeable)

instance Exception PDFError

-- | PDF parsing context.
data Context m = Context
    { contextSource  :: Source m
    }

-- | A continuation-based parsing monad.
newtype PDFReaderT m a = PDFReaderT
    { runPDFReaderT :: forall r.
                       Context m
                    -> (PDFError -> m r)
                    -> (a -> m r)
                    -> m r
    }

instance Functor (PDFReaderT m) where
    fmap f m = PDFReaderT $ \c kf ks ->
        let ks' a = ks (f a)
        in  runPDFReaderT m c kf ks'

instance Applicative (PDFReaderT m) where
    pure  = return
    (<*>) = ap

instance Alternative (PDFReaderT m) where
    empty   = fail "empty"
    a <|> b = PDFReaderT $ \c kf ks ->
        let kf' _ = runPDFReaderT b c kf ks
        in  runPDFReaderT a c kf' ks

instance Monad (PDFReaderT m) where
    return a = PDFReaderT $ \_ _ ks -> ks a

    m >>= g = PDFReaderT $ \c kf ks ->
        let ks' a = runPDFReaderT (g a) c kf ks
        in  runPDFReaderT m c kf ks'

    fail _ = PDFReaderT $ \_ kf _ -> kf PDFError

instance MonadPlus (PDFReaderT m) where
    mzero = fail "mzero"
    mplus = (<|>)

instance Monoid (PDFReaderT m a) where
    mempty  = fail "mempty"
    mappend = (<|>)

instance MonadReader (Context m) (PDFReaderT m) where
    ask       = PDFReaderT $ \c _  ks -> ks c
    local f m = PDFReaderT $ \c kf ks -> runPDFReaderT m (f c) kf ks
    reader f  = PDFReaderT $ \c _  ks -> ks (f c)

instance MonadError PDFError (PDFReaderT m) where
    throwError e   = PDFReaderT $ \_ kf _ -> kf e
    catchError m f = PDFReaderT $ \c kf ks ->
        let kf' e = runPDFReaderT (f e) c kf ks
        in  runPDFReaderT m c kf' ks

instance MonadTrans PDFReaderT where
    lift m = PDFReaderT $ \_ _ ks -> m >>= ks

instance MonadIO m => MonadIO (PDFReaderT m) where
    liftIO = lift . liftIO
