{-# LANGUAGE FlexibleContexts #-}

module Utils where

import Config
import System.Directory
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Except
import Servant.Server

validateName :: (MonadError ServantErr m, MonadReader Config m, MonadIO m) => FilePath -> m ()
validateName name = do
  projects <- liftIO . listDirectory =<< asks configDataDir
  when (name `notElem` projects) $ throwError err404

validateChunk :: (MonadError ServantErr m, Num a, Ord a) => a -> a -> m ()
validateChunk chunk len = do
  when (chunk < 0) $ throwError err400
  when (chunk >= len) $ throwError err404
