{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Config (
    asks
  , ConfigHandler(..)
  , Config(..)
  , readConfigFromEnv
  , convertServer
  ) where

import System.Environment
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except
import Servant
import Servant.Utils.Enter

newtype ConfigHandler a = ConfigHandler {
    runConfigHandler :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader Config,
               MonadError ServantErr, MonadIO)

data Config = Config {
    configDataDir :: FilePath
  , configDataSchema :: String
  , configHost :: String
  , configPort :: Int
  }

convertHandler :: Config -> ConfigHandler :~> Handler
convertHandler cfg = NT (Handler . flip runReaderT cfg . runConfigHandler)

convertServer :: Enter (Entered Handler ConfigHandler t) ConfigHandler Handler t
              => Config -> Entered Handler ConfigHandler t -> t
convertServer cfg = enter (convertHandler cfg)

readConfigFromEnv :: IO Config
readConfigFromEnv = do
  env <- M.fromList <$> getEnvironment
  return Config {
    configDataDir = fromMaybe "data" $ M.lookup "MARKCO_DATA_DIR" env
  , configDataSchema = fromMaybe "http" (read <$> M.lookup "MARKCO_DATA_SCHEMA" env)
  , configHost = fromMaybe "localhost" $ M.lookup "MARKCO_HOST_NAME" env
  , configPort = fromMaybe 8081 (read <$> M.lookup "MARKCO_PORT" env)
  }
