module Server.Static where

import Servant
import System.FilePath
import qualified Data.ByteString as B
import Config
import Utils
import API
import Control.Monad.IO.Class

getBinaryFile :: FilePath -> FilePath -> ConfigHandler B.ByteString
getBinaryFile name file = do
  validateName name
  dataDir <- asks configDataDir
  liftIO $ B.readFile (dataDir </> name </> file)

staticServer :: ServerT StaticAPI ConfigHandler
staticServer = getBinaryFile
