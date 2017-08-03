{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Access files on the filesystem.
module Server.Static
    ( staticServer
    ) where

import System.FilePath ((</>))
import System.IO (withBinaryFile, IOMode(..))
-- import Util
import Data.ByteString (ByteString)
import Control.Exception (SomeException, try)
import qualified Network.Wai as W
import System.PosixCompat.Files (fileSize, getFileStatus, modificationTime, isRegularFile)
import Data.ByteArray.Encoding
import Crypto.Hash (hashlazy, MD5, Digest)
import qualified Data.ByteString.Lazy as BL
import Network.Wai.Application.Static
import Network.URI
import Data.List
import WaiAppStatic.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Config
import Servant
import WaiAppStatic.Storage.Filesystem
import Data.ByteString.Builder
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import System.PosixCompat.Types


staticServer :: Config -> Server Raw
staticServer Config{..} = serveDirectoryWith settings
  where
  settings = (defaultWebAppSettings undefined) {
      ssLookupFile = lookupFile
    , ssIndices = [indexPiece]
    }
  serveData = isRelativeReference configDataUri
  indexPiece = unsafeToPiece "index.html"
  dataPieces
    | serveData
    = maybe (error "couln't parse MARKCO_DATA_URI as URI")
            (map (unsafeToPiece . T.pack) . pathSegments)
    $ parseURIReference configDataUri
    | otherwise = undefined
  lookupFile [] = staticDirLookup []
  lookupFile xs
    | serveData
    , Just pieces <- stripPrefix dataPieces xs
    = dataDirLookup pieces
    | not $ null xs
    , last xs == indexPiece = getIndexFile configStaticDir configNewCmdFile indexPiece
    | otherwise = staticDirLookup xs
  dataDirLookup = webAppLookup configDataDir
  staticDirLookup = webAppLookup configStaticDir

getIndexFile :: FilePath -> Maybe FilePath -> Piece -> IO LookupResult
getIndexFile dir Nothing idx = webAppLookup dir [idx]
getIndexFile dir (Just ncfile) idx = fmap (maybe LRNotFound LRFile) . runMaybeT $ do
  let indexfile = dir </> T.unpack (fromPiece idx)
  content <- maybeReadFile indexfile
  modcon <- lift . runMaybeT $ getModifiedContent content
  let bytes = TE.encodeUtf8 $ fromMaybe content modcon
  [ifs, ins] <- lift $ mapM (runMaybeT . getModTime) [indexfile, ncfile]
  return File{
    fileGetSize = toInteger $ BL.length bytes
  , fileName = unsafeToPiece "index.html"
  , fileGetHash =
      let hash = hashlazy bytes :: Digest MD5
      in return . Just $ convertToBase Base64 hash
  , fileGetModified = max ifs ins
  , fileToResponse = \s h -> W.responseBuilder s h (lazyByteString bytes)
  }
  where
    getModifiedContent :: TL.Text -> MaybeT IO TL.Text
    getModifiedContent content = do
      newcmds <- maybeReadFile ncfile
      return $ TL.replace "{{newcommands}}" newcmds content

-- | Construct a new path from a root and some @Pieces@.
pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces = foldl' (\fp p -> fp </> T.unpack (fromPiece p))

-- | Convenience wrapper for @fileHelper@.
fileHelperLR :: ETagLookup
             -> FilePath -- ^ file location
             -> Piece -- ^ file name
             -> IO LookupResult
fileHelperLR a b c = maybe LRNotFound LRFile <$> fileHelper a b c

-- | Attempt to load up a @File@ from the given path.
fileHelper :: ETagLookup
           -> FilePath -- ^ file location
           -> Piece -- ^ file name
           -> IO (Maybe File)
fileHelper hashFunc fp name = do
    efs <- try $ getFileStatus fp
    case efs of
        Left (_ :: SomeException) -> return Nothing
        Right fs | isRegularFile fs -> return $ Just File
            { fileGetSize = fromIntegral $ fileSize fs
            , fileToResponse = \s h -> W.responseFile s h fp Nothing
            , fileName = name
            , fileGetHash = hashFunc fp
            , fileGetModified = Just $ modificationTime fs
            }
        Right _ -> return Nothing

-- | More efficient than @fileSystemLookup@ as it only concerns itself with
-- finding files, not folders.
webAppLookup :: FilePath -> Pieces -> IO LookupResult
webAppLookup prefix pieces =
    fileHelperLR hashFileIfExists fp lastPiece
  where
    fp = pathFromPieces prefix pieces
    lastPiece
        | null pieces = unsafeToPiece ""
        | otherwise = last pieces

-- | MD5 hash and base64-encode the file contents. Does not check if the file
-- exists.
hashFile :: FilePath -> IO ByteString
hashFile fp = withBinaryFile fp ReadMode $ \h -> do
    f <- BL.hGetContents h
    let !hash = hashlazy f :: Digest MD5
    return $ convertToBase Base64 hash

hashFileIfExists :: ETagLookup
hashFileIfExists fp = runMaybeT $ tryMaybe $ hashFile fp

tryMaybe :: IO a -> MaybeT IO a
tryMaybe = MaybeT . fmap (either (\(_::SomeException) -> Nothing) Just) . try

maybeReadFile :: FilePath -> MaybeT IO TL.Text
maybeReadFile = fmap TE.decodeUtf8 . tryMaybe . BL.readFile

getModTime :: FilePath -> MaybeT IO EpochTime
getModTime = fmap modificationTime . tryMaybe . getFileStatus
