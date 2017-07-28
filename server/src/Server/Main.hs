{-# LANGUAGE FlexibleContexts #-}

module Server.Main where

import System.Directory
import qualified Text.Blaze.Html.Renderer.Text as H
import Text.Pandoc
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Set as S
import Text.Pandoc.Walk
import Control.Monad.Reader
import Config
import Utils
import API
import Servant
import Control.Monad.Error.Class

mainServer :: ServerT MainAPI ConfigHandler
mainServer = listProjects
        :<|> render
        :<|> update
        :<|> getSource

mdOpts :: WriterOptions
mdOpts = def {
    writerSetextHeaders = False
  , writerExtensions = S.delete Ext_simple_tables pandocExtensions
  }

htmlOpts :: WriterOptions
htmlOpts = def{
    writerHtml5 = True
  , writerHTMLMathMethod = MathJax ""
  }

handlePandocError :: (MonadError ServantErr m) => Either PandocError Pandoc -> m Pandoc
handlePandocError (Left err) = throwError err500{ errBody = LT.encodeUtf8 $ LT.pack $ show err }
handlePandocError (Right res) = return res

getBody :: (MonadIO m, MonadError ServantErr m, MonadReader Config m) => FilePath -> m Pandoc
getBody name = do
  dataDirectory <- asks configDataDir
  handlePandocError . readMarkdown def
    =<< liftIO (readFile (dataDirectory </> name </> "index.md"))

render :: FilePath -> ConfigHandler [LT.Text]
render name = do
  validateName name
  host <- asks configHost
  port <- asks configPort
  schema <- asks configDataSchema
  let modImgs (Image a t (src, tit)) = Image a t (uri </> "data" </> name </> src, tit)
      modImgs x = x
      uri = schema ++ "://" ++ host ++ ":" ++ show port
  Pandoc meta body <- getBody name
  let body' = walk modImgs body
  return $ map (H.renderHtml . writeHtml htmlOpts . Pandoc meta . return) body'

update :: FilePath -> Int -> T.Text -> ConfigHandler ()
update name chunk mdbody = do
  validateName name
  dataDirectory <- asks configDataDir
  Pandoc meta body <- getBody name
  validateChunk chunk (length body)
  Pandoc _ newChunkBody <- handlePandocError $ readMarkdown def $ T.unpack mdbody
  let (b1, _:b2) = splitAt chunk body
      body' = b1 ++ newChunkBody ++ b2
  liftIO
    $ writeFile (dataDirectory </> name </> "index.md")
    $ writeMarkdown mdOpts $ Pandoc meta body'

getSource :: FilePath -> Int -> ConfigHandler T.Text
getSource name chunk = do
  validateName name
  Pandoc meta body <- getBody name
  validateChunk chunk (length body)
  let c = [body !! chunk]
  return $ T.pack $ writeMarkdown mdOpts (Pandoc meta c)

listProjects :: ConfigHandler [FilePath]
listProjects = do
  dataDirectory <- asks configDataDir
  liftIO $ listDirectory dataDirectory
