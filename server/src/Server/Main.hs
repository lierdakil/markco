{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Main where

import System.Directory
import qualified Text.Blaze.Html.Renderer.Text as H
import Text.Pandoc
import System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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
import Text.Pandoc.CrossRef
import Data.Monoid ((<>))
import Text.Pandoc.Builder
import Data.Generics
import Crypto.Hash (hash, Digest, SHA1)
import Data.Char (isAlphaNum)

mainServer :: ServerT MainAPI ConfigHandler
mainServer (_ :: User)
     = listProjects
  :<|> createProject
  :<|> deleteProject
  :<|> render
  :<|> update
  :<|> appendChunk
  :<|> getSource
  :<|> fileList
  :<|> deleteFile
  :<|> uploadFile
  :<|> renderDocx

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
  validateName name
  dataDirectory <- asks configDataDir
  everywhere (mkT splitMath) <$> (handlePandocError . readMarkdown def
    =<< liftIO (readFile (dataDirectory </> name </> "index.md")))

splitMath :: [Block] -> [Block]
splitMath (Para ils:xs)
  | length ils > 1 = map Para (split ils) ++ xs
  where
    split ys =
      let bef = takeWhile (not . isMath) ys
          rest = drop (length bef) ys
          m = takeWhile (not . isSpace) rest
          af = drop (length m) rest
      in filter (not . null) [bef, m, af]
    isMath (Math DisplayMath _) = True
    isMath (Span _ [Math DisplayMath _]) = True
    isMath _ = False
    isSpace Space = True
    isSpace SoftBreak = True
    isSpace _ = False
splitMath xs = xs

createProject :: FilePath -> T.Text -> ConfigHandler ()
createProject name content = do
  projects <- liftIO . listDirectory =<< asks configDataDir
  when (name `elem` projects) $ throwError err409
  unless (all isAlphaNum name) $ throwError err400
  dataDir <- asks configDataDir
  liftIO $ do
    createDirectory (dataDir </> name)
    BL.writeFile (dataDir </> name </> "index.md") $ LT.encodeUtf8 $ LT.fromStrict content

deleteProject :: FilePath -> ConfigHandler ()
deleteProject name = do
  validateName name
  dataDir <- asks configDataDir
  liftIO $ removeDirectoryRecursive (dataDir </> name)

render :: FilePath -> ConfigHandler [Chunk]
render name = do
  Pandoc meta body <- getBody name
  uri <- asks configDataUri
  let modImgs (Image a t (src, tit)) = Image a t (uri </> name </> src, tit)
      modImgs x = x
  let body' = runCrossRef (crossRefSettings <> meta) Nothing crossRefBlocks . wrapDiv $ walk modImgs body
  return $ zipWith3 (mkChunk meta) body body' [0..]
  where
    wrapDiv = map (Div nullAttr . return)
    mkChunk meta blold bl idx = Chunk {
          chunkHtml = H.renderHtml $ writeHtml htmlOpts $ Pandoc meta [bl]
        , chunkSrc = T.pack $ writeMarkdown mdOpts $ Pandoc meta [blold]
        , chunkNum = idx
        }

renderDocx :: FilePath -> ConfigHandler FileData
renderDocx name = do
  validateName name
  dataDirectory <- asks configDataDir
  Pandoc meta body <- handlePandocError . readMarkdown def . (newcommands <>)
    =<< liftIO (readFile (dataDirectory </> name </> "index.md"))
  let modImgs (Image a t (src, tit)) = Image a t (dataDirectory </> name </> src, tit)
      modImgs x = x
  let body' = runCrossRef (crossRefSettings <> meta) Nothing crossRefBlocks $ walk modImgs body
  FileData <$> liftIO (writeDocx def (Pandoc meta body'))

newcommands :: String
newcommands =
     "\
     \\\newcommand{\\d}{\\mathrm{d}}\n\
     \\\newcommand{\\pdt}[1]{\\frac{\\partial #1}{\\partial t}}\n\
     \\\newcommand{\\pd}[2]{\\frac{\\partial #1}{\\partial #2}}\n\
     \\\newcommand{\\fdt}[1]{\\frac{\\d #1}{\\d t}}\n\
     \\\newcommand{\\fd}[2]{\\frac{\\d #1}{\\d #2}}\n\
     \\\newcommand{\\arctg}{\\mathrm{arctg}}\n\
     \\\newcommand{\\tg}{\\mathrm{tg}}\n\
     \\\newcommand{\\vect}[1]{\\boldsymbol{\\mathbf{#1}}}\n\
     \\\newcommand{\\unit}[1]{\\;\\text{#1}}\n\
     \\\newcommand{\\lr}[3]{\\left#1{#3}\\right#2}\n\
     \\\newcommand{\\paren}[1]{\\lr(){#1}}\n\
     \\\newcommand{\\brace}[1]{\\lr\\lbrace\\rbrace{#1}}\n\
     \\\newcommand{\\brack}[1]{\\lr\\lbrack\\rbrack{#1}}\n\
     \\\newcommand{\\angle}[1]{\\lr\\langle\\rangle{#1}}\n\
     \\\newcommand{\\abs}[1]{\\lr||{#1}}\n\
     \\\newcommand{\\rline}[1]{\\lr.|{#1}}\n\
     \\\newcommand{\\deg}{{^\\circ}}\n\
     \\\renewcommand{\\epsilon}{\\varepsilon}\n\
     \\\newcommand{\\max}{\\mathrm{max}}\n"

crossRefSettings :: Meta
crossRefSettings =
     chapters True
  <> numberSections True
  <> sectionsDepth "3"
  <> chaptersDepth "1"
  <> figureTitle (str "Рисунок")
  <> tableTitle (str "Таблица")
  <> listingTitle (str "Листинг")
  <> figPrefix [str "рис."]
  <> eqnPrefixTemplate (str "(" <> var "i" <> str ")")
  <> tblPrefix [str "табл."]
  <> lstPrefix [str "лист."]
  <> secPrefix [str "разд."]
  <> lofTitle (header 1 $ text "Список рисунков")
  <> lotTitle (header 1 $ text "Список таблиц")
  <> lolTitle (header 1 $ text "Список листингов")
  -- <> autoEqnLabels True
  <> subfigGrid True
  <> linkReferences True
  where var = displayMath

update :: FilePath -> Int -> T.Text -> ConfigHandler ()
update name chunk mdbody = do
  Pandoc meta body <- getBody name
  dataDirectory <- asks configDataDir
  validateChunk chunk (length body)
  Pandoc _ newChunkBody <- handlePandocError $ readMarkdown def $ T.unpack mdbody
  let (b1, _:b2) = splitAt chunk body
      body' = b1 ++ newChunkBody ++ b2
  liftIO
    $ writeFile (dataDirectory </> name </> "index.md")
    $ writeMarkdown mdOpts $ Pandoc meta body'

appendChunk :: FilePath -> T.Text -> ConfigHandler ()
appendChunk name mdbody = do
  Pandoc meta body <- getBody name
  dataDirectory <- asks configDataDir
  Pandoc _ newChunkBody <- handlePandocError $ readMarkdown def $ T.unpack mdbody
  let body' = body ++ newChunkBody
  liftIO
    $ writeFile (dataDirectory </> name </> "index.md")
    $ writeMarkdown mdOpts $ Pandoc meta body'

getSource :: FilePath -> Int -> ConfigHandler T.Text
getSource name chunk = do
  Pandoc meta body <- getBody name
  validateChunk chunk (length body)
  let c = [body !! chunk]
  return $ T.pack $ writeMarkdown mdOpts (Pandoc meta c)

listProjects :: ConfigHandler [FilePath]
listProjects = do
  dataDirectory <- asks configDataDir
  liftIO $ listDirectory dataDirectory

uploadFile :: FilePath -> FileData -> ConfigHandler T.Text
uploadFile name (FileData content) = do
  validateName name
  dataDirectory <- asks configDataDir
  let content' = BL.toStrict content
      filename = show (hash content' :: Digest SHA1)
  liftIO $ B.writeFile (dataDirectory </> name </> filename) content'
  return $ T.pack filename

fileList :: FilePath -> ConfigHandler [FileInfo]
fileList name = do
  validateName name
  dataDirectory <- asks configDataDir
  uriBase <- asks configDataUri
  map (mkFI uriBase) . filter (/= "index.md") <$> liftIO (listDirectory (dataDirectory </> name))
  where
    mkFI uriBase fn = FileInfo {
        fileName = T.pack fn
      , fileURI = T.pack $ uriBase </> name </> fn
      }

deleteFile :: FilePath -> FilePath -> ConfigHandler ()
deleteFile proj fn = do
  validateFile proj fn
  dataDirectory <- asks configDataDir
  liftIO $ removeFile (dataDirectory </> proj </> fn)
