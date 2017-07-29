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
import Text.Pandoc.CrossRef
import Data.Monoid ((<>))
import Text.Pandoc.Builder
import Data.Generics

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
  everywhere (mkT splitMath) <$> (handlePandocError . readMarkdown def
    =<< liftIO (readFile (dataDirectory </> name </> "index.md")))

splitMath :: [Block] -> [Block]
splitMath (Para ils:xs)
  | length ils > 1 = map Para (split [] [] ils) ++ xs
  where
    split res acc [] = reverse (reverse acc : res)
    split res acc (x@(Math DisplayMath _):ys) =
      split ([x] : reverse (dropSpaces acc) : res)
            [] (dropSpaces ys)
    split res acc (y:ys) = split res (y:acc) ys
    dropSpaces = dropWhile (\x -> x == Space || x == SoftBreak)
splitMath xs = xs

render :: FilePath -> ConfigHandler [LT.Text]
render name = do
  validateName name
  uri <- asks configDataUri
  let modImgs (Image a t (src, tit)) = Image a t (uri </> name </> src, tit)
      modImgs x = x
  Pandoc meta body <- getBody name
  let body' = runCrossRef (crossRefSettings <> meta) Nothing crossRefBlocks $ walk modImgs body
  return $ map (H.renderHtml . writeHtml htmlOpts . Pandoc meta . return) body'

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
