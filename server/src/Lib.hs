{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Lib where

import Prelude

import Control.Monad.Except
import Network.Wai
import Servant
import System.Directory
import qualified Text.Blaze.Html.Renderer.Text as H
import Text.Pandoc
import System.FilePath
import Servant.Swagger
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Wai.Middleware.Cors
import Network.HTTP.Types
import Network.Wai.Middleware.Servant.Options
import Network.Wai.Middleware.RequestLogger
import Data.Swagger (Swagger)
import qualified Data.Set as S
import Text.Pandoc.Walk
import qualified Data.ByteString as B

type MainAPI = "projects" :> Get '[JSON] [FilePath]
      :<|> "projects" :> Capture "name" String :> "render" :> Get '[JSON] [LT.Text]
      :<|> "projects" :> Capture "name" String
        :> "update" :> Capture "chunk" Int
        :> ReqBody '[JSON] T.Text :> Patch '[JSON] ()
      :<|> "projects" :> Capture "name" String
        :> "source" :> Capture "chunk" Int
        :> Get '[JSON] T.Text

type API = MainAPI
     :<|> "swagger.json" :> Get '[JSON] Swagger
     :<|> "data" :> Capture "name" String :> Capture "file" String :> Get '[OctetStream] B.ByteString

dataDirectory :: String
dataDirectory = "data"

host :: FilePath
host = "http://localhost:8081"
-- host = "/home/livid/github/lierdakil/markco"

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

getBody :: MonadIO m => FilePath -> m Pandoc
getBody name = either (error . show) id . readMarkdown def
    <$> liftIO (readFile (dataDirectory </> name </> "index.md"))

render :: FilePath -> Handler [LT.Text]
render name = liftIO $ do
  Pandoc meta body <- getBody name
  let body' = walk modImgs body
  return $ map (H.renderHtml . writeHtml htmlOpts . Pandoc meta . return) body'
  where
    modImgs (Image a t (src, tit)) = Image a t (host </> "data" </> name </> src, tit)
    modImgs x = x

update :: FilePath -> Int -> T.Text -> Handler ()
update name chunk mdbody = do
  Pandoc meta body <- getBody name
  let (b1, _:b2) = splitAt chunk body
      body' = b1 ++ newChunkBody ++ b2
      Pandoc _ newChunkBody = either (error . show) id $ readMarkdown def $ T.unpack mdbody
  liftIO
    $ writeFile (dataDirectory </> name </> "index.md")
    $ writeMarkdown mdOpts $ Pandoc meta body'

getSource :: FilePath -> Int -> Handler T.Text
getSource name chunk = do
  Pandoc meta body <- getBody name
  let c = [body !! chunk]
  return $ T.pack $ writeMarkdown mdOpts (Pandoc meta c)

mainServer :: Server MainAPI
mainServer = liftIO (listDirectory dataDirectory)
    :<|> render
    :<|> update
    :<|> getSource

server1 :: Server API
server1 = mainServer
     :<|> return (toSwagger mainApi)
     :<|> (\name file -> liftIO $ B.readFile (dataDirectory </> name </> file))

mainApi :: Proxy MainAPI
mainApi = Proxy

api :: Proxy API
api = Proxy

app1 :: Application
app1 = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions mainApi
    $ serve api server1
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ]
           , corsMethods = map renderStdMethod [GET, PUT, POST, PATCH]}
