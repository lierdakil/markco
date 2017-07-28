{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Swagger (Swagger)
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
      :<|> StaticAPI
      :<|> "swagger.json" :> Get '[JSON] Swagger

type StaticAPI =
      "data" :> Capture "name" String :> Capture "file" String :> Get '[OctetStream] B.ByteString

mainApi :: Proxy MainAPI
mainApi = Proxy

api :: Proxy API
api = Proxy
