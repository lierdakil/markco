{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Swagger (Swagger)

type MainAPI = "projects" :> Get '[JSON] [FilePath]
      :<|> "projects" :> Capture "name" String :> "render" :> Get '[JSON] [LT.Text]
      :<|> "projects" :> Capture "name" String
           :> "update" :> Capture "chunk" Int
           :> ReqBody '[JSON] T.Text :> Patch '[JSON] ()
      :<|> "projects" :> Capture "name" String
           :> "source" :> Capture "chunk" Int
           :> Get '[JSON] T.Text

type API = MainAPI
      :<|> "projects" :> Capture "name" String
           :> "upload" :> ReqBody '[OctetStream] B.ByteString :> Post '[JSON]  T.Text
      :<|> "swagger.json" :> Get '[JSON] Swagger

mainApi :: Proxy MainAPI
mainApi = Proxy

api :: Proxy API
api = Proxy
