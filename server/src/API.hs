{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module API where

import Servant
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Swagger (Swagger, ToSchema)
import GHC.Generics
import Data.Aeson

data FileInfo = FileInfo {
    fileName :: T.Text
  , fileURI :: T.Text
  } deriving (Generic)

instance ToJSON FileInfo
instance ToSchema FileInfo

type MainAPI = "projects" :> Get '[JSON] [FilePath]
      :<|> "projects" :> Capture "name" String
           :> ReqBody '[JSON] T.Text :> Post '[JSON] ()
      :<|> "projects" :> Capture "name" String :> Delete '[JSON] ()
      :<|> "projects" :> Capture "name" String :> Get '[JSON] [LT.Text]
      :<|> "projects" :> Capture "name" String
           :> Capture "chunk" Int
           :> ReqBody '[JSON] T.Text :> Patch '[JSON] ()
      :<|> "projects" :> Capture "name" String
           :> ReqBody '[JSON] T.Text :> Patch '[JSON] ()
      :<|> "projects" :> Capture "name" String
           :> Capture "chunk" Int
           :> Get '[JSON] T.Text
      :<|> "projects" :> Capture "name" String
           :> "files" :> Get '[JSON] [FileInfo]
      :<|> "projects" :> Capture "name" String
           :> "files" :> Capture "fileName" String
           :> Delete '[JSON] ()

type API = MainAPI
      :<|> "projects" :> Capture "name" String
           :> "files" :> ReqBody '[OctetStream] B.ByteString :> Post '[JSON]  T.Text
      :<|> "swagger.json" :> Get '[JSON] Swagger

mainApi :: Proxy MainAPI
mainApi = Proxy

api :: Proxy API
api = Proxy
