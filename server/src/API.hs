{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module API where

import Servant
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Swagger (Swagger, ToSchema(..))
import qualified Data.Swagger as S
import GHC.Generics
import Data.Aeson
import Servant.Server.Experimental.Auth

data FileInfo = FileInfo {
    fileName :: T.Text
  , fileURI :: T.Text
  } deriving (Generic)

instance ToJSON FileInfo
instance ToSchema FileInfo

newtype User = User { userUsername :: T.Text }
type instance AuthServerData (AuthProtect "markco") = User

data AuthData = AuthData {
    authLogin :: T.Text
  , authHashedPassword :: T.Text
  } deriving (Generic)

instance FromJSON AuthData

newtype FileData = FileData { unFileData :: B.ByteString }
    deriving (
      MimeRender OctetStream
    , MimeUnrender OctetStream
    )

instance ToSchema FileData where
  declareNamedSchema _ = return $ S.NamedSchema Nothing S.binarySchema

type BasicAPI =
           "projects" :> Get '[JSON] [FilePath]
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
      :<|> "projects" :> Capture "name" String
           :> "files" :> ReqBody '[OctetStream] FileData
           :> Post '[JSON]  T.Text

type MainAPI = AuthProtect "markco" :> BasicAPI

type LoginAPI = "login" :> ReqBody '[JSON] AuthData :> Post '[JSON] T.Text

type API = MainAPI
      :<|> LoginAPI
      :<|> "swagger.json" :> Get '[JSON] Swagger

basicApi :: Proxy BasicAPI
basicApi = Proxy

mainApi :: Proxy MainAPI
mainApi = Proxy

api :: Proxy API
api = Proxy
