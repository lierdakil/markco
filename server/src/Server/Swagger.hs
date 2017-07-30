module Server.Swagger where

import API
import Config
import Servant.Swagger
import Data.Swagger

swaggerServer :: ConfigHandler Swagger
swaggerServer = return (toSwagger basicApi)
