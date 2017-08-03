module Server.Swagger where

import API
import Servant.Swagger
import Data.Swagger
import Servant

swaggerServer :: Handler Swagger
swaggerServer = return (toSwagger basicApi)
