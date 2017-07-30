{-# LANGUAGE FlexibleContexts #-}

module Server where

import Servant
import API
import Config
import Server.Main
import Server.Swagger

server :: Config -> Server API
server cfg = convertServer cfg $
          mainServer
     :<|> swaggerServer
