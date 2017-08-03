{-# LANGUAGE FlexibleContexts #-}

module Server (
  server, authServerContext, AuthMap
) where

import Servant
import API
import Config
import Server.Main
import Server.Login
import Server.Swagger
import Server.Static

server :: AuthMap -> Config -> Server API
server m cfg = convertServer cfg (mainServer :<|> loginServer m)
     :<|> swaggerServer
     :<|> staticServer cfg
