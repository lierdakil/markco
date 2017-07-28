{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prelude

import Network.Wai
import Servant
import Network.Wai.Middleware.Cors
import Network.HTTP.Types
import Network.Wai.Middleware.Servant.Options
import Network.Wai.Middleware.RequestLogger
import API
import Server
import Config

app :: Config -> Application
app cfg = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions mainApi
    $ serve api (server cfg)
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ]
           , corsMethods = map renderStdMethod [GET, PUT, POST, PATCH]}
