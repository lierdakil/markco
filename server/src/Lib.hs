{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prelude

import Network.Wai
import Servant
import Network.Wai.Middleware.Cors
import Network.HTTP.Types
import Network.Wai.Middleware.Servant.Options
import Network.Wai.Middleware.RequestLogger
import qualified Data.ByteString.Char8 as B
import API
import Server
import Config
import Misc.Instances ()

app :: AuthMap -> Config -> Application
app m cfg = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions basicApi
    $ serveWithContext api (authServerContext m cfg) (server m cfg)
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ]
           , corsMethods = map renderStdMethod [GET, PUT, POST, PATCH, DELETE]
           , corsOrigins = flip (,) True . map B.pack <$> configOrigins cfg}
