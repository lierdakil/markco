{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Login where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Monad.Reader
import Config
import API
import Servant
import Data.Monoid ((<>))
import Network.Wai
import Servant.Server.Experimental.Auth
import qualified STMContainers.Map as M
import Crypto.Random
import GHC.Conc

type AuthMap = M.Map B.ByteString User

loginServer :: AuthMap -> ServerT LoginAPI ConfigHandler
loginServer = login

authCheck :: AuthMap -> Config -> AuthHandler Request User
authCheck m Config{..}
  | Just _ <- configUserFile =
  let check req = do
        let auth = B64.decodeLenient <$> lookup "x-markco-authentication" (requestHeaders req)
        case auth of
          Nothing -> throwError err401
          Just hsh -> do
            user <- liftIO $ atomically $ M.lookup hsh m
            maybe (throwError err403) return user
  in mkAuthHandler check
  | otherwise = mkAuthHandler (const . return $ User "guest")

authServerContext :: AuthMap -> Config -> Context (AuthHandler Request User ': '[])
authServerContext m cfg = authCheck m cfg :. EmptyContext

login :: AuthMap -> AuthData -> ConfigHandler T.Text
login m AuthData{..} = do
  confMaybe <- asks configUserFile
  case confMaybe of
    Just conf -> do
      let hsh = authLogin <> ":" <> authHashedPassword
      uf <- liftIO $ LT.lines . LT.decodeUtf8 <$> BL.readFile conf
      if LT.fromStrict hsh `elem` uf
      then do
        (bytes :: B.ByteString) <- liftIO $ getRandomBytes 64
        liftIO . atomically $ M.insert (User authLogin) bytes m
        return $ LT.toStrict $ LT.decodeUtf8 $ BL.fromStrict $ B64.encode bytes
      else throwError err403
    Nothing -> throwError err400
