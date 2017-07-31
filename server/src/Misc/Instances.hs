{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Misc.Instances () where

import Servant
import Servant.Foreign

import Control.Lens ((&), (.~), (%~))
import Data.Text.Encoding
import Data.Text
import Servant.Foreign.Internal

instance {-# OVERLAPS #-} (HasForeignType lang ftype a, HasForeign lang ftype api)
      => HasForeign lang ftype (ReqBody '[OctetStream] a :> api) where
  type Foreign ftype (ReqBody '[OctetStream] a :> api) = Foreign ftype api
  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) $
      req & reqBody .~ (Just $ typeFor lang ftype (Proxy :: Proxy a))


instance {-# OVERLAPS #-} (HasForeignType lang ftype a, ReflectMethod method)
  => HasForeign lang ftype (Verb method status '[OctetStream] a) where
  type Foreign ftype (Verb method status '[OctetStream] a) = Req ftype
  foreignFor lang Proxy Proxy req =
    req & reqFuncName . _FunctionName %~ (methodLC :)
        & reqMethod .~ method
        & reqReturnType .~ Just retType
    where
      retType  = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a)
      method   = reflectMethod (Proxy :: Proxy method)
      methodLC = toLower $ decodeUtf8 method
