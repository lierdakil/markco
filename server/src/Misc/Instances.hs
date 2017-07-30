{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Misc.Instances () where

import Servant
import Servant.Foreign

import Control.Lens ((&), (.~))

instance {-# OVERLAPS #-} (HasForeignType lang ftype a, HasForeign lang ftype api)
      => HasForeign lang ftype (ReqBody '[OctetStream] a :> api) where
  type Foreign ftype (ReqBody '[OctetStream] a :> api) = Foreign ftype api
  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) $
      req & reqBody .~ (Just $ typeFor lang ftype (Proxy :: Proxy a))
