module Main where

import Lib
import Config
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  cfg <- readConfigFromEnv
  run (configPort cfg) (app cfg)
