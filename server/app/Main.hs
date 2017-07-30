module Main where

import Lib
import Config
import Network.Wai.Handler.Warp
import qualified STMContainers.Map as M
import GHC.Conc

main :: IO ()
main = do
  cfg <- readConfigFromEnv
  m <- atomically M.new
  run (configPort cfg) (app m cfg)
