module TCP where

import Network.Simple.TCP (HostName, ServiceName, send, recv, serve)
import Network.Simple.TCP (HostPreference (Host))
import Control.Concurrent (forkIO)

import Hoodle
import Pool
import Data.Time (LocalTime)
import Data.Maybe (fromJust)


run :: IO()
run = serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
  user <- recv connectionSocket 10
  send connectionSocket (fromJust user)
  putStrLn (fromJust user)
  putStrLn $ "TCP connection established from " ++ show remoteAddr
