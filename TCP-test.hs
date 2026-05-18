module TCP where

import Network.Simple.TCP (HostName, ServiceName, connect)

run :: IO ()
run = connect "jellyfin.boel-mongool-9.dev" "443" $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr

