module TCP where

import Network.Simple.TCP (HostName, HostPreference(Host), ServiceName, connect, serve, Socket, accept, send, recv)
import Hoodle
import Pool
import Data.Time (LocalTime)
import LazyHoodle
import Control.Monad (unless, when, forever)

import qualified Data.ByteString.Char8 as B

-- Move connect to connectSock to manually close connections
client = connect "localhost" "4000" $ \(sock, addr) -> do
  putStrLn $ "Connection established to " ++ show addr
  putStr "<== "
  action <- getLine
  send sock (B.pack action)
  msg <- recv sock 255
  case msg of
      Just val -> do
          putStrLn $ "==> " ++ show val
      Nothing -> putStrLn "Connection closed"


server :: IO ()
server = serve (Host "127.0.0.1") "4000" $ \(sock, addr) -> do
    putStrLn $ "Connection from: " ++ show addr
    msg <- recv sock 255
    case msg of
        Just val -> do
            putStrLn $ "Received: " ++ show val
            send sock val
        Nothing -> putStrLn "Connection closed"
