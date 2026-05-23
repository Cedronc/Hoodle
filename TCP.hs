module TCP where

import Network.Simple.TCP (HostName, HostPreference(Host), ServiceName, connect, serve, Socket, accept, send, recv)
import Hoodle
import Pool
import Data.Time (LocalTime)
import LazyHoodle
import Control.Monad (unless, when)

import qualified Data.ByteString.Char8 as B


client = connect "localhost" "4000" $ \(sock, addr) -> do
  putStrLn $ "Connection established to " ++ show addr
  action <- getLine
  send sock (B.pack action)
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end

server :: IO ()
server = serve (Host "127.0.0.1") "4000" $ \(sock, addr) -> do
    putStrLn $ "Connection from: " ++ show addr
    msg <- recv sock 255
    case msg of
        Just bs -> do
            putStrLn $ "Received: " ++ show bs
        Nothing -> putStrLn "Connection closed"
