module TCP where

import Control.Monad (unless, when)
import Data.ByteString.Char8 qualified as B
import Data.Time (LocalTime)
import Pool
import Hoodle
import LazyHoodle
import Network.Simple.TCP (HostName, HostPreference (Host), ServiceName, Socket, accept, connect, recv, send, serve)
import Parser
import Pool
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Monad.Except

client = connect "localhost" "4000" $ \(sock, addr) -> do
  putStrLn $ "Connection established to " ++ show addr
  action <- getLine
  send sock (B.pack action)

-- Now you may use connectionSocket as you please within this scope,
-- possibly using recv and send to interact with the remote end

server :: IO ()
server = do
  dbSTM <- newTVarIO emptyDB
  pool <- newTVarIO (emptyPool :: MapPool Int (LazyHoodle LocalTime))

  let appState = (dbSTM, pool)

  serve (Host "127.0.0.1") "4000" $ \(sock, addr) -> do
    putStrLn $ "Connection from: " ++ show addr
    msg <- recv sock 255

    case msg of
          Just bs -> case parseProtocolMessage (B.unpack bs) of
            Right request -> do
              -- Run with BOTH poolVar and dbVar
              result <- runAppAtomically (processRequest request) appState
              case result of
                Right response -> send sock (B.pack (show response))
                Left err -> send sock (B.pack (show err))
            Left err -> send sock (B.pack (show err))
          Nothing -> putStrLn "Connection closed"

processRequest :: Request -> AppMonad Response
processRequest (ChangePassword login newPass) = changePassword login newPass
processRequest (AddHoodle login token hoodle) = do
  (poolVar, dbVar) <- ask
  db <- lift . lift $ readTVar dbVar
  if validLogin login db
    then do
      -- Modify pool (atomic)
      lift . lift $ modifyTVar poolVar (`addToPool` hoodle)
      return (OkToken "hoodle-added")
    else return WrongLogin


processRequest req = return (OkToken "not-implemented")
