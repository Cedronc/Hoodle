module TCP where

import Control.Concurrent.STM
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Char8 qualified as B
import Data.Time (LocalTime)
import Hoodle
import LazyHoodle
import Network.Simple.TCP (HostName, HostPreference (Host), ServiceName, Socket, accept, connect, recv, send, serve)
import Parser
import Pool

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
server = do
  dbSTM <- newTVarIO emptyDB
  -- Time is from Pasers module
  pool <- newTVarIO (emptyPool :: MapPool Int (LazyHoodle Time))
  atomically $ modifyTVar dbSTM (addUser "cedric" "123")

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
        Left err -> send sock (B.pack "error")
      Nothing -> putStrLn "Connection closed"

processRequest :: Request -> AppMonad Response
processRequest (ChangePassword login newPass) = changePassword login newPass
processRequest (AddHoodle login token hoodle) = do
  (dbVar, poolVar) <- ask
  db <- lift . lift $ readTVar dbVar
  if validLogin login db
    then do
      -- Modify pool (atomic)
      -- lift . lift $ modifyTVar (addToPool poolVar hoodle)
      lift . lift $ modifyTVar poolVar (snd . addToPool hoodle)
      return (OkToken "hoodle-added")
    else return WrongLogin
processRequest req = return (OkToken "not-implemented")

changePassword :: Login -> Token -> AppMonad Response
changePassword login newPass = do
  (tvarDB, _) <- ask
  -- first lifted to (ExceptT Response STM) then lifted to (ReaderT AppState)
  db <- lift . lift $ readTVar tvarDB
  if validLogin login db
    then lift . lift $ modifyTVar tvarDB (addUser (fst login) newPass) >> return (OkToken "changed")
    else throwError WrongLogin
