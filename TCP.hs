module TCP where

import Control.Monad (unless, when)
import Data.ByteString.Char8 qualified as B
import Data.Time (LocalTime)
import Hoodle
import LazyHoodle
import Network.Simple.TCP (HostName, HostPreference (Host), ServiceName, Socket, accept, connect, recv, send, serve)
import Parser
import Pool

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
    Just bs -> case parseProtocolMessage (B.unpack bs) of
      Right request -> print $ generateResponse request
      Left err -> print err
    Nothing -> putStrLn "Connection closed"

generateResponse :: Request -> Response
generateResponse (ChangePassword _ _) = OkToken "password-updated"
generateResponse (AddUser _ _) = OkToken "user-added"
generateResponse (GetHoodle _ ) = OkHoodle [] -- Empty hoodle (replace with real data)
generateResponse (AddHoodle _ _ _) = OkToken "hoodle-added"
generateResponse (EditHoodle _ _ _) = OkToken "hoodle-edited"
generateResponse (RemoveHoodle _ _) = OkToken "hoodle-removed"
generateResponse (Register _ _) = OkToken "registration-token"
generateResponse (Unregister _ _) = OkToken "unregistered"
generateResponse (Toggle _ _ _) = OkToken "slot-toggled"
generateResponse (OptimalSchedule _) = OkSchedule [] -- Empty schedule (replace with real data)
