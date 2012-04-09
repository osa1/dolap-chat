{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (fromException)
import Control.Monad (forM_)
import Control.Concurrent hiding (newChan, Chan)
import Control.Monad.IO.Class (liftIO)
import qualified IO as IO

import qualified Data.Text as T
import qualified Data.Map as Map

import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

import qualified Text.Parsec as P

import Channel
import Client
import Parser
import qualified Msg as Msg

type ServerS  = Map.Map T.Text (WS.Sink WS.Hybi10)

tryLogin :: ServerS -> T.Text -> WS.Sink WS.Hybi10 -> Maybe ServerS
tryLogin server nick sock =
  case Map.lookup nick server of
    Just _  -> Nothing
    Nothing -> Just $ Map.insert nick sock server

removeClient :: ServerS -> Client -> ServerS
removeClient server (Client nick _) = Map.delete nick server

main :: IO ()
main = do
  serverS  <- newMVar newServerS
  chanS    <- newMVar newChanS
  Warp.runSettings Warp.defaultSettings
    { Warp.settingsPort = 3000
    --, Warp.settingsHost = "*"
    , Warp.settingsIntercept = WaiWS.intercept (application serverS chanS)
    } (Static.staticApp Static.defaultWebAppSettings)

--login :: MVar ServerState -> WS.Sink WS.Hybi10 -> WS.WebSockets WS.Hybi10 (Text)
login :: MVar ServerS -> WS.Sink WS.Hybi10 -> WS.WebSockets WS.Hybi10 (T.Text)
login serverS sink = do
  msg <- WS.receiveData :: WS.WebSockets WS.Hybi10 (T.Text)
  server <- liftIO $ takeMVar serverS
  case P.parse cmd "" msg of
    (Right (LoginCmd nick)) -> case tryLogin server (T.pack nick) sink of
                         Nothing -> do
                           liftIO $ putMVar serverS server
                           liftIO $ WS.sendSink sink $ WS.textData Msg.nickInUse
                           login serverS sink
                         Just server' -> do
                           liftIO $ putMVar serverS server'
                           return (T.pack nick)
    _ -> do liftIO $ putMVar serverS server
            liftIO $ WS.sendSink sink $ WS.textData Msg.loginFirst
            login serverS sink

application :: MVar ServerS -> MVar ChanS -> WS.Request -> WS.WebSockets WS.Hybi10 ()
--application = undefined
application serverS chanS rq = do
  WS.acceptRequest rq
  sink <- WS.getSink
  liftIO $ putStrLn "incoming connection"

  -- TODO: exception handling in login and ping threads
  nick <- login serverS sink
  let client = Client nick sink

  flip WS.catchWsError catchPingTErr $ WS.spawnPingThread 5
  talk client serverS chanS

catchPingTErr e = do
  liftIO $ putStrLn "error on ping thread"

talk :: Client -> MVar ServerS -> MVar ChanS -> WS.WebSockets WS.Hybi10 ()
talk client@(Client nick sink) serverS chanS = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ case (P.parse cmd "" msg) of
               Left  err -> WS.sendSink sink $ WS.textData (Msg.unknownCommand (show err))
               Right cmd -> runCmd client cmd chanS
    talk client serverS chanS
      where catchDisconnect e = case fromException e of
              Just WS.ConnectionClosed -> do
                -- remove user from channels and send "leave" msgs
                liftIO $ modifyMVar_ chanS $ \s -> do -- s :: Map.Map T.Text Chan
                  forM_ (Map.toList s) $ \(cn, (Chan clients)) ->
                    case Map.lookup nick clients of
                      Nothing -> return ()
                      Just _  ->
                        sendClientsMap clients (Msg.leftChannelCmd nick cn)

                  let s' = Map.map (flip removeFromChan client) s
                  return s'

                -- remove user from server
                liftIO $ modifyMVar_ serverS $ \s -> do
                  let s' = removeClient s client
                  return s'

              _ -> return ()


runCmd :: Client -> Cmd -> MVar ChanS -> IO ()
runCmd cl@(Client nick sink) cmd chanS = do
  case cmd of

    (JoinCmd cn) -> do
      --putStrLn "joincmd"
      let chanName = T.pack cn
      liftIO $ modifyMVar_ chanS $ \s -> do
        case Map.lookup chanName s of
          Nothing -> do sendClient cl (Msg.usersCmd chanName [nick])
                        return $ Map.insert chanName (addToChan newChan cl) s
          Just ec -> do broadcastChan ec (Msg.joinChannelCmd nick chanName)
                        sendClient cl (Msg.usersCmd chanName (userList ec ++ [nick]))
                        return $ Map.insert chanName (addToChan ec cl) s

    (MsgCmd chan msg) -> do
      --putStrLn "msgcmd"
      let chanName = (T.pack chan)
          msgT = (T.pack msg)
      chanS <- readMVar chanS
      case Map.lookup chanName chanS of
        Nothing -> sendClient cl (Msg.unknownChan chanName)
        Just c -> broadcastChan c (Msg.msgCmd nick chanName msgT)

    (LeaveCmd chan) -> do
      chans <- takeMVar chanS
      case Map.lookup (T.pack chan) chans of
        Nothing -> putMVar chanS chans
        Just c -> do
          putMVar chanS (Map.map (flip removeFromChan cl) chans)
          broadcastChan c (Msg.leftChannelCmd nick (T.pack chan))

logHandle :: IO.Handle
logHandle = IO.stdout
--logHandle = unsafePerformIO $ IO.openFile "/dev/null" IO.WriteMode

newServerS :: ServerS
newServerS = Map.empty

newChanS :: ChanS
newChanS = Map.empty