{-# LANGUAGE OverloadedStrings #-}
--import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_)
import Control.Concurrent hiding (newChan)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Data.Map as Map

--import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
--import Data.FileEmbed (embedDir)

import Parser
import qualified Text.Parsec as P

import qualified IO as IO
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace (trace)



type Client = (Text, WS.Sink WS.Hybi10)
type ChannelState = (Text, Map.Map Text (WS.Sink WS.Hybi10))
type ChannelIndex = Map.Map Text ChannelState

type ServerState = Map.Map Text (WS.Sink WS.Hybi10)

logHandle :: IO.Handle
logHandle = IO.stdout
--logHandle = unsafePerformIO $ IO.openFile "/dev/null" IO.WriteMode

newServerState :: ServerState
newServerState = Map.empty

newChannelIndex :: ChannelIndex
newChannelIndex = Map.empty

newChan :: Text -> ChannelState
--newChan = flip (,) Map.empty -- lol, this is what I wrote at first
newChan name = (name, Map.empty)

numClients :: ServerState -> Int
numClients = Map.size

clientExists :: Text -> ServerState -> Bool
clientExists name state =
    case Map.lookup name state of
        Just _  -> True
        Nothing -> False

addClient :: Client -> ServerState -> ServerState
addClient (name, sock) = Map.insert name sock

removeClient :: Client -> ServerState -> ServerState
removeClient client state
  | trace ("removeClient " ++ show (fst client)) False = undefined
removeClient (name, _) a = Map.delete name a

joinUser :: Client -> ChannelState -> ChannelState
joinUser (nick, sink) (chanName, userIndex) =
  (chanName, Map.insert nick sink userIndex)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients
  | trace ("broadcast " ++ show message) False = undefined
broadcast message clients = do
    T.putStrLn message
    forM_ (Map.toList clients) $ \(_, sink) -> WS.sendSink sink $ WS.textData message

broadcastExcept :: Text -> Text -> ServerState -> IO ()
broadcastExcept message client clients
  | trace ("broadcastExcept " ++ show message ++ " " ++ show client) False = undefined
broadcastExcept message client clients = do
  T.putStrLn message
  forM_ (filter ((/= client) . fst) (Map.toList clients)) $
    \(_, sink) -> WS.sendSink sink $ WS.textData message

sendUserlist :: Client -> ChannelState -> IO ()
sendUserlist client@(nick, sink) cs@(cn, chanmap) =
  let msg = T.pack $ "users " ++ T.unpack cn ++ " " ++
              intercalate "," (map T.unpack (Map.keys $ chanmap))
  in liftIO $ WS.sendSink sink $ WS.textData msg

runCmd :: Client -> Cmd -> MVar ChannelIndex -> IO ()
runCmd client@(nick, sink) cmd cindex =
  --type ChannelIndex = Map.Map Text ChannelState

  case cmd of
    (JoinCmd chan) -> do
      chanlist <- takeMVar cindex
      let chantext = T.pack chan
      case Map.lookup chantext chanlist of
        Nothing -> do
          let cindex' = Map.insert chantext (joinUser client (newChan chantext)) chanlist
          putMVar cindex cindex'
        Just c  -> do
          putMVar cindex (Map.insert chantext (joinUser client c) chanlist)
          liftIO $ broadcast (T.pack $ concat ["join ", (T.unpack nick), " ", chan]) (snd c)

    (MsgCmd chan msg) -> do
      chanlist <- readMVar cindex
      let chantext = T.pack chan
      case Map.lookup chantext chanlist of
        Nothing -> WS.sendSink sink $ WS.textData ("no such channel" :: T.Text)
        Just c  -> broadcastExcept (T.pack msg') nick (snd c)
                     where  msg' = concat ["msg ", (T.unpack nick), " ", chan, " ", msg]

    (LeaveCmd chan) -> do
      chanlist <- takeMVar cindex
      let chantext = T.pack chan
      case Map.lookup chantext chanlist of
        Nothing -> putMVar cindex chanlist
        Just (cn, c) -> do
          let newchan = Map.delete nick c
          -- type ChannelState = (Text, Map.Map Text (WS.Sink WS.Hybi10))
          -- type ChannelIndex = Map.Map Text ChannelState
          liftIO $ putMVar cindex (Map.insert chantext (chantext, newchan) chanlist)
          liftIO $ broadcast (T.pack $ concat ["leave ", T.unpack nick, " ", chan]) newchan

main :: IO ()
main = do
    serverState  <- newMVar newServerState
    channelIndex <- newMVar newChannelIndex
    Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = 3000
      , Warp.settingsHost = "0.0.0.0"
      , Warp.settingsIntercept = WaiWS.intercept (application serverState channelIndex)
      } (Static.staticApp Static.defaultWebAppSettings)

login :: MVar ServerState -> WS.Sink WS.Hybi10 -> WS.WebSockets WS.Hybi10 (Text)
login state sink = do
  msg <- WS.receiveData
  case P.parse cmd "" msg of
    Right (LoginCmd nick) -> do

        s <- liftIO $ takeMVar state
        let n = T.pack nick
            s' = case Map.lookup n s of
                    Nothing -> (addClient (n, sink) s, True)
                    Just _  -> (s, False)

        if (snd s')
          then do liftIO $ putMVar state (fst s')
                  liftIO $ WS.sendSink sink $ WS.textData ("logged in" :: T.Text)
                  return n
          else do liftIO $ putMVar state (fst s')
                  liftIO $ WS.sendSink sink $ WS.textData ("choose another nick" :: T.Text)
                  login state sink

    otherwise -> do liftIO $ putStrLn (T.unpack msg)
                    liftIO $ WS.sendSink sink $ WS.textData ("nop" :: T.Text)
                    login state sink

application :: MVar ServerState -> MVar ChannelIndex -> WS.Request -> WS.WebSockets WS.Hybi10 ()
--application = undefined
application state chans rq = do
    WS.acceptRequest rq
    sink <- WS.getSink
    nick <- login state sink
    WS.spawnPingThread 5
    talk state chans (nick, sink)


talk :: WS.Protocol p => MVar ServerState -> MVar ChannelIndex -> Client -> WS.WebSockets p ()
talk state chans client@(nick, sink) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ case (P.parse cmd "" msg) of
               Left  err -> WS.sendSink sink $ WS.textData ("err" :: T.Text)
               Right cmd -> runCmd client cmd chans
    talk state chans client

  --WS.WebSockets p ()
  where --catchDisconnect :: WS.Protocol p => GHC.Exception.SomeException -> WS.WebSockets p ()
        catchDisconnect e | trace ("catchDisconnect " ++ show nick) False = undefined
        catchDisconnect e = case fromException e of
          Just WS.ConnectionClosed -> do
              state' <- liftIO $ readMVar state
              liftIO $ modifyMVar_ state $ \s -> do
                return $ removeClient client s
              liftIO $ broadcast ("disconnect " `mappend` nick) state'
              return ()
          otherwise -> do liftIO $ putStrLn "bir hata olustu"
