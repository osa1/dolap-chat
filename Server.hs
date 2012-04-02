{-# LANGUAGE OverloadedStrings #-}
--import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
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
--import System.IO.Unsafe (unsafePerformIO)


type Client  = (Text, WS.Sink WS.Hybi00)

type ChannelUsers = (Text, [Client])
type ChannelState = Map.Map Text ChannelUsers
--type ServerState = Map.Map Text (WS.Sink WS.Hybi00)

data ServerState = ServerState { getChannels :: ChannelState
                               , getUsers    :: Map.Map Text (WS.Sink WS.Hybi00)
                               }


logHandle :: IO.Handle
logHandle = IO.stdout
--logHandle = unsafePerformIO $ IO.openFile "/dev/null" IO.WriteMode

newServerState :: ServerState
newServerState = ServerState Map.empty Map.empty

newChannelState :: ChannelState
newChannelState = Map.empty

numClients :: ServerState -> Int
numClients (ServerState _ users) = Map.size users

clientExists :: Text -> ServerState -> Bool
clientExists name (ServerState _ users) =
    case Map.lookup name users of
        Just _  -> True
        Nothing -> False

addClient :: Client -> ServerState -> ServerState
addClient (name, sock) (ServerState cs users) =
  ServerState cs (Map.insert name sock users)

removeClient :: Client -> ServerState -> ServerState
removeClient (name, _) (ServerState cs users) = ServerState cs (Map.delete name users)

broadcast :: Text -> ServerState -> IO ()
broadcast message (ServerState _ clients) = do
    T.putStrLn message
    forM_ (Map.toList clients) $ \(_, sink) -> WS.sendSink sink $ WS.textData message

broadcastExcept :: Text -> Text -> ServerState -> IO ()
broadcastExcept message client (ServerState _ clients) = do
  T.putStrLn message
  forM_ (filter ((/= client) . fst) (Map.toList clients)) $
    \(_, sink) -> WS.sendSink sink $ WS.textData message

runCmd :: Client -> Cmd -> IO ()
runCmd client (JoinCmd chan)    = undefined
runCmd client (LeaveCmd chan)   = undefined
runCmd client (MsgCmd chan msg) = undefined
runCmd client (LoginCmd nick)   = undefined

main :: IO ()
main = do
    state <- newMVar newServerState
    Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = 3000
      , Warp.settingsHost = Warp.Host "0.0.0.0"
      , Warp.settingsIntercept = WaiWS.intercept (application state)
      } (Static.staticApp Static.defaultWebAppSettings)

login :: MVar ServerState -> WS.Sink WS.Hybi00 -> WS.WebSockets WS.Hybi00 (Text)
login state sink = do
  msg <- WS.receiveData
  case P.parse cmd "" msg of
    Right (LoginCmd nick) -> do

        (ServerState cs s) <- liftIO $ takeMVar state
        let n = T.pack nick
            s' = case Map.lookup n s of
                    Nothing -> (Map.insert n sink s, True)
                    Just _  -> (s, False)

        if (snd s')
          then do liftIO $ putMVar state $ ServerState cs (fst s')
                  liftIO $ WS.sendSink sink $ WS.textData ("logged in" :: T.Text)
                  return n
          else do liftIO $ putMVar state $ ServerState cs (fst s')
                  liftIO $ WS.sendSink sink $ WS.textData ("choose another nick" :: T.Text)
                  login state sink

        --liftIO $ modifyMVar_ state $ \s -> do
        --    let n  = T.pack nick
        --        s' = case Map.lookup n s of
        --                Nothing -> (addClient (n, sink) s, True)
        --                Just _  -> (s, False)
        --    if (snd s')
        --      then liftIO $ WS.sendSink sink $ WS.textData ("yoooo" :: T.Text)
        --      else liftIO $ WS.sendSink sink $ WS.textData ("nop" :: T.Text)

        --    return $ fst s'
    otherwise -> do liftIO $ WS.sendSink sink $ WS.textData ("nop" :: T.Text)
                    login state sink

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
--application = undefined
application state rq = do
    WS.acceptRequest rq
    sink <- WS.getSink
    nick <- login state sink
    talk state (nick, sink)


talk :: WS.Protocol p => MVar ServerState -> Client -> WS.WebSockets p ()
talk state client@(nick, sink) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ readMVar state >>= broadcast
        (nick `mappend` msg)
    talk state client

  where catchDisconnect = undefined
  --where catchDisconnect e = case fromException e of
  --        Just WS.ConnectionClosed -> do
  --            liftIO $ modifyMVar_ state $ \s -> do
  --              return $ removeClient client s
  --            broadcast ("disconnect" `mappend` nick) (liftIO $ readMVar state)








