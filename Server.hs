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


data ClientState = Edit | Normal

type Client = (Text, WS.Sink WS.Hybi00)

type ServerState = Map.Map Text (WS.Sink WS.Hybi00)

logHandle :: IO.Handle
logHandle = IO.stdout
--logHandle = unsafePerformIO $ IO.openFile "/dev/null" IO.WriteMode

newServerState :: ServerState
newServerState = Map.empty

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
removeClient (name, _) = Map.delete name

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ (Map.toList clients) $ \(_, sink) -> WS.sendSink sink $ WS.textData message

broadcastExcept :: Text -> Text -> ServerState -> IO ()
broadcastExcept message client clients = do
  T.putStrLn message
  forM_ (filter ((/= client) . fst) (Map.toList clients)) $
    \(_, sink) -> WS.sendSink sink $ WS.textData message

runCmd :: Client -> Cmd -> IO ()
runCmd = undefined

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








