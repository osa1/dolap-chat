module Client where

import Control.Monad (forM_)

import qualified Data.Text as T
import qualified Data.Map as Map

import qualified Network.WebSockets as WS

data Client = Client { getNick :: T.Text
                     , getSink :: WS.Sink WS.Hybi10
                     }

sendClient :: Client -> T.Text -> IO ()
sendClient (Client _ sink) = WS.sendSink sink . WS.textData

sendClients :: [Client] -> T.Text -> IO ()
sendClients clients msg = do
  forM_ clients (\(Client _ sink) -> WS.sendSink sink $ WS.textData msg)

sendClientsMap :: Map.Map T.Text (WS.Sink WS.Hybi10) -> T.Text -> IO ()
sendClientsMap clients msg = do
  forM_ (Map.toList clients) (\(_, sink) ->
    WS.sendSink sink $ WS.textData msg)

