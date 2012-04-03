module Channel where

import Control.Monad (forM_)

import qualified Data.Text as T
import qualified Data.Map as Map

import qualified Network.WebSockets as WS

import Client

data Chan = Chan { getName    :: T.Text
                 , getClients :: Map.Map T.Text (WS.Sink WS.Hybi10)
                 }

newChan :: T.Text -> Chan
newChan n = Chan n (Map.empty)

addToChan :: Chan -> Client -> Chan
addToChan (Chan name clients) (Client nick sink) =
  Chan name (Map.insert nick sink clients)

removeFromChan :: Chan -> Client -> Chan
removeFromChan (Chan name clients) (Client nick _) =
  Chan name (Map.delete nick clients)

broadcastChan :: Chan -> T.Text -> IO ()
broadcastChan (Chan _ clients) text =
  forM_ (Map.toList clients) (\(_, sink) -> WS.sendSink sink $ WS.textData text)

chanClientList :: Chan -> [Client]
chanClientList (Chan _ clients) =
  map (\(nick, sink) -> Client nick sink) $ Map.toList clients