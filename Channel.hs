module Channel where

import Control.Monad (forM_)

import qualified Data.Text as T
import qualified Data.Map as Map

import qualified Network.WebSockets as WS

import Client

data Chan = Chan { getClients :: Map.Map T.Text (WS.Sink WS.Hybi10) }

newChan :: Chan
newChan = Chan Map.empty

addToChan :: Chan -> Client -> Chan
addToChan (Chan clients) (Client nick sink) =
  Chan (Map.insert nick sink clients)

removeFromChan :: Chan -> Client -> Chan
removeFromChan (Chan clients) (Client nick _) =
  Chan (Map.delete nick clients)

broadcastChan :: Chan -> T.Text -> IO ()
broadcastChan (Chan clients) text =
  forM_ (Map.toList clients) (\(_, sink) -> WS.sendSink sink $ WS.textData text)

chanClientList :: Chan -> [Client]
chanClientList (Chan clients) =
  map (\(nick, sink) -> Client nick sink) $ Map.toList clients

--removeUser :: [Chan] -> T.Text -> IO ([Chan])
--removeUser = undefined