module Client where

import qualified Data.Text as T

import qualified Network.WebSockets as WS

data Client = Client { getNick :: T.Text
                     , getSink :: WS.Sink WS.Hybi10
                     }

sendClient :: Client -> T.Text -> IO ()
sendClient (Client _ sink) = WS.sendSink sink . WS.textData

