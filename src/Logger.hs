module Logger where

import Control.Concurrent
--import Data.List
import Parser (Cmd)

type Log = (String, Cmd)

newLogCache :: IO (MVar [Log])
newLogCache = newMVar []

log :: String -> Cmd -> MVar [Log] -> IO ()
log nick cmd cache = do
    modifyMVar_ cache (\cache -> do
        return $ cache ++ [(nick, cmd)])
