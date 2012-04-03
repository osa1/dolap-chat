{-# LANGUAGE OverloadedStrings #-}
module Msg where

import Data.Monoid (mappend)
import qualified Data.Text as T

nickInUse :: T.Text
nickInUse = "Nick is in use."

loginFirst :: T.Text
loginFirst = "You should login first."

unknownCommand :: String -> T.Text
unknownCommand = T.pack . (++) "Unknown command: "

unknownChan :: T.Text -> T.Text
unknownChan = mappend "Unknown chan: "

msgCmd :: T.Text -> T.Text -> T.Text -> T.Text
msgCmd client chan msg =
    "msg " `mappend` (T.intercalate " " [client, chan, msg])

leftChannelCmd :: T.Text -> T.Text -> T.Text
leftChannelCmd nick channel =
    "leave " `mappend` nick `mappend` " " `mappend` channel

joinChannelCmd :: T.Text -> T.Text -> T.Text
joinChannelCmd nick channel =
    "join " `mappend` nick `mappend` " " `mappend` channel
