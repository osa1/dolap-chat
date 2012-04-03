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

leftChannelCmd :: T.Text -> T.Text -> T.Text
leftChannelCmd nick channel =
    (T.pack "leave ") `mappend` nick `mappend` (T.pack " ") `mappend` channel

joinChannelCmd :: T.Text -> T.Text -> T.Text
joinChannelCmd nick channel =
    (T.pack "join ") `mappend` nick `mappend` (T.pack " ") `mappend` channel
