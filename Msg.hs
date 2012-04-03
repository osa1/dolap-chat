{-# LANGUAGE OverloadedStrings #-}
module Msg where

import qualified Data.Text as T

nickInUse :: T.Text
nickInUse = "Nick is in use."

loginFirst :: T.Text
loginFirst = "You should login first."

unknownCommand :: String -> T.Text
unknownCommand = T.pack . (++) "Unknown command: "

leftChannelCmd :: String -> String -> T.Text
leftChannelCmd nick channel = T.pack $ "leave " ++ nick ++ " " ++ channel