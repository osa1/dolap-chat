{-# LANGUAGE OverloadedStrings #-}
module Msg where

import qualified Data.Text as T

nickInUse :: T.Text
nickInUse = "Nick is in use."

loginFirst :: T.Text
loginFirst = "You should login first."