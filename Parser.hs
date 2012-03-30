module Parser where

--import Text.ParserCombinators.Parsec
import Text.Parsec.Text
import Text.Parsec

import Control.Applicative ((<*))

type ChanName = String

data Cmd = LoginCmd String
         | NewmsgCmd String MsgId
         | MovCmd Int
         | RmCmd Int
         | EndmsgCmd
    deriving (Show)

type MsgId = [Int]

cmd :: Parser Cmd
--cmd = undefined
cmd = loginCmd <|> newmsgCmd <|> movCmd <|> rmCmd <|> endmsgCmd

chanName :: Parser ChanName
chanName = many1 (letter <|> oneOf "-" <|> digit) -- <* eof

loginCmd :: Parser Cmd
loginCmd = do
    cmd <- string "login"
    spaces
    nick <- many1 alphaNum
    eof
    return $ LoginCmd nick

msgId :: Parser MsgId
msgId = do
    ids <- (many1 digit) `sepBy` (char '-')
    return (map (\i -> read i :: Int) ids)

newmsgCmd :: Parser Cmd
--newmsgcmd = undefined
newmsgCmd = do
    string "newmsg"
    spaces
    chan <- chanName
    spaces
    msgId <- msgId
    eof
    return $ NewmsgCmd chan msgId

movCmd :: Parser Cmd
movCmd = do
    string "mov"
    spaces
    pos <- (many1 digit)
    return $ MovCmd (read pos :: Int)

rmCmd :: Parser Cmd
rmCmd = do
    string "rm"
    spaces
    a <- (many1 digit)
    return $ RmCmd (read a :: Int)

endmsgCmd :: Parser Cmd
endmsgCmd = do
    string "endmsg"
    return EndmsgCmd



