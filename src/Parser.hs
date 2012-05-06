{-# OPTIONS -fno-warn-unused-do-bind #-}
module Parser where

--import Text.ParserCombinators.Parsec
import Text.Parsec.Text
import Text.Parsec
import Control.Applicative ((<*))
import Control.Monad (liftM)

type ChanName = String

data Cmd = LoginCmd String
         | JoinCmd String
         | MsgCmd String String
         | LeaveCmd String
         | HelpCmd
    deriving (Show)


cmd :: Parser Cmd
--cmd = undefined
--cmd = msgCmd <|> singleparam

cmd = try msgCmd <|> try leaveCmd <|> try loginCmd <|> try joinCmd <|> helpCmd

joinCmd :: Parser Cmd
joinCmd = do
    string "join"
    spaces
    nick <- many1 anyChar
    eof
    return $ JoinCmd nick

leaveCmd :: Parser Cmd
leaveCmd = do
    string "leave"
    spaces
    chan <- many1 anyChar
    eof
    return $ LeaveCmd chan

loginCmd :: Parser Cmd
loginCmd = do
    string "login"
    spaces
    nick <- many1 alphaNum
    eof
    return $ LoginCmd nick

helpCmd :: Parser Cmd
helpCmd = string "help" >> eof >> return HelpCmd

chanName :: Parser ChanName
chanName = many1 (letter <|> oneOf "-" <|> digit)


--singleparam :: Parser Cmd
--singleparam = do
--    cmd <- choice [try $ string "leave", try $ string "login", try $ string "join"]
--    spaces
--    nick <- many1 anyChar
--    eof
--    --return $ LoginCmd nick
--    return $ case nick of
--               _ | cmd == "login" -> LoginCmd nick
--                 | cmd == "leave" -> LeaveCmd nick
--                 | cmd == "join"  ->  JoinCmd nick


msgCmd :: Parser Cmd
--newmsgcmd = undefined
msgCmd = do
    string "msg"
    spaces
    chan <- chanName
    spaces
    msg <- many1 anyChar
    eof
    return $ MsgCmd chan msg



