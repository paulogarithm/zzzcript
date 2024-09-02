-- Baby Glados

import System.Environment
import System.Exit
import Data.List

data Symbol = While | If | Else |
    LBra | LPar | RBra | RPar |
    Semi | Eof | Plus | Minus | Less | Great | Equal |
    Id String | Number Float | Boolean Bool
    deriving(Show)

data Node = Node {  id :: String,
                    num :: Float,
                    children :: (Node, Node, Node)
                 } deriving(Show)

-- Lexing

symbolList :: [(String,Symbol)]
symbolList = [
        ("while",While),
        ("if",If),
        ("else",Else),
        ("true",Boolean True),
        ("false",Boolean False)
    ]

getSymChar :: Char -> Maybe Symbol
getSymChar '}' = Just RBra
getSymChar '{' = Just LBra
getSymChar '+' = Just Plus
getSymChar '-' = Just Minus
getSymChar '<' = Just Less
getSymChar '>' = Just Great
getSymChar ')' = Just RPar
getSymChar '(' = Just LPar
getSymChar '=' = Just Equal
getSymChar ';' = Just Semi
getSymChar _ = Nothing

getSymStr :: String -> Maybe (Symbol,String)
getSymStr str = case (find (\(x,_) -> isPrefixOf x str) symbolList) of
    Just (symstr,sym) -> Just (sym, drop (length symstr) str)
    Nothing -> Nothing

loopAction :: String -> [Symbol] -> Maybe [Symbol]
loopAction [] list = Just list
loopAction (' ':xs) list = loopAction xs list
loopAction ('\n':xs) list = loopAction xs list
loopAction (x:xs) list = case (getSymChar x) of
    Just (Eof) -> Just list
    Just sym -> loopAction xs (list++[sym])
    Nothing -> case (getSymStr (x:xs)) of
        Just (ssym,rest) -> loopAction rest (list++[ssym])
        Nothing -> Nothing

main :: IO()
main = putStrLn "hello"