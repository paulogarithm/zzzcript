-- Baby Glados

import System.Environment
import System.Exit
import Data.List
import Data.Char
import Text.Read(reads)

data Symbol = While | If | Else |
    LBra | LPar | RBra | RPar |
    Semi | Eof | Plus | Minus | Less | Great | Equal |
    Id String | Number Float | Boolean Bool
    deriving(Show)

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

getSymUser :: String -> Either String (Symbol,String)
getSymUser s = case (reads s :: [(Float, String)]) of
    [(n,rest)] -> Right (Number n,rest)
    _ -> case (span isAlphaNum s) of
        (var, rest)
            | not (null var) && isAlphaNum (head var) -> Right (Id var,rest)
            | otherwise -> Left ("parsing: invalid word: " ++ rest)

getAllSyms :: String -> [Symbol] -> Either String [Symbol]
getAllSyms [] list = Right list
getAllSyms (' ':xs) list = getAllSyms xs list
getAllSyms ('\n':xs) list = getAllSyms xs list
getAllSyms (x:xs) list = case (getSymChar x) of
    Just (Eof) -> Right list
    Just sym -> getAllSyms xs (list++[sym])
    Nothing -> case (getSymStr (x:xs)) of
        Just (sym,rest) -> getAllSyms rest (list++[sym])
        Nothing -> case (getSymUser (x:xs)) of
            Right (sym,rest) -> getAllSyms rest (list++[sym])
            Left err -> Left err

-- Parsing

data Token = TAdd | TSub | TLt | TGt | TIf1 | TIf2 | TWhile |
    TEmpty | TSeq | TExpr | TProg | TSet |
    TVar String | TConst Float
    deriving(Show)

data Node = NullNode | Node (Token,(Node,Node,Node)) deriving(Show)

type AST = Node

type ParsingRes = Maybe (Node,[Symbol])

newNode :: Token -> Node
newNode t = Node (t, (NullNode, NullNode, NullNode))

giveResult :: Token -> [Symbol] -> ParsingRes
giveResult t l = Just (newNode t, l)

parseTerm :: [Symbol] -> ParsingRes
parseTerm [] = Nothing
parseTerm (x:xs) = case (x) of
    Id s -> giveResult (TVar s) xs
    Number c -> giveResult (TConst c) xs
    _ -> parseParen (x:xs)

_parseSumloop :: [Symbol] -> Node -> ParsingRes
_parseSumloop (Plus:xs) n = case (parseTerm xs) of
    Just (newn, xxs) -> _parseSumloop xxs (Node (TAdd, (n, newn, NullNode)))
    Nothing -> Nothing
_parseSumloop (Minus:xs) n = case (parseTerm xs) of
    Just (newn, xxs) -> _parseSumloop xxs (Node (TSub, (n, newn, NullNode)))
    Nothing -> Nothing
_parseSumloop l n = Just (n, l)

parseSum :: [Symbol] -> ParsingRes
parseSum [] = Nothing
parseSum l = case (parseTerm l) of
    Nothing -> Nothing
    Just (n,xs) -> case (_parseSumloop xs n) of
        Nothing -> Nothing
        x -> x

_parseTestloop :: [Symbol] -> Node -> ParsingRes
_parseTestloop (Minus:xs) n = case (parseSum xs) of
    Just (newn, xxs) -> _parseTestloop xxs (Node (TLt, (n, newn, NullNode)))
    Nothing -> Nothing
_parseTestloop (Great:xs) n = case (parseSum xs) of
    Just (newn, xxs) -> _parseTestloop xxs (Node (TGt, (n, newn, NullNode)))
    Nothing -> Nothing
_parseTestloop l n = Just (n, l)

parseTest :: [Symbol] -> ParsingRes
parseTest [] = Nothing
parseTest l = case (parseSum l) of
    Nothing -> Nothing
    Just (n,xs) -> case (_parseTestloop xs n) of
        Nothing -> Nothing
        x -> x

parseExpr :: [Symbol] -> ParsingRes
parseExpr l@((Id i):xs) = case (parseTest l) of
    Nothing -> Nothing
    Just (n@(Node(TVar i,_)),(Equal:xxs)) -> case (parseExpr xxs) of
        Nothing -> Nothing
        Just (nn,rest) -> Just (Node(TSet, (n, nn, NullNode)), rest)
    x -> x
parseExpr l = parseTest l

parseParen :: [Symbol] -> ParsingRes
parseParen (LPar:xs) = case (parseExpr xs) of
    Just (n,(RPar:rest)) -> Just (n, rest)
    _ -> Nothing
parseParen _ = Nothing

parseStatement :: [Symbol] -> ParsingRes
parseStatement (If:xs) = case (parseParen xs) of
    Nothing -> Nothing
    Just (a,ys) -> case (parseStatement ys) of
        Nothing -> Nothing
        Just (b,(Else:zs)) -> case (parseStatement zs) of
            Nothing -> Nothing
            Just (c,ns) -> Just (Node(TIf2,(a,b,c)),ns)
        Just (b,zs) -> Just (Node(TIf1,(a,b,NullNode)),zs)
parseStatement l = case (parseExpr l) of
    Just (a,(Semi:[])) -> Just(Node(TExpr,(a,NullNode,NullNode)),[])
    Just (a,(Semi:xs)) -> case (parseStatement xs) of
        Nothing -> Nothing
        Just (b,xs) -> Just(Node(TExpr,(a,b,NullNode)),xs)
    _ -> Nothing

parsing :: String -> Either String AST
parsing s = case (getAllSyms s []) of
    Right syms -> case (parseStatement syms) of
        Just (n,_) -> Right n
        Nothing -> Left "error"
    Left err -> Left err

-- Main

main :: IO()
main = putStrLn "hello"
