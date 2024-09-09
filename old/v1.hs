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

type ParsingRes = Either String (Node,[Symbol])

newNode :: Token -> Node
newNode t = Node (t, (NullNode, NullNode, NullNode))

giveResult :: Token -> [Symbol] -> ParsingRes
giveResult t l = Right (newNode t, l)

parseTerm :: [Symbol] -> ParsingRes
parseTerm [] = Left "expected anything, but nothing was found."
parseTerm (x:xs) = case (x) of
    Id s -> giveResult (TVar s) xs
    Number c -> giveResult (TConst c) xs
    _ -> parseParen (x:xs)

_parseSumloop :: [Symbol] -> Node -> ParsingRes
_parseSumloop (Plus:xs) n = case (parseTerm xs) of
    Right (newn, xxs) -> _parseSumloop xxs (Node (TAdd, (n, newn, NullNode)))
    Left err -> Left ("in sum (+), " ++ err)
_parseSumloop (Minus:xs) n = case (parseTerm xs) of
    Right (newn, xxs) -> _parseSumloop xxs (Node (TSub, (n, newn, NullNode)))
    Left err -> Left ("in sum (-), " ++ err)
_parseSumloop l n = Right (n, l)

parseSum :: [Symbol] -> ParsingRes
parseSum l = case (parseTerm l) of
    Left err -> Left ("in sum, " ++ err)
    Right (n,xs) -> case (_parseSumloop xs n) of
        Left err -> Left err
        Right x -> Right x

_parseTestloop :: [Symbol] -> Node -> ParsingRes
_parseTestloop (Less:xs) n = case (parseTerm xs) of
    Right (newn, xxs) -> _parseTestloop xxs (Node (TLt, (n, newn, NullNode)))
    Left err -> Left ("in test (<), " ++ err)
_parseTestloop (Great:xs) n = case (parseTerm xs) of
    Right (newn, xxs) -> _parseTestloop xxs (Node (TGt, (n, newn, NullNode)))
    Left err -> Left ("in test (>), " ++ err)
_parseTestloop l n = Right (n, l)

parseTest :: [Symbol] -> ParsingRes
parseTest l = case (parseTerm l) of
    Left err -> Left ("in test, " ++ err)
    Right (n,xs) -> case (_parseTestloop xs n) of
        Left err -> Left err
        Right x -> Right x

parseExpr :: [Symbol] -> ParsingRes
parseExpr l@((Id i):xs) = case (parseTest l) of
    Left err -> Left ("in expression, " ++ err)
    Right (n@(Node(TVar i,_)),(Equal:xxs)) -> case (parseExpr xxs) of
        Left err -> Left err
        Right (nn,rest) -> Right (Node(TSet, (n, nn, NullNode)), rest)
    Right x -> Right x
parseExpr l = parseTest l

parseParen :: [Symbol] -> ParsingRes
parseParen (LPar:xs) = case (parseExpr xs) of
    Right (n,(RPar:rest)) -> Right (n, rest)
    Right (n,[]) -> Left "expected RPar, got nothing."
    Right (n,(x:xs)) -> Left ("expected RPar, got " ++ (show x) ++ ".")
    Left err -> Left ("in parenthesis, " ++ err)
parseParen (x:xs) = Left ("expected left parenthesis, got " ++ (show x) ++ ".")
parseParen [] = Left "expected left parenthesis, got nothing."

_parseBrackets :: [Symbol] -> Node -> ParsingRes
_parseBrackets (RBra:xs) n = Right (n,xs)
_parseBrackets [] _ = Left "expect right bracket."
_parseBrackets l n = case (parseStatement l) of
    Left err -> Left ("in brackets, " ++ err)
    Right (a, rest) -> _parseBrackets rest (Node(TSeq, (n, a, NullNode)))

parseStatement :: [Symbol] -> ParsingRes
parseStatement (If:xs) = case (parseParen xs) of
    Left err -> Left ("in the if condition, " ++ err)
    Right (a,ys) -> case (parseStatement ys) of
        Left err -> Left ("in the if statement, " ++ err)
        Right (b,(Else:zs)) -> case (parseStatement zs) of
            Left err -> Left ("in the else statement, " ++ err)
            Right (c,ns) -> Right (Node(TIf2,(a,b,c)),ns)
        Right (b,zs) -> Right (Node(TIf1,(a,b,NullNode)),zs)
parseStatement (While:xs) = case (parseParen xs) of
    Left err -> Left ("in the while condition, " ++ err)
    Right (a,ys) -> case (parseStatement ys) of
        Left err -> Left ("in the while statement, " ++ err)
        Right (b,zs) -> Right (Node(TWhile,(a,b,NullNode)),zs)
parseStatement (LBra:xs) = _parseBrackets xs (newNode TEmpty)
parseStatement l = case (parseExpr l) of
    Right (a,(Semi:xs)) -> Right (Node(TExpr,(a,NullNode,NullNode)),xs)
    Right (a,rest) -> Left "expected semicolon."
    Left err -> Left ("in statement, " ++ err)

parsing :: String -> Either String AST
parsing s = case (getAllSyms s []) of
    Right syms -> case (parseStatement syms) of
        Right (n,_) -> Right n
        Left err -> Left err
    Left err -> Left err

-- ce qui arrive quand je fais 'while (a < 20) { a = 10; b = 20; }'
foo = Right (Node (TWhile,(
    Node (TLt,(
        Node (TVar "a",(NullNode,NullNode,NullNode)),
        Node (TConst 20.0,(NullNode,NullNode,NullNode)),
        NullNode)
    ),
    Node (TSeq,(
        Node (TSeq,(
            Node (TEmpty,(NullNode,NullNode,NullNode)),
            Node (TExpr,(
                Node (TSet,(
                    Node (TVar "a",(NullNode,NullNode,NullNode)),
                    Node (TConst 10.0,(NullNode,NullNode,NullNode)),
                    NullNode)
                ),
                NullNode,
                NullNode)
            ),
            NullNode)
        ),
        Node (TExpr,(
            Node (TSet,(
                Node (TVar "b",(NullNode,NullNode,NullNode)),
                Node (TConst 20.0,(NullNode,NullNode,NullNode)),
                NullNode)
            ),
            NullNode,
            NullNode)
        ),
        NullNode)
    ),
    NullNode)
))

-- Main

main :: IO()
main = putStrLn "hello"
