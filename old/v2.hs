-- Baby Glados

import Data.Char(isAlphaNum)
import Text.Read(reads)
import Data.List(isPrefixOf, find)

-- Lexing

data Sym = SymLPar | SymRPar | SymDef |
    SymAdd | SymSub | SymMul | SymDiv | SymLess | SymGreat |
    SymID String | SymBool Bool | SymNumber Float
    deriving(Show, Eq)

symList :: [(String,Sym)]
symList = [
        ("define ", SymDef),
        ("#f", SymBool False),
        ("#t", SymBool True),
        ("+ ", SymAdd),
        ("- ", SymSub),
        ("* ", SymMul),
        ("/ ", SymDiv),
        ("< ", SymRPar),
        ("> ", SymRPar),
        ("(", SymLPar),
        (")", SymRPar)
    ]

getSymKeyword :: String -> Maybe (Sym,String)
getSymKeyword str = case (find (\(x,_) -> isPrefixOf x str) symList) of
    Just (symstr,sym) -> Just (sym, drop (length symstr) str)
    Nothing -> Nothing

getSymData :: String -> Maybe (Sym, String)
getSymData s = case (reads s :: [(Float, String)]) of
    [(n,rest)] -> Just (SymNumber n,rest)
    _ -> case (span isAlphaNum s) of
        (var, rest)
            | not (null var) && isAlphaNum (head var) -> Just (SymID var,rest)
            | otherwise -> Nothing

getAllSyms :: String -> [Sym] -> Either String [Sym]
getAllSyms [] list = Right list
getAllSyms (' ':xs) list = getAllSyms xs list
getAllSyms ('\n':xs) list = getAllSyms xs list
getAllSyms str list = case (getSymKeyword str) of
    Just (sym,rest) -> getAllSyms rest (list++[sym])
    Nothing -> case (getSymData str) of
        Just (sym,rest) -> getAllSyms rest (list++[sym])
        Nothing -> Left "can't parse element."

-- Parsing

data Token = TokDef | TokAdd | TokSub | TokMul | TokDiv | TokVar |
    TokID String | TokNum Float | TokBool Bool
    deriving(Show)

data Node = NullNode | Node(Token,(Node,Node))
    deriving(Show)

emptyNode :: Token -> Node
emptyNode t = Node(t,(NullNode,NullNode))

parseData :: [Sym] -> Either String (Node,[Sym])
parseData (SymID id:xs) = Right (emptyNode (TokID id),xs)
parseData (SymNumber n:xs) = Right (emptyNode (TokNum n),xs)
parseData (SymBool b:xs) = Right (emptyNode (TokBool b),xs)
parseData _ = Left "in data: expected booolean, number or variable."

parseDef :: [Sym] -> Either String (Node, [Sym])
parseDef (SymID id:xs) = case (parseData xs) of
    Left err -> Left ("in def, " ++ err)
    Right (n,rest) -> Right (Node(TokVar,(emptyNode (TokID id),n)),rest)
parseDef _ = Left "in def: expected SymId."

parseAdd :: [Sym] -> Either String (Node, [Sym])
parseAdd xs = case (parseData xs) of
    Left err -> Left ("in add, " ++ err)
    Right (a,as) -> case (parseData as) of
        Left err -> Left ("in add, " ++ err)
        Right (b,bs) -> Right (Node(TokAdd,(a,b)),bs)

type CallBack = [Sym] -> Either String (Node, [Sym])

parseList :: [(Sym,CallBack)]
parseList = [
        (SymDef, parseDef),
        (SymAdd, parseAdd)
    ]

parseParen :: [Sym] -> Either String (Node, [Sym])
parseParen [] = Left "in paren: expected something after."
parseParen (sym:xs) = case (find (\(x,_) -> x == sym) parseList) of
    Nothing -> Left ("in paren: can't use '" ++ (show sym) ++ "'.")
    Just (_,f) -> case (f xs) of
        Left err -> Left ("in paren, " ++ err)
        Right (n,(SymRPar:rest)) -> Right (n,rest)
        Right _ -> Left "in paren: expected right parenthesis."

parseStatement :: [Sym] -> Either String (Node, [Sym])
parseStatement (SymLPar:xs) = parseParen xs
parseStatement any = parseData any

parseAll :: String -> Either String (Node, [Sym])
parseAll s = case (getAllSyms s []) of
    Left err -> Left err
    Right l -> parseStatement l

prettyParser :: String -> IO()
prettyParser s = case (parseAll s) of
    Left err -> putStrLn ("*** ERROR : " ++ err)
    Right (node,_) -> print node
