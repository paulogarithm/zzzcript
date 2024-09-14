import Data.List(isPrefixOf, find)
import Data.Char(isAlpha, isAlphaNum)

-- lexer

data Direction = DirOpen | DirClose
    deriving(Show)

data SymKeyword = SkVal | SkFunc
    deriving(Show)

data SymOperator = SoSet
    | SoPlus
    | SoMinus
    | SoMul
    | SoDiv
    | SoLT
    | SoGT
    | SoEqual
    deriving(Show)

data Symbol = SymPar Direction
    | SymBra Direction
    | SymDef String
    | SymKW SymKeyword
    | SymNum Float
    | SymOpe SymOperator
    | SymBool Bool
    | SymComma
    deriving Show

symKeywordList :: [(String, Symbol)]
symKeywordList = [("func", SymKW SkFunc), ("val", SymKW SkVal),
        ("true", SymBool True), ("false", SymBool False)
    ]

symOpeList :: [(String, Symbol)]
symOpeList = [("(", SymPar DirOpen), (")", SymPar DirClose),
        ("{", SymBra DirOpen), ("}", SymBra DirClose), ("+", SymOpe SoPlus),
        ("-", SymOpe SoMinus), ("<", SymOpe SoLT), (">", SymOpe SoGT),
        ("=", SymOpe SoSet), ("==", SymOpe SoEqual), ("/", SymOpe SoDiv),
        ("*", SymOpe SoMul), (";", SymComma)
    ]

symGetValue :: String -> Either String (Symbol, String)
symGetValue xs = case (reads xs :: [(Float, String)]) of
    [(n, rest)] -> Right (SymNum n, rest)
    _ -> case (span isAlphaNum xs) of
        (v, r)  | not (null v) && isAlpha (head v) -> Right (SymDef v, r)
                | otherwise -> Left (
                    "lexer: can't parse symbol -> " ++ take 5 xs ++ "...")

symNextSym :: String -> Either String (Symbol, String)
symNextSym xs = case (find (\(x, _) -> isPrefixOf x xs) symOpeList) of
    Just (s, sym) -> Right (sym, drop (length s) xs)
    Nothing -> case (find (\(x, _) -> isPrefixOf x xs) symKeywordList) of
        Just (s, sym) -> Right (sym, drop (length s) xs)
        Nothing -> symGetValue xs

symGetAll :: String -> Either String [Symbol]
symGetAll "" = Right []
symGetAll (' ':xs) = symGetAll xs
symGetAll ('\n':xs) = symGetAll xs
symGetAll a = case (symNextSym a) of
    Right (sym, b) -> case (symGetAll b) of
        Right xs -> Right $ sym:xs
        Left err -> Left err
    Left err -> Left ("symGetAll -> " ++ err)

