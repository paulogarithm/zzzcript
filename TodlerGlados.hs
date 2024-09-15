import Data.List(isPrefixOf, find)
import Data.Char(isAlpha, isAlphaNum)

-- lexer

data Direction = DirOpen | DirClose
    deriving(Show)

data SymKeyword = SkFunc
    | SkIf
    | SkElse
    | SkReturn
    deriving(Show)

data SymOperator = SoPlus
    | SoMinus
    | SoMul
    | SoDiv
    deriving(Show)

data SymTest = SoLT
    | SoGT
    | SoEqual
    deriving(Show)

data Symbol = SymSet
    | SymPar Direction
    | SymBra Direction
    | SymDef String
    | SymKW SymKeyword
    | SymNum Float
    | SymOpe SymOperator
    | SymTest SymTest
    | SymBool Bool
    | SymComma
    | SymSep
    deriving Show

symKeywordList :: [(String, Symbol)]
symKeywordList = [("func", SymKW SkFunc), ("true", SymBool True),
        ("false", SymBool False), ("if", SymKW SkIf), ("else", SymKW SkElse),
        ("return", SymKW SkReturn)
    ]

symOpeList :: [(String, Symbol)]
symOpeList = [("(", SymPar DirOpen), (")", SymPar DirClose),
        ("{", SymBra DirOpen), ("}", SymBra DirClose), ("+", SymOpe SoPlus),
        ("-", SymOpe SoMinus), ("<", SymTest SoLT), (">", SymTest SoGT),
        ("==", SymTest SoEqual), ("=", SymSet), ("/", SymOpe SoDiv),
        ("*", SymOpe SoMul), (";", SymComma), (",", SymSep)
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

-- parser

data Token = TokProcedure
    | TokSet String
    | TokOpe SymOperator
    | TokTest SymTest
    | TokNum Float
    | TokDef String
    | TokBool Bool
    | TokBlock
    | TokIf
    | TokCall String
    deriving(Show)

data Node = Node (Token, [Node]) deriving(Show)

emptyNode :: Token -> Node
emptyNode t = Node(t, [])

-- term ::= <def> | <float> | <bool> | <call> | <parenexpr>
parseTerm :: [Symbol] -> Either String (Node, [Symbol])
parseTerm [] = Left "parsing: expected value, got nothing."
parseTerm (SymBool b:xs) = Right (emptyNode $ TokBool b, xs)
parseTerm (SymNum n:xs) = Right (emptyNode $ TokNum n, xs)
parseTerm sx@(SymDef d:SymPar DirOpen:xs) = parseCall sx
parseTerm (SymDef d:xs) = Right (emptyNode $ TokDef d, xs)
parseTerm _ = Left "parsing: nothing for now."

-- operation ::= <term> | <operation> <opesym> <term>
parseOperation' :: [Symbol] -> Node -> Either String (Node, [Symbol])
parseOperation' ((SymOpe x):xs) n = case (parseTerm xs) of
    Right (nn, ys) -> parseOperation' ys (Node (TokOpe x, [n, nn]))
    Left err -> Left ("parseOperation -> " ++ err)
parseOperation' l n = Right (n, l)

parseOperation :: [Symbol] -> Either String (Node, [Symbol])
parseOperation l = case (parseTerm l) of
    Left err -> Left ("parseOperation -> " ++ err)
    Right (n,xs) -> case (parseOperation' xs n) of
        Left err -> Left err
        Right x -> Right x

-- test ::= <operation> | <test> <testsym> <operation>
parseTest' :: [Symbol] -> Node -> Either String (Node, [Symbol])
parseTest' ((SymTest x):xs) n = case (parseOperation xs) of
    Right (nn, ys) -> parseTest' ys (Node (TokTest x, [n, nn]))
    Left err -> Left ("parseTest -> " ++ err)
parseTest' l n = Right (n, l)

parseTest :: [Symbol] -> Either String (Node, [Symbol])
parseTest sx = case (parseOperation sx) of
    Left err -> Left ("parseTest -> " ++ err)
    Right (n, xs) -> case (parseTest' xs n) of
        Left err -> Left err
        Right x -> Right x

-- call ::= <def> ( <test>, <test>, ... )
parseCallChildren :: [Symbol] -> Either String ([Node], [Symbol])
parseCallChildren sx = case (parseTest sx) of
    Left err -> Left ("parseCall -> " ++ err)
    Right (n, (SymPar DirClose:xs)) -> Right ([n], xs)
    Right (n, (SymSep:xs)) -> case (parseCallChildren xs) of
        Left err -> Left err
        Right (nx, sx) -> Right (n:nx, sx)

parseCall :: [Symbol] -> Either String (Node, [Symbol])
parseCall (SymDef f:SymPar DirOpen:SymPar DirClose:xs) =
    Right (emptyNode $ TokCall f, xs)
parseCall (SymDef f:SymPar DirOpen:xs) = case (parseCallChildren xs) of
    Left err -> Left ("parseCall -> " ++ err)
    Right (nx, xs) -> Right (Node(TokCall f, nx), xs)
parseCall _ = Left "parseCall: expected 'func(...)'."

-- declar ::= <def> = <test>
parseDeclar :: [Symbol] -> Either String (Node, [Symbol])
parseDeclar (SymDef d:SymSet:xs) = case (parseTest xs) of
    Left err -> Left ("parseDeclar -> " ++ err)
    Right (n, xs) -> Right (Node(TokSet d, [n]), xs)

parseThisPlease :: String -> Either String Node
parseThisPlease s = case (symGetAll s) of
    Right sx -> case (parseDeclar sx) of
        Right (n, _) -> Right n
        Left err -> Left ("parseThisPlease -> " ++ err)
    Left err -> Left ("parseThisPlease -> " ++ err)


