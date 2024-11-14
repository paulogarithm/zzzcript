import Data.List(isPrefixOf, find, elemIndex)
import Data.Char(isAlpha, isAlphaNum)
import Foreign.Storable(Storable, poke, peek)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Ptr(castPtr)
import System.IO.Unsafe(unsafePerformIO)
import Data.Bits(shiftR, (.&.), (.|.))
import Data.Word(Word8, Word32, Word64)

type Float32 = Float
type Float64 = Double

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
    | SoDiff
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
    | SymString String
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
        ("*", SymOpe SoMul), (";", SymComma), (",", SymSep),
        ("!=", SymTest SoDiff)
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

symGetString :: String -> Either String [Symbol]
symGetString str = case (elemIndex '"' str) of
    Just x -> case symGetAll $ drop (succ x) str of
        Right xs -> Right ((SymString $ take x str) : xs)
        Left err -> Left err
    Nothing -> Left "string: can't get end of string..."

symGetAll :: String -> Either String [Symbol]
symGetAll "" = Right []
symGetAll (' ':xs) = symGetAll xs
symGetAll ('\n':xs) = symGetAll xs
symGetAll ('"':xs) = symGetString xs
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
    | TokString String
    deriving(Show)

data Node = Node (Token, [Node]) deriving(Show)

emptyNode :: Token -> Node
emptyNode t = Node(t, [])

-- term ::= <def> | <float> | <bool> | <call> | <string> | <parenexpr>
parseTerm :: [Symbol] -> Either String (Node, [Symbol])
parseTerm [] = Left "parsing: expected value, got nothing."
parseTerm (SymBool b:xs) = Right (emptyNode $ TokBool b, xs)
parseTerm (SymNum n:xs) = Right (emptyNode $ TokNum n, xs)
parseTerm sx@(SymDef d:SymPar DirOpen:xs) = parseCall sx
parseTerm (SymDef d:xs) = Right (emptyNode $ TokDef d, xs)
parseTerm (SymString s:xs) = Right (emptyNode $ TokString s, xs)
parseTerm xs = parseParen xs

-- operation ::= <term> | <operation> <opesym> <term>
parseOperation' :: [Symbol] -> Node -> Either String (Node, [Symbol])
parseOperation' ((SymOpe x):xs) n = case (parseTerm xs) of
    Right (nn, ys) -> parseOperation' ys (Node (TokOpe x, [n, nn]))
    Left err -> Left ("operation -> " ++ err)
parseOperation' l n = Right (n, l)

parseOperation :: [Symbol] -> Either String (Node, [Symbol])
parseOperation l = case (parseTerm l) of
    Left err -> Left ("operation -> " ++ err)
    Right (n,xs) -> case (parseOperation' xs n) of
        Left err -> Left err
        Right x -> Right x

-- test ::= <operation> | <test> <testsym> <operation>
parseTest' :: [Symbol] -> Node -> Either String (Node, [Symbol])
parseTest' ((SymTest x):xs) n = case (parseOperation xs) of
    Right (nn, ys) -> parseTest' ys (Node (TokTest x, [n, nn]))
    Left err -> Left ("test -> " ++ err)
parseTest' l n = Right (n, l)

parseTest :: [Symbol] -> Either String (Node, [Symbol])
parseTest sx = case (parseOperation sx) of
    Left err -> Left ("test -> " ++ err)
    Right (n, xs) -> case (parseTest' xs n) of
        Left err -> Left err
        Right x -> Right x

-- call ::= <def> ( <test>, <test>, ... )
parseCallChildren :: [Symbol] -> Either String ([Node], [Symbol])
parseCallChildren sx = case (parseTest sx) of
    Left err -> Left ("call -> " ++ err)
    Right (n, (SymPar DirClose:xs)) -> Right ([n], xs)
    Right (n, (SymSep:xs)) -> case (parseCallChildren xs) of
        Left err -> Left err
        Right (nx, sx) -> Right (n:nx, sx)

parseCall :: [Symbol] -> Either String (Node, [Symbol])
parseCall (SymDef f:SymPar DirOpen:SymPar DirClose:xs) =
    Right (emptyNode $ TokCall f, xs)
parseCall (SymDef f:SymPar DirOpen:xs) = case (parseCallChildren xs) of
    Left err -> Left err
    Right (nx, xs) -> Right (Node(TokCall f, nx), xs)
parseCall _ = Left "call: expected 'func(...)'."

-- statement ::= <def> = <test> | <call>
parseStatement :: [Symbol] -> Either String (Node, [Symbol])
parseStatement (SymDef d:SymSet:xs) = case (parseTest xs) of
    Left err -> Left ("statement -> " ++ err)
    Right (n, (SymComma:xs)) -> Right (Node(TokSet d, [n]), xs)
    Right _ -> Left $ "declaration of " ++ show d ++ ": expected ';'."
parseStatement sx@(SymDef d:xs) = case (parseCall sx) of
    Left err -> Left $ "statement -> " ++ err
    Right (n, (SymComma:xs)) -> Right (n, xs)
    Right _ -> Left $ "call of " ++ show d ++ ": expected ';'."
parseStatement (SymComma:xs) = parseStatement xs
parseStatement (x:xs) = Left $ "statement: invalid token '" ++ show x ++ "'."
parseStatement [] = Left "statement: expected token."

-- paren ::= ( <test> )
parseParen :: [Symbol] -> Either String (Node, [Symbol])
parseParen (SymPar DirOpen:xs) = case (parseTest xs) of
    Left err -> Left $ "parenthesis -> " ++ err
    Right (n, (SymPar DirClose:xs)) -> Right (n, xs)
    Right _ -> Left "parenthesis: expected ')'."
parseParen _  = Left "parenthesis: expected '('."

-- ifstat ::= if <paren> <block> +? else <block>
parseElse :: [Symbol] -> Either String (Maybe (Node, [Symbol]))
parseElse (SymKW SkElse:xs) = case (parseBlock xs) of
    Left err -> Left $ "else -> " ++ err
    Right (n, xs) -> Right (Just (n, xs))
parseElse xs = Right Nothing

parseIf :: [Symbol] -> Either String (Node, [Symbol])
parseIf (SymKW SkIf:xs) = case (parseParen xs) of
    Right (c, xs) -> case (parseBlock xs) of
        Right (b, xs) -> case (parseElse xs) of
            Right Nothing -> Right (Node(TokIf, [c, b]), xs)
            Right (Just (e, xs)) -> Right (Node(TokIf, [c, b, e]), xs)
            Left err -> Left err
        Left err -> Left $ "if body -> " ++ err
    Left err -> Left $ "if condition -> " ++ err

-- line ::= <ifstat> | <statament>
parseLine :: [Symbol] -> Either String (Node, [Symbol])
parseLine sx@(SymKW SkIf:xs) = parseIf sx
parseLine xs = parseStatement xs

-- block ::= { <line> <line> ... } | <line>
parseBlock' :: [Symbol] -> Either String ([Node], [Symbol])
parseBlock' (SymBra DirClose:xs) = Right ([], xs)
parseBlock' xs = case (parseLine xs) of
    Right (n, xs) -> case (parseBlock' xs) of
        Right (content, rest) -> Right (n:content, rest)
        Left err -> Left err
    Left err -> Left $ "block -> " ++ err

parseBlock :: [Symbol] -> Either String (Node, [Symbol])
parseBlock (SymBra DirOpen:xs) = case (parseBlock' xs) of
    Left err -> Left $ "block -> " ++ err
    Right (nodes, xs) -> Right (Node(TokBlock, nodes), xs)
parseBlock xs = parseLine xs

-- compiler

data OpCode = OpLoad
    | OpCall
    deriving(Show, Eq, Bounded)

doubleRawcastInt :: Float64 -> Word64
doubleRawcastInt f = unsafePerformIO i
    where i = alloca (\p -> poke (castPtr p) f >> peek p) :: IO Word64

floatRawcastInt :: Float32 -> Word32
floatRawcastInt f = unsafePerformIO i
    where i = alloca (\p -> poke (castPtr p) f >> peek p) :: IO Word32

-- /* Add ULEB128 value to buffer. */
-- static void bcwrite_uleb128(BCWriteCtx *ctx, uint32_t v)
-- {
--   MSize n = ctx->sb.n;
--   uint8_t *p = (uint8_t *)ctx->sb.buf;
--   for (; v >= 0x80; v >>= 7)
--     p[n++] = (uint8_t)((v & 0x7f) | 0x80);
--   p[n++] = (uint8_t)v;
--   ctx->sb.n = n;
-- }


word32toword8' :: Word32 -> [Word8]
word32toword8' x = map fromIntegral [ x .&. 0xff ]

word32toword8 :: Word32 -> Word8
word32toword8 x = head $ word32toword8' x

uleb128Encode' :: Word32 -> [Word8] -> [Word8]
uleb128Encode' x list   | x >= 0x80 = uleb128Encode' newV newList 
                        | otherwise = list ++ [k .&. 0xff]
                        where   newV = x `shiftR` 7
                                newList = list ++ [((k .&. 0x7f) .|. 0x80) .&. 0xff]
                                k = word32toword8 x

uleb128Encode :: Word32 -> [Word8]
uleb128Encode x = uleb128Encode' x []

word64splitter :: Word64 -> (Word32, Word32)
word64splitter x = (fromIntegral (x .&. 0xFFFFFFFF), fromIntegral (x `shiftR` 32))

double2uleb :: Float64 -> [Word8]
double2uleb n = (uleb128Encode x) ++ (uleb128Encode y)
    where   x = fst res
            y = snd res
            res = word64splitter $ doubleRawcastInt n

opNumbers :: [(Int, OpCode)]
opNumbers = [(0, OpLoad)]

-- instance Enum OpCode where
--     fromEnum n = case (find (\(_, y) -> y == n) opNumbers) of
--         Just (x, _) -> x
--         Nothing -> error $ "opcode: '" ++ show n ++ "' not found."

data OpValue = OpCode OpCode
    | OpValue Float
    deriving(Show, Eq)

-- compileThisPlease :: Node -> [Int]
-- compileThisPlease _ = [fromEnum OpLoad]

-- main

parseThisPlease :: String -> Either String Node
parseThisPlease s = case (symGetAll s) of
    Right sx -> case (parseBlock sx) of
        Right (n, _) -> Right n
        Left err -> Left ("parseThisPlease -> " ++ err)
    Left err -> Left ("parseThisPlease -> " ++ err)

dispTree :: [Node] -> Int -> IO()
dispTree [] n = return()
dispTree (Node(t, cx):xs) n = (putStrLn $ (replicate n ' ') ++ (show t))
    >> dispTree cx (n + 4) >> dispTree xs n

prettyParser :: String -> IO()
prettyParser s = case (parseThisPlease s) of
    Left err -> putStrLn $ "[ Exception Caught ] in " ++ err
    Right tree -> dispTree [tree] 0
