{-# LANGUAGE RecordWildCards #-}

import Data.List(isPrefixOf, find, elemIndex, sort)
import Data.Char(isAlpha, isAlphaNum)
import Foreign.Storable(poke, peek)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Ptr(castPtr)
import System.IO.Unsafe(unsafePerformIO)
import Data.Bits(shiftR, (.&.), (.|.))
import Data.Word(Word8, Word16, Word32, Word64)

type Float32 = Float
type Float64 = Double

-- lexer

data Direction = DirOpen | DirClose
    deriving(Show)

data SymKeyword = SkFunc
    | SkIf
    | SkElse
    | SkReturn
    | SkBasically
    deriving(Show)

data SymOperator = SoPlus
    | SoMinus
    | SoMul
    | SoDiv
    deriving(Show, Eq)

data SymTest = SoLT
    | SoGT
    | SoEqual
    | SoDiff
    deriving(Show, Eq)

data Symbol = SymSet
    | SymPar Direction
    | SymBra Direction
    | SymDef String
    | SymKW SymKeyword
    | SymNum Float64
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
        ("return", SymKW SkReturn), ("basically", SymKW SkBasically)
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
symGetValue xs = case (reads xs :: [(Float64, String)]) of
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
    | TokNum Float64
    | TokDef String
    | TokBool Bool
    | TokBlock
    | TokIf
    | TokCall String
    | TokString String
    | TokFunction String
    | TokArgs
    | TokReturn
    | TokFile
    deriving(Show, Eq)

data Node = Node (Token, [Node]) deriving(Show, Eq)

typeWeights :: Node -> Int
typeWeights (Node(TokBool _,_)) = 1
typeWeights (Node(TokNum _,_)) = 1
typeWeights (Node(TokString _,_)) = 1
typeWeights (Node(TokDef _,_)) = 2
typeWeights _ = 3

emptyNode :: Token -> Node
emptyNode t = Node(t, [])

unNode :: Node -> (Token, [Node])
unNode (Node x) = x

compareTokenValues :: [Node] -> [Node] -> Ordering
compareTokenValues [] [] = EQ
compareTokenValues (a:_) (b:_) = case (a, b) of
    (Node (TokNum n1, _), Node (TokNum n2, _)) -> compare n1 n2
    (Node (TokDef s1, _), Node (TokDef s2, _)) -> compare s1 s2
    _ -> compare (typeWeights a) (typeWeights b)

nodeCmp :: Node -> Node -> Ordering
nodeCmp (Node(TokFunction _,(Node(TokArgs,as)):_))
        (Node(TokFunction _,(Node(TokArgs,bs)):_))
        | tokenCmp == EQ = compareTokenValues as bs
        | otherwise = tokenCmp
        where tokenCmp = compare (map typeWeights as) (map typeWeights bs)

instance Ord Node where
    compare :: Node -> Node -> Ordering
    compare a b = nodeCmp a b

foo :: [Node]
foo = [
        Node(TokFunction "ninon", [Node(TokArgs, [Node(TokNum 20,[])])]),
        Node(TokFunction "penis", [Node(TokArgs, [Node(TokDef "hello",[])])]),
        Node(TokFunction "paul", [Node(TokArgs, [Node(TokNum 19,[])])]),
        Node(TokFunction "tom", [Node(TokArgs, [Node(TokNum 14,[])])])
    ]

-- term ::= <def> | <float> | <bool> | <call> | <string> | <parenexpr>
parseTerm :: [Symbol] -> Either String (Node, [Symbol])
parseTerm [] = Left "parsing: expected value, got nothing."
parseTerm (SymBool b:xs) = Right (emptyNode $ TokBool b, xs)
parseTerm (SymNum n:xs) = Right (emptyNode $ TokNum n, xs)
parseTerm sx@(SymDef _:SymPar DirOpen:_) = parseCall sx
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

-- callSeries ::= <test>, <test>, ... )
parseCallSeries :: [Symbol] -> Either String ([Node], [Symbol])
parseCallSeries sx = case (parseTest sx) of
    Left err -> Left ("callSeries -> " ++ err)
    Right (n, (SymPar DirClose:xs)) -> Right ([n], xs)
    Right (n, (SymSep:xs)) -> case (parseCallSeries xs) of
        Left err -> Left err
        Right (nx, sx') -> Right (n:nx, sx')
    Right (_, (a:_)) -> Left $ "callSeries: expected separatir, got " ++ show a ++ "."
    Right (_, []) -> Left $ "callSeries: expected separatir, got nothing"

-- call ::= <def> <series>
parseCall :: [Symbol] -> Either String (Node, [Symbol])
parseCall (SymDef f:SymPar DirOpen:SymPar DirClose:xs) =
    Right (emptyNode $ TokCall f, xs)
parseCall (SymDef f:SymPar DirOpen:xs) = case (parseCallSeries xs) of
    Left err -> Left err
    Right (nx, xs') -> Right (Node(TokCall f, nx), xs')
parseCall _ = Left "call: expected 'func(...)'."

-- statement ::= <def> = <test> | <call> | return <test>
parseStatement' :: [Symbol] -> Either String (Node, [Symbol])
parseStatement' [] = Left "statement: expected token."
parseStatement' (SymComma:xs) = parseStatement xs
parseStatement' (SymKW SkReturn:xs) = case (parseTest xs) of
    Left err -> Left $ "return -> " ++ err
    Right (n,(SymComma:xs')) -> Right (Node(TokReturn, [n]), xs')
    Right _ -> Left $ "statement: expected comma."
parseStatement' (x:_) = Left $ "statement: invalid token '" ++ show x ++ "'."

parseStatement :: [Symbol] -> Either String (Node, [Symbol])
parseStatement (SymDef d:SymSet:xs) = case (parseTest xs) of
    Left err -> Left ("statement -> " ++ err)
    Right (n, (SymComma:xs')) -> Right (Node(TokSet d, [n]), xs')
    Right _ -> Left $ "statement: declaration of " ++ show d ++ ": expected ';'."
parseStatement sx@(SymDef d:_) = case (parseCall sx) of
    Left err -> Left $ "statement -> " ++ err
    Right (n, (SymComma:xs)) -> Right (n, xs)
    Right _ -> Left $ "statement: call of " ++ show d ++ ": expected ';'."
parseStatement xs = parseStatement' xs


-- paren ::= ( <test> )
parseParen :: [Symbol] -> Either String (Node, [Symbol])
parseParen (SymPar DirOpen:xs) = case (parseTest xs) of
    Left err -> Left $ "parenthesis -> " ++ err
    Right (n, (SymPar DirClose:xs')) -> Right (n, xs')
    Right _ -> Left "parenthesis: expected ')'."
parseParen _  = Left "parenthesis: expected '('."

-- ifstat ::= if <paren> <block> +? else <block>
parseElse :: [Symbol] -> Either String (Maybe (Node, [Symbol]))
parseElse (SymKW SkElse:xs) = case (parseBlock xs) of
    Left err -> Left $ "else -> " ++ err
    Right (n, xs') -> Right $ Just (n, xs')
parseElse _ = Right Nothing

parseIf :: [Symbol] -> Either String (Node, [Symbol])
parseIf (SymKW SkIf:xs) = case (parseParen xs) of
    Right (c, xs') -> case (parseBlock xs') of
        Right (b, xs'') -> case (parseElse xs'') of
            Right Nothing -> Right (Node(TokIf, [c, b]), xs'')
            Right (Just (e, xs''')) -> Right (Node(TokIf, [c, b, e]), xs''')
            Left err -> Left err
        Left err -> Left $ "if body -> " ++ err
    Left err -> Left $ "if condition -> " ++ err
parseIf _ = Left "if: expected 'if'."

-- line ::= <ifstat> | <statament>
parseLine :: [Symbol] -> Either String (Node, [Symbol])
parseLine sx@(SymKW SkIf:_) = parseIf sx
parseLine xs = parseStatement xs

-- block ::= { <line> <line> ... return <test> } | <line>
parseBlock' :: [Symbol] -> Either String ([Node], [Symbol])
parseBlock' (SymBra DirClose:xs) = Right ([], xs)
parseBlock' xs = case (parseLine xs) of
    Right (n, xs') -> case (parseBlock' xs') of
        Right (content, rest) -> Right (n:content, rest)
        Left err -> Left err
    Left err -> Left $ "block -> " ++ err

parseBlock :: [Symbol] -> Either String (Node, [Symbol])
parseBlock (SymBra DirOpen:xs) = case (parseBlock' xs) of
    Left err -> Left $ "block -> " ++ err
    Right (nodes, xs') -> Right (Node(TokBlock, nodes), xs')
parseBlock xs = parseLine xs

-- parseArgs ::= ( <test>, ... )
parseArgs :: [Symbol] -> Either String (Node, [Symbol])
parseArgs (SymPar DirOpen:SymPar DirClose:xs) = Right (emptyNode TokArgs, xs)
parseArgs (SymPar DirOpen:xs) = case (parseTest xs) of
    Left err -> Left $ "arguments -> " ++ err
    Right (n, (SymPar DirClose:xs')) -> Right (Node(TokArgs, [n]), xs')
    Right (n, (SymSep:xs')) -> case (parseArgs xs') of
        Left err -> Left err
        Right (Node(TokArgs, nx), sx) -> Right (Node(TokArgs, n:nx), sx)
        Right _ -> Left "arguments: can't find more aguments to append."
    Right _ -> Left "arguments: expected ')' or ','."
parseArgs _ = Left "arguments: expected '('."

-- func ::= func <def> <series> <block>
parseFunc :: [Symbol] -> Either String (Node, [Symbol])
parseFunc (SymKW SkFunc:SymDef name:xs) = case (parseArgs xs) of
    Left err -> Left $ "func args -> " ++ err
    Right (args, xs') -> case (parseBlock xs') of
        Left err -> Left $ "func body -> " ++ err
        Right (block, xs'') -> Right (Node(TokFunction name,[args,block]),xs'')
parseFunc (SymKW SkFunc:_) = Left "func: expected function name."
parseFunc _ = Left "func: expected a function keyword."

-- startBlock ::= <basic> | <func>
parseStartBlock :: [Symbol] -> Either String (Node, [Symbol])
parseStartBlock (SymComma:xs) = parseStartBlock xs
parseStartBlock xs = parseFunc xs
-- parseStartBlock _ = Left "not implemented"

parseThisPlease' :: [Symbol] -> [Node] -> Either String Node
parseThisPlease' [] nodes = Right $ Node(TokFile, nodes)
parseThisPlease' sx nodes = case (parseStartBlock sx) of
    Right (n, rest) -> parseThisPlease' rest (nodes ++ [n])
    Left err -> Left $ "parseThisPlease -> " ++ err

parseThisPlease :: String -> Either String Node
parseThisPlease s = case (symGetAll s) of
    Right sx -> parseThisPlease' sx []
    Left err -> Left $ "parseThisPlease -> " ++ err

dispTree :: [Node] -> Int -> IO()
dispTree [] _ = return()
dispTree (Node(t, cx):xs) n = (putStrLn $ (replicate n ' ') ++ (show t))
    >> dispTree cx (n + 4) >> dispTree xs n

prettyParser :: String -> IO()
prettyParser s = case (parseThisPlease s) of
    Left err -> putStrLn $ "[ Exception Caught ] in " ++ err
    Right tree -> dispTree [tree] 0

-- assembler

data ASMAction =
      ISNEN     Word8 Word16
    | JMP       Word16
    | KSHORT    Word8 Word8
    | UGET      Word8 Word8
    | GGET      Word8 Word8
    | GSET      Word8 Word8
    | SUBVN     Word8 Word8 Word8
    | CALL      Word8 Word8 Word8
    | MULVV     Word8 Word8 Word8
    | FNEW      Word8 Word8
    | MOV       Word8 Word8
    | CALLM     Word8 Word8 Word8
    | UCLO      Word16
    | RET1      Word8 Word8
    | RET0      Word8 Word8
    | IDK
    deriving(Show, Eq)

data Context = Context {
        register :: Word8
    } deriving (Show, Eq)

type FunctionCode = [ASMAction]

toChange = 84

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

ctxChangeReg :: Context -> (Word8 -> Word8) -> Context
ctxChangeReg (Context{register = r, ..}) f = Context{register = f r, .. }

ctxEmpty :: Context
ctxEmpty = Context{register = 0}

ctxAdd :: Int -> (Int -> Int)
ctxAdd n = (\ x -> x + n )

ctxMore :: Context -> Context
ctxMore ctx = ctxChangeReg ctx succ

asmRemoveFunctions :: Node -> String -> Node
asmRemoveFunctions (Node(TokFile, xs)) s =
    let x = filter (\ (Node(TokFunction k, _)) -> s /= k ) xs
    in Node(TokFile, x)
asmRemoveFunctions _ _ = error "expected TokFile node"

asmGetFunctions :: Node -> String -> [Node]
asmGetFunctions (Node(TokFile, xs)) s =
    filter (\ (Node(TokFunction k, _)) -> s == k) xs
asmGetFunctions _ _ = error "expected TokFile node"

asmGetSymbols :: [Node] -> [String]
asmGetSymbols [] = []
asmGetSymbols (Node(TokFunction x,_):xs)
    | elem x next = next
    | otherwise = x : next
    where next = asmGetSymbols xs
asmGetSymbols (_:xs) = asmGetSymbols xs

asmParseCall' :: Context -> [Node] -> [ASMAction]
-- asmParseCall' [] = [CALL ]
asmParseCall' _ [] = []
asmParseCall' ctx (_:_) = [UGET (register nctx) toChange]
    where nctx = ctxChangeReg ctx succ

asmParseCall :: Context -> Node -> [ASMAction]
asmParseCall ctx (Node(TokCall _, xs)) =
    [UGET (register nctx) toChange] ++ asmParseCall' nctx xs
    where nctx = ctxChangeReg ctx succ
asmParseCall _ _ = []

asmParseBlock :: Context -> Node -> [ASMAction]
asmParseBlock ctx (Node(TokNum x,[])) = [KSHORT (register $ ctxMore ctx) (round x)]
asmParseBlock ctx (Node(TokReturn, x:xs)) = asmParseBlock ctx x ++ [RET1 (register $ ctxMore ctx) 2]
asmParseBlock ctx (Node(TokBlock, x:xs)) = asmParseBlock ctx x
    where   helper [] = []
            helper (x:xs) = asmParseBlock ctx x ++ helper xs
asmParseBlock ctx n@(Node(TokCall f, x:xs)) = asmParseCall ctx n

asmPatternMatchingNum :: Context -> Float64 -> [Node] -> [ASMAction]
asmPatternMatchingNum ctx n (z:[]) =
    [ISNEN 0 $ round n, JMP $ intToWord16 $ succ $ length ys] ++ ys
    where ys = asmParseBlock ctx z

asmFromSubfunction :: Context -> [Node] -> [Node] -> [ASMAction]
asmFromSubfunction ctx (Node(TokNum v,[]):xs) cx = asmPatternMatchingNum ctx v cx
asmFromSubfunction _ (Node(TokDef v,[]):xs) cx = [IDK]
asmFromSubfunction _ _ _ = [IDK]

asmUnpackFunction :: Node -> ([Node], [Node])
asmUnpackFunction (Node(TokFunction _,(Node(TokArgs,as):bs))) = (as, bs)

asmFromFunction' :: Context -> [Node] -> [ASMAction]
asmFromFunction' _ [] = []
asmFromFunction' ctx (x:xs) = (asmFromSubfunction ctx a b) ++ next
    where   next = asmFromFunction' ctx xs
            (a, b) = asmUnpackFunction x

asmFromFunction :: Context -> [Node] -> FunctionCode
asmFromFunction ctx fx = asmFromFunction' ctx $ sort fx

ast2LuassemblyJit' :: Context -> Node -> [String] -> [FunctionCode]
ast2LuassemblyJit' _ _ [] = []
ast2LuassemblyJit' ctx n (x:xs) = (asmFromFunction ctx $ asmGetFunctions n x) : next
    where next = ast2LuassemblyJit' ctx n xs

ast2LuassemblyJit :: Node -> [FunctionCode]
ast2LuassemblyJit node@(Node(TokFile, xs)) =
    ast2LuassemblyJit' ctxEmpty node (asmGetSymbols xs)

prettyAsm :: String -> IO()
prettyAsm s = case (parseThisPlease s) of
    Left err -> putStrLn $ "[ Exception Caught ] in " ++ err
    Right n -> print $ ast2LuassemblyJit n

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
