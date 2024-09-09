import Text.Read(reads)
import Data.Char(isAlphaNum)
import Data.List(find)
import System.IO(isEOF, hFlush, stdout)
import System.Exit(exitWith, ExitCode( ExitSuccess, ExitFailure))

-- lexer

data Symbol = SymParL
    | SymParR
    | SymNum Float
    | SymBool Bool
    | SymDef String
    deriving(Show)

getSymbol :: String -> Maybe (Symbol, String)
getSymbol ('(':xs) = Just (SymParL, xs)
getSymbol (')':xs) = Just (SymParR, xs)
getSymbol ('#':'t':xs) = Just (SymBool True, xs)
getSymbol ('#':'f':xs) = Just (SymBool False, xs)
getSymbol xs = case (reads xs :: [(Float, String)]) of
    [(n,rest)] -> Just (SymNum n,rest)
    _ -> case (span isAlphaNum xs) of
        (v, r)  | not (null v) && isAlphaNum (head v) -> Just (SymDef v, r)
                | otherwise -> Nothing

_getAllSyms :: String -> Either String [Symbol]
_getAllSyms [] = Right []
_getAllSyms (' ':xs) = _getAllSyms xs
_getAllSyms ('\n':xs) = _getAllSyms xs
_getAllSyms str = case (getSymbol str) of
    Just (sym, rest) -> case (_getAllSyms rest) of
        Right l -> Right (l ++ [sym])
        e -> e
    Nothing -> Left "_getAllSyms: invalid symbol."

getAllSyms :: String -> Either String [Symbol]
getAllSyms s = case _getAllSyms s of
    Right l -> Right (reverse l)
    e -> e

-- parser

data Token = TokElem
    | TokDef String
    | TokNum Float
    | TokBool Bool
    deriving(Show)

data Node = NodeNull
    | Node (Token, [Node])
    deriving(Show)

type Result = (Node, [Symbol])

emptyNode :: Token -> Node
emptyNode tok = Node (tok, [])

parseData :: [Symbol] -> Either String Result
parseData ((SymNum n):xs) = Right ((emptyNode (TokNum n)), xs)
parseData ((SymBool b):xs) = Right ((emptyNode (TokBool b)), xs)
parseData ((SymDef d):xs) = Right ((emptyNode (TokDef d)), xs)
parseData _ = Left "parseData: invalid token."

parseParenthesis :: [Symbol] -> Either String Result
parseParenthesis [] = Left "parseParenthesis: expected ')'."
parseParenthesis (SymParR:xs) = Right (emptyNode TokElem, xs)
parseParenthesis xs = case (parseStatement xs) of
    Right(n, rest) -> case (parseParenthesis rest) of
        Right(Node(nt, chdn), l) -> Right (Node(nt, n:chdn), l)
        Left e -> Left ("parseParenthesis.1 -> " ++ e)
    Left e -> Left ("parseParenthesis.2 -> " ++ e)

parseStatement :: [Symbol] -> Either String Result
parseStatement (SymParL:xs) = parseParenthesis xs
parseStatement xs = parseData xs

parseThisPlease :: String -> Either String Node
parseThisPlease s = case (getAllSyms s) of
    Right l -> case (parseStatement l) of
        Right (ast, []) -> Right ast
        Right _ -> Left "parseThisPlease: extra instructions founded."
        Left e -> Left ("parseThisPlease -> " ++ e)
    Left e -> Left e

-- env

type Env = [(String, Value)]
type BuiltinCallback = [Node] -> Env -> Either String (Value,Env)

data Value = ValNone
    | ValNum Float
    | ValBool Bool
    | ValBuiltin BuiltinCallback

instance Show Value where
    show ValNone = ""
    show (ValNum n) = show n
    show (ValBool n) = show n
    show (ValBuiltin _) = "<function>"

envRawGet :: String -> Env -> Maybe Value
envRawGet s e = case (find (\(k, _) -> k == s) e) of
    Just (_, v) -> Just v
    Nothing -> Nothing

envGetBuiltin :: String -> Env -> Maybe BuiltinCallback
envGetBuiltin s [] = Nothing
envGetBuiltin s ((name, ValBuiltin f):xs)   | s == name = Just f
                                            | otherwise = envGetBuiltin s xs
envGetBuiltin s ((_,x):xs) = envGetBuiltin s xs

envCheckExists :: String -> Env -> Bool
envCheckExists s e = case (find (\(x,_) -> x == s) e) of
    Just _ -> True
    Nothing -> False

-- todo here: the function stuff i dont wanna do
envDefine :: BuiltinCallback
envDefine [(Node(TokDef k, _)), node] env
    | envCheckExists k env = Left "define: already defined."
    | otherwise = case (evalThisTree node env) of
        Right (v,nenv) -> Right (ValNone, (k, v):nenv)
        Left err -> Left ("define -> " ++ err)
envDefine _ _ = Left "define: bad format."

addValues :: Value -> Value -> Either String Value
addValues (ValNum a) (ValNum b) = Right (ValNum (a + b))
addValues _ _ = Left "addValues: cant perform add on these."

envAdd :: BuiltinCallback
envAdd [a, b] env = case (evalThisTree a env) of
    Right (av,env) -> case (evalThisTree b env) of
        Right (bv,env) -> case (addValues av bv) of
            Right v -> Right (v, env)
            Left e -> Left e
        Left e -> Left e
    Left e -> Left e
envAdd _ _ = Left "add: bad format, expected (add a b)."

defaultEnv :: Env
defaultEnv = [
        ("define", ValBuiltin envDefine),
        ("add", ValBuiltin envAdd)
    ]

-- eval

tokenToValue :: Token -> Env -> Either String Value
tokenToValue (TokNum n) env = Right (ValNum n)
tokenToValue (TokBool n) env = Right (ValBool n)
tokenToValue (TokDef d) env = case (envRawGet d env) of
    Just x -> Right x
    _ -> Left ("tokenToValue: you didnt defined '" ++ d ++ "'.")
tokenToValue _ _ = Left "tokenToValue: couldn't convert token."

evalThisTree :: Node -> Env -> Either String (Value, Env)
evalThisTree (Node(TokElem, ((Node ((TokDef d),_)):xs))) env =
    case (envGetBuiltin d env) of
        Just f -> f xs env
        _ -> Left ("eval: cant eval '" ++ d ++ "'.")
evalThisTree (Node(tok, _)) env = case (tokenToValue tok env) of
    Right v -> Right (v, env)
    Left err -> Left err

evalThisPlease :: Env -> String -> Either String (Value, Env)
evalThisPlease env s = case (parseThisPlease s) of
    Right n -> evalThisTree n env
    Left e -> Left e

-- main code

infiniteLoop :: Env -> IO()
infiniteLoop env = putStr "\x1b[35;1mλ\x1b[m > "
    >> hFlush stdout >> isEOF >>= (\x -> case x of
        True -> exitWith ExitSuccess
        False -> getLine >>= (\s -> case (evalThisPlease env s) of
                Left err -> putStrLn ("code failed because " ++ err)
                    >> exitWith (ExitFailure 84)
                Right (ValNone, nenv) -> infiniteLoop nenv
                Right (val, nenv) -> print val >> infiniteLoop nenv
            ) )

main :: IO()
main = infiniteLoop defaultEnv
