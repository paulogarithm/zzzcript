import Text.Read(reads)
import Data.Char(isDigit, isSeparator)
import Data.List(find)
import System.IO(isEOF, hFlush, stdout)
import System.Exit(exitWith, ExitCode(ExitSuccess, ExitFailure))
import Data.Maybe(fromJust)
import Data.Tuple(swap)

-- lexer

data Symbol = SymParL
    | SymParR
    | SymNum Float
    | SymBool Bool
    | SymDef String
    deriving(Show)

isDefName :: Char -> Bool
isDefName ')' = False
isDefName '(' = False
isDefName c = (not (isDigit c)) && (not (isSeparator c))

getSymbol :: String -> Maybe (Symbol, String)
getSymbol ('(':xs) = Just (SymParL, xs)
getSymbol (')':xs) = Just (SymParR, xs)
getSymbol ('#':'t':xs) = Just (SymBool True, xs)
getSymbol ('#':'f':xs) = Just (SymBool False, xs)
getSymbol xs = case (reads xs :: [(Float, String)]) of
    [(n,rest)] -> Just (SymNum n,rest)
    _ -> case (span isDefName xs) of
        (v, r)  | not (null v) && isDefName (head v) -> Just (SymDef v, r)
                | otherwise -> Nothing

getAllSyms' :: String -> Either String [Symbol]
getAllSyms' [] = Right []
getAllSyms' (' ':xs) = getAllSyms' xs
getAllSyms' ('\n':xs) = getAllSyms' xs
getAllSyms' str = case (getSymbol str) of
    Just (sym, rest) -> case (getAllSyms' rest) of
        Right l -> Right (l ++ [sym])
        e -> e
    Nothing -> Left "getAllSyms: invalid symbol."

getAllSyms :: String -> Either String [Symbol]
getAllSyms s = case getAllSyms' s of
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
type CallbackResult = Either String (Value, Env)
type BuiltinCallback = [Node] -> Env -> CallbackResult
type FunctionBody = ([String], Node)

data Value = ValNone
    | ValNum Float
    | ValBool Bool
    | ValBuiltin BuiltinCallback
    | ValFunction FunctionBody

instance Show Value where
    show ValNone = "<none>"
    show (ValNum n) = show n
    show (ValBool n) = show n
    show (ValBuiltin _) = "<procedure>"
    show (ValFunction (x,_)) = "<function" ++ show x ++ ">"

envRawGet :: String -> Env -> Maybe Value
envRawGet s e = case (find (\(k, _) -> k == s) e) of
    Just (_, v) -> Just v
    Nothing -> Nothing

envGetCallable :: String -> Env -> Maybe (Either FunctionBody BuiltinCallback)
envGetCallable s [] = Nothing
envGetCallable s ((name, ValBuiltin f):xs)  | s == name = Just (Right f)
                                            | otherwise = envGetCallable s xs
envGetCallable s ((name, ValFunction f):xs) | s == name = Just (Left f)
                                            | otherwise = envGetCallable s xs
envGetCallable s ((_,x):xs) = envGetCallable s xs

envCheckExists :: String -> Env -> Bool
envCheckExists s e = case (find (\(x,_) -> x == s) e) of
    Just _ -> True
    Nothing -> False

funcGetOnlyDefs :: [Node] -> [String] -> Either String (String, [String])
funcGetOnlyDefs [] x = Right ("", x)
funcGetOnlyDefs (Node(TokDef x, _):xs) [] = case funcGetOnlyDefs xs [x] of
    Right (_, ys) -> Right (x, reverse $ tail $ reverse ys)
    Left err -> Left err
funcGetOnlyDefs (Node(TokDef s, _):xs) ys = case funcGetOnlyDefs xs ys of
    Right (a, b) -> Right (a, s:b)
    Left err -> Left err
funcGetOnlyDefs _ _ = Left "funcOnlyDef: not a TokDef."

createFunction' :: (String, [String]) -> Node -> Env -> (Value, Env)
createFunction' (n, a) b env = (ValNone, (n, (ValFunction (a, b))):env)

createFunction :: [Node] -> Node -> Env -> CallbackResult
createFunction header body env = case funcGetOnlyDefs header [] of
    Right x -> Right (createFunction' x body env)
    Left err -> Left ("createFunction -> " ++ err)

-- todo here: the function stuff i dont wanna do
envDefine :: BuiltinCallback
envDefine [(Node(TokDef k, _)), node] env
    | envCheckExists k env = Left "define: already defined."
    | otherwise = case (evalThisTree node env) of
        Right (v,nenv) -> Right (ValNone, (k, v):nenv)
        Left err -> Left ("define -> " ++ err)
envDefine [(Node(TokElem, headr)), body] env = (createFunction headr body env)
envDefine _ _ = Left "define: bad format."

addValues :: Value -> Value -> Either String Value
addValues (ValNum a) (ValNum b) = Right (ValNum (a + b))
addValues _ _ = Left "addValues: cant perform add on these."

subValues :: Value -> Value -> Either String Value
subValues (ValNum a) (ValNum b) = Right (ValNum (a - b))
subValues _ _ = Left "subValues: cant perform add on these."

envAdd :: BuiltinCallback
envAdd [a, b] env = case (evalThisTree a env) of
    Right (av,env) -> case (evalThisTree b env) of
        Right (bv,env) -> case (addValues av bv) of
            Right v -> Right (v, env)
            Left e -> Left e
        Left e -> Left e
    Left e -> Left e
envAdd _ _ = Left "add: bad format, expected (add a b)."

envSub :: BuiltinCallback
envSub [a, b] env = case (evalThisTree a env) of
    Right (av,env) -> case (evalThisTree b env) of
        Right (bv,env) -> case (subValues av bv) of
            Right v -> Right (v, env)
            Left e -> Left e
        Left e -> Left e
    Left e -> Left e
envSub _ _ = Left "sub: bad format, expected (sub a b)."

defaultEnv :: Env
defaultEnv = [
        ("define", ValBuiltin envDefine),
        ("+", ValBuiltin envAdd),
        ("-", ValBuiltin envSub)
    ]

-- bytecode

data ByteCode = IPush Value
    | ICall String
    | ISwap
    deriving(Show)

nodeToClosure :: [Node] -> [ByteCode]
nodeToClosure _ = []

-- eval

extendEnv :: [Node] -> [String] -> Env -> Either String Env
extendEnv [] [] env = Right env
extendEnv (node:ns) (key:ks) env1 = case evalThisTree node env1 of
    Right (value, env2) -> case extendEnv ns ks env2 of
        Right nenv -> Right ((key, value):nenv)
        Left err -> Left err
    Left err -> Left ("extendEnv -> " ++ err)
extendEnv [] _ _ = Left "extendEnv: too few arguments."
extendEnv _ [] _ = Left "extendEnv: too many arguments."

callFunction :: [Node] -> FunctionBody -> Env -> Either String (Value, Env)
callFunction ns (ks, body) env = case extendEnv ns ks env of
        Right nenv -> case evalThisTree body nenv of
            Right (v, _) -> Right (v, env)
            Left err -> Left ("callFunction -> " ++ err)
        Left err -> Left ("callFunction -> " ++ err)

tokenToValue :: Token -> Env -> Either String Value
tokenToValue (TokNum n) env = Right (ValNum n)
tokenToValue (TokBool n) env = Right (ValBool n)
tokenToValue (TokDef d) env = case (envRawGet d env) of
    Just x -> Right x
    _ -> Left ("tokenToValue: you didnt defined '" ++ d ++ "'.")
tokenToValue _ _ = Left "tokenToValue: couldn't convert token."

evalThisTree :: Node -> Env -> Either String (Value, Env)
evalThisTree (Node(TokElem, ((Node ((TokDef d),_)):xs))) env =
    case (envGetCallable d env) of
        Just (Right f) -> f xs env
        Just (Left f) -> callFunction xs f env
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
infiniteLoop env = putStr "\x1b[32;1mλ\x1b[m > "
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
