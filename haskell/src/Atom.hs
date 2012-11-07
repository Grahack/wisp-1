module Atom where

import qualified Data.Map as DM

type HostDict = DM.Map W W

data W =
   WBool Bool |
   WChar Char |
   WNum Integer |
   WList [W] |
   WDict HostDict |
   WSym String |
   WType BuiltinType |
   WVauRun { capturedEnv :: HostDict, bindArgSym :: String, bindEnvSym :: String, capturedExpression :: W } |
   WEval | WIf | WParse | WQuote | WSequence |
   WReadFile | WVau | WTypeEq | WTypeOf |
   WBoolEq | WBoolNot |
   WCharEq | WCharToNum |
   WNumAdd | WNumDiv | WNumGT | WNumGTE | WNumEq | WNumLT | WNumLTE | WNumMult | WNumSub | WNumToCharList |
   WSymEq | WSymToCharList |
   WListCons | WListHead | WListIsEmpty | WListMake | WListTail |
   WDictContains | WDictGet | WDictInsert | WDictMake | WDictRemove | WDictSize | WDictToList |
   WTrace | WError
  deriving (Show, Ord, Eq)
 
data BuiltinType = BoolType | CharType | DictType | FuncType | ListType | NumType | SymType | TypeType
  deriving (Show, Ord, Eq)

simpleEval :: W -> IO W
simpleEval w = eval w DM.empty

eval :: W -> HostDict -> IO W

eval sym@(WSym _) env = return $ env DM.! sym

eval (WList []) _ = error "Can't evaluate nothing"

eval (WList (rawFn:rawArgs)) env =
   do
      fn <- eval rawFn env
      case fn of
         WVauRun capEnv argS envS capCode -> eval capCode newEnv
           where newEnv = DM.insert (WSym envS) (WDict env) $ DM.insert (WSym argS) (WList rawArgs) capEnv
         WEval ->
            evc (\[newForm, WDict newEnv] -> eval newForm newEnv)
         WIf ->
            let [cond,trueCase,falseCase] = rawArgs in
               do r <- eval cond env
                  if isTrue r then eval trueCase env else eval falseCase env
         WParse -> error "TODO:..."
         WQuote ->
            ev (\[x] -> x)
         WSequence ->
            ev (\[_, y] -> y)
         WReadFile ->
            evc (\[WList file] -> fmap strToCharList (readFile $ mkString file))
         WVau ->
            ev (\[WSym argS, WSym envS, code] -> WVauRun { capturedEnv = env,  bindArgSym = argS, bindEnvSym = envS, capturedExpression = code })
         WTypeEq ->
            ev (\[WType x, WType y] -> WBool $ x == y)
         WTypeOf ->
            ev (\[x] -> WType $ typeof x)
         WBoolEq ->
            ev (\[WBool x, WBool y] -> WBool $ x == y)
         WBoolNot ->
            ev (\[WBool x] -> WBool $ not x)
         WCharEq ->
            ev (\[WChar x, WChar y] -> WBool $ x == y)
         WCharToNum ->
            ev (\[WChar x] -> WNum $ toInteger $ fromEnum x)
         WNumAdd ->
            ev (\[WNum x, WNum y] -> WNum $ x + y)
         WNumDiv ->
            ev (\[WNum x, WNum y] -> WNum $ x `div` y)
         WNumGT ->
            ev (\[WNum x, WNum y] -> WBool $ x > y)
         WNumGTE ->
            ev (\[WNum x, WNum y] -> WBool $ x >= y)
         WNumEq ->
            ev (\[WNum x, WNum y] -> WBool $ x == y)
         WNumLT ->
            ev (\[WNum x, WNum y] -> WBool $ x < y)
         WNumLTE ->
            ev (\[WNum x, WNum y] -> WBool $ x <= y)
         WNumMult ->
            ev (\[WNum x, WNum y] -> WNum $ x * y)
         WNumSub ->
            ev (\[WNum x, WNum y] -> WNum $ x - y)
         WNumToCharList ->
            ev (\[WNum x] -> strToCharList $ show x)
         WSymEq ->
            ev (\[WSym x, WSym y] -> WBool $ x == y)
         WSymToCharList ->
            ev (\[WSym x] -> strToCharList x)
         WListCons ->
            ev (\[WList l, e] -> WList $ e : l)
         WListHead ->
            ev (\[WList l] -> head l)
         WListIsEmpty ->
            ev (\[WList l] -> WBool $ null l)
         WListMake ->
            ev (\elems -> WList $ elems)
         WListTail ->
            ev (\[WList l] -> WList $ tail l)
         WDictGet ->
            ev (\[WDict d, k] -> d DM.! k)
         WDictContains ->
            ev (\[WDict d, k] -> WBool $ DM.member k d)
         WDictInsert ->
            ev (\[WDict d, k, v] -> WDict $ DM.insert k v d)
         WDictMake ->
            ev (\pairs -> WDict $ DM.fromList $ map mkKV pairs)
               where mkKV (WList [k, v]) = (k,v) 
                     mkKV x = error $ "Can't make a kv from: " ++ (show x)
         WDictRemove ->
            ev (\[WDict dict, k] -> WDict $ DM.delete k dict)
         WDictSize ->
            ev (\[WDict dict] -> WNum $ toInteger $ DM.size dict)
         WDictToList ->
            ev (\[WDict dict] -> WList $ map (\(x,y) -> WList [x,y]) (DM.toList dict))
         WTrace ->
            evc (\[WList message, x] -> (putStrLn $ mkString message) >> return x)
         WError ->
            ev (\[WList message] -> error $ mkString message)
         x@(WChar _) -> niceError $ show x
         x@(WNum _) -> niceError $ show x
         x@(WList _) -> niceError $ show x
         x@(WDict _) -> niceError $ show x
         x@(WSym _) -> niceError $ show x
         x@(WType _) -> niceError $ show x
         x@(WBool _) -> niceError $ show x      
  where
     isTrue :: W -> Bool
     isTrue w = case w of
       WBool x -> x
       x -> error $ (show x) ++ " isn't a boolean"
     mkString :: [W] -> String
     mkString [] = "" 
     mkString (x:xs) = case x of
       WChar c -> c : (mkString xs)
       u -> error $ "Can't make a string, when found a: " ++ (show u)
     strToCharList :: String -> W
     strToCharList x = WList $ map WChar x
     ev :: ([W] -> W) -> IO W
     ev f = evc (return . f)  
     evc :: ([W] -> IO W) -> IO W
     evc f = (sequence $ map (\x -> eval x env) rawArgs) >>= f
     niceError s = error $ "Can't evalute: " ++ s ++ " as a function. Raw function was: " ++ (show rawFn) ++ " with args " ++ (show rawArgs) 
     typeof :: W -> BuiltinType
     typeof w = case w of
         WChar _ -> CharType
         WNum _ -> NumType
         WList _ -> ListType
         WDict _ -> DictType
         WSym _ -> SymType
         WBool _ -> BoolType
         WType _ -> TypeType
         _ -> FuncType
eval x _ = return $ x