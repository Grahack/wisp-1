{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Control.Applicative                (Applicative, pure, (*>),
                                                     (<$), (<$>), (<*>))
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char as PC
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
         WParse ->
            ev (\[WList str] -> let file = mkString str in doParse file)
              where
                doParse :: String -> W
                doParse file = case parse file_parser "WParse" file of
                                 Left l -> error $ "Parse Error: " ++ (show l)
                                 Right r -> WList r
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

atom :: Parser W
atom =
   do a <- (   list_parser
           <|> literal_vector
           <|> literal_dict
           <|> literal_string
           <|> literal_char
           <|> num_parser
           <|> literal_symbol
           <|> builtin_symbol)
      r <- optionMaybe (char '.' *> atom)
      return $
         case r of
            Nothing -> a
            Just v -> (WList [a, v])     
  <?> "atom"

file_parser :: Parser [W]
file_parser = do _ <- many blank_line
                 topLevels <- sepBy (line_parser 0) (many blank_line)
                 _ <- many blank_line
                 _ <- eof
                 return topLevels

line_parser :: Int -> Parser W
line_parser depth = 
  do _ <- count depth (char '\t')
     firstLine <- sepBy1 atom (char ' ')
     (skipMany1 blank_line) <|> eof
     rest <- many $ try $ line_parser (depth+1)
     return $
       if length firstLine == 1 && length rest == 0 then
         head firstLine
       else
         WList $ firstLine ++ rest
  <?> "line"

blank_line :: Parser ()
blank_line =  try(
                many (char ' ' <|> char '\t')
             *> optional coment
             *> char '\n'
             *> return ())
           <?> "blank line"
   where
      coment = char ';' *> skipMany (satisfy (\x -> x /= '\n'))

literal_char :: Parser W
literal_char = (WChar <$> (normal_char <|> escape_char)) <?> "character"
   where
      normal_char = char '~' *> (noneOf "\n\\\t ") <?> "normal character"

literal_symbol :: Parser W
literal_symbol = WSym <$> many1 (plain_char <|> escape_char) <?> "symbol"
   where
      plain_char = noneOf "\r\\\t\n ();#.{}" <?> "plain char"

escape_char :: Parser Char
escape_char = char '\\' >> (
   choice $ escape_pair <$> [("alarm", '\a')
                   ,("backspace", '?')
                   ,("backslash", '\\')
                   ,("double-quote", '"')
                   ,("newline", '\n')
                   ,("space", ' ')
                   ,("single-quote", '\'')
                   ,("formfeed", '?')
                   ,("\"", '"')
                   ,("return", '?')
                   ,("tab", '\t')])
   where
     escape_pair :: (String, Char) -> Parser Char
     escape_pair (s, c) = c <$ try (string s)

builtin_symbol :: Parser W
builtin_symbol = do _ <- char '#'
                    builtin_pairs reader_symbols
   where
      builtin_pairs :: [(String, W)] -> Parser W
      builtin_pairs l = choice $ uncurry builtin_pair <$> l
      reader_symbols :: [(String, W)]
      reader_symbols =  [("True", WBool True)
                        ,("False", WBool False)
                        ,("eval", WEval)
                        ,("if", WIf)
                        ,("quote", WQuote)
                        ,("sequence", WSequence)
                        ,("parse", WParse)
                        ,("read-file", WReadFile)
                        ,("vau", WVau)
                        ,("type-eq", WTypeEq)
                        ,("type-of", WTypeOf)
                        ,("bool-not", WBoolNot)
                        ,("bool-eq", WBoolEq)
                        ,("char-eq", WCharEq)
                        ,("char-to-num", WCharToNum)
                        ,("num-add", WNumAdd)
                        ,("num-div", WNumDiv)
                        ,("num-gt", WNumGT)
                        ,("num-gte", WNumGTE)
                        ,("num-eq", WNumEq)
                        ,("num-lt", WNumLT)
                        ,("num-lte", WNumLTE)
                        ,("num-mult", WNumMult)
                        ,("num-sub", WNumSub)
                        ,("num-to-char-list", WNumToCharList)
                        ,("list-cons", WListCons)
                        ,("list-head", WListHead)
                        ,("list-is-empty", WListIsEmpty)
                        ,("list-make", WListMake)
                        ,("list-tail", WListTail)
                        ,("dict-contains", WDictContains)
                        ,("dict-get", WDictGet)
                        ,("dict-insert", WDictInsert)
                        ,("dict-make", WDictMake)
                        ,("dict-size", WDictSize)
                        ,("dict-to-list", WDictToList)
                        ,("trace", WTrace)
                        ,("error", WError)]
      builtin_pair :: String -> W -> Parser W
      builtin_pair symbol wvalue = wvalue <$ try (string symbol)

literal_string :: Parser W
literal_string = do _ <- char '"'
                    r <- many wchar
                    _ <- char '"'
                    return $ WList (WListMake : r)
   <?> "quoted string"
   where
      wchar = WChar <$> (plain_char <|> escape_char)
      plain_char = noneOf "\r\n\\\""

literal_vector :: Parser W
literal_vector = do _ <- char '['
                    r <- sepBy atom space
                    _ <- char ']'
                    return $ WList (WListMake:r)

literal_dict :: Parser W
literal_dict = do _ <- char '{'
                  _ <- optional space
                  kvs <- sepBy dict_pair (optional space)
                  _ <- optional space
                  _ <- char '}'
                  return $ WDict (DM.fromList kvs)
   where
      dict_pair = do _ <- char '['
                     k <- atom
                     _ <- space
                     v <- atom
                     _ <- char ']'
                     return (k,v)

list_parser :: Parser W
list_parser = do
    _ <- char '('
    r <- sepBy atom space
    _ <- char ')'
    return $ WList r
  <?> "list"

num_parser :: Parser W
num_parser = WNum <$> (option id (char '-' *> pure negate) <*> natural) <?> "decimal integer"
  where
     natural :: Parser Integer
     natural = foldl (\ a b -> a * 10 + (toInteger b)) 0
        <$> many1 ((\ c -> fromEnum c - fromEnum '0') <$> PC.digit)
        <?> "nonnegative decimal integer"


main :: IO ()
main =
    do c <- getContents
       case parse (line_parser 0) "(stdin)" c of
            Left e  -> do putStrLn "Error parsing input:"
                          print e
            Right a -> do r <- eval a DM.empty
                          putStrLn (show r)
