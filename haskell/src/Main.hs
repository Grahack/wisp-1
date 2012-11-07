{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Control.Applicative                (Applicative, pure, (*>),
                                                     (<$), (<$>), (<*>))
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char as PC
import Atom
import qualified Data.Map as DM

 
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
            Right a -> do r <- simpleEval a
                          putStrLn (show r)
