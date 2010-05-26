module Main
where

import System.IO
import Data.List
import qualified Control.Monad.State as State
import Control.Applicative hiding (many, (<|>), optional)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

data FunDecl = FunDecl {
    funname :: String
  , rettype :: Type
  , params  :: [VarDecl]
  }
  deriving (Eq, Read, Show)

type Type = String

data VarDecl = VarDecl {
    vartype :: Type
  , varname :: String
  }
  deriving (Eq, Read, Show)

header = many funDecl

funDecl = do
    spaces
    ft <- many1 letter <?> "function return type"
    many1 space
    fn <- many1 letter <?> "function name"
    spaces
    char '('
    spaces
    pars <- sepBy varDecl (char ',' >> spaces)
    char ')'
    char ';'
    spaces
    return $ FunDecl fn ft pars

varDecl = do
    pts <- many1 (ptrStar <|> simpleWord)
    spaces
    return $ VarDecl (intercalate " " (init pts)) (last pts)

ptrStar = do
  ns <- many1 $ char '*'
  spaces
  return ns

simpleWord = do
  n <- many1 alphaNum
  spaces
  return n

untilEOL :: CharParser u String
untilEOL = many $ noneOf "\n"

escapedEOL :: CharParser u Char
escapedEOL = char '\\' >> newline

preprocess :: CharParser (M.Map String String) String
preprocess = do
  ns <- many $ many1 (space <|> newline) <|> preprocessorLine <|> otherLine
  return $ concat ns

preprocessorLine = try macroDef <|> otherMacro

otherMacro = do
    many newline
    spaces
    char '#'
    untilEOL
    spaces
    return ""

macroDef :: CharParser (M.Map String String) String
macroDef = do
    many newline
    spaces
    string "#define "
    mname <- many alphaNum
    spaces
    newline
    mval <- untilEOL
    updateState (M.insert mname mval)
    return ""

{-
ifndef = do
    spaces
    string "#ifndef "
    mname <- many alphaNum
    ms <- getState
    if member mname ms
      then return ""
      else return ""
-}

otherLine :: CharParser (M.Map String String) String
otherLine = do
  ls <- many1 $ try expandMacro
  return $ concat ls

expandMacro = do
    ns <- many $ noneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "#"
    mexp <- expandWord
    ns2 <- many $ noneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "#"
    return $ ns ++ mexp ++ ns2

expandWord = do
    wd <- many1 alphaNum
    ms <- getState
    return $ case M.lookup wd ms of
      Nothing -> wd
      Just v  -> v

removeComments :: CharParser () String
removeComments = do
   optional getComment
   many getCode
 where getCode = do
         n <- anyChar
         optional getComment
         return n

getComment :: CharParser () String
getComment = concat <$> many1 (try blockComment <|> lineComment)

blockComment :: CharParser u String
blockComment = do -- between (string "/*") (string "*/") (many anyToken)
  string "/*"
  manyTill anyChar (try (string "*/"))

lineComment :: CharParser u String
lineComment = do -- between (string "//") newline (many anyToken)
  string "//"
  manyTill anyChar (try newline)

main :: IO ()
main = do 
  input <- hGetContents stdin
  completeParse input

completeParse :: String -> IO ()
completeParse input = do
  case parse removeComments "removeComments" input of
    Left  err -> putStrLn $ "Could not remove comments: " ++ show err
    Right inp -> do
      putStrLn inp
      case runParser preprocess M.empty "preprocessor" inp of
        Left  err -> putStrLn $ "Could not preprocess: " ++ show err
        Right prp -> do
          putStrLn prp
          print $ parse header "Header" prp

completeParser :: String -> Either ParseError [FunDecl]
completeParser input =
  case parse removeComments "removeComments" input of
    Left  err -> Left err
    Right inp -> 
      case runParser preprocess M.empty "preprocessor" inp of
        Left  err -> Left err
        Right prp -> parse header "Header" prp

