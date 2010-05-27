module Main
where

import System.IO
import Data.List
import Control.Applicative hiding (many, (<|>), optional)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

type Type = String

data VarDecl = VarDecl {
    vartype :: Type
  , varname :: String
  }
  deriving (Eq, Read, Show)

data Object = FunDecl {
    funname     :: String
  , rettype     :: Type
  , params      :: [VarDecl]
  , fnnamespace :: [String]
  }
            | Namespace String [Object]
            | TypeDef (String, String)
  deriving (Eq, Read, Show)

type HeaderState = [String] -- current namespace stack

type Header = [Object]

header :: CharParser HeaderState Header
header = do
  spaces
  many oneobj

oneobj :: CharParser HeaderState Object
oneobj = namespace (many1 oneobj) <|> funDecl

namespace :: CharParser HeaderState [Object] -> CharParser HeaderState Object
namespace nscont = do
    _ <- string "namespace"
    _ <- many1 space
    n <- option "" simpleWord
    spaces
    _ <- char '{'
    spaces
    updateState (n:)
    ret <- nscont
    updateState tail
    spaces
    _ <- char '}'
    spaces
    return $ Namespace n ret

funDecl = do
    ft <- simpleWord <?> "function return type"
    _ <- many1 space
    fn <- simpleWord <?> "function name"
    spaces
    _ <- char '(' <?> "start of function parameter list: ("
    spaces
    pars <- (try (spaces >> string "void" >> spaces >> return [])) <|> sepBy varDecl (char ',' >> spaces)
    _ <- char ')' <?> "end of function parameter list: )"
    _ <- char ';'
    spaces
    ns <- getState
    return $ FunDecl fn ft pars ns

varDecl = do
    pts <- many1 (ptrStar <|> (simpleWord >>= \n -> spaces >> return n))
    spaces
    return $ VarDecl (intercalate " " (init pts)) (last pts)

ptrStar = do
  ns <- many1 $ char '*'
  spaces
  return ns

simpleWord = do
  n <- many1 alphaNum
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
    _ <- many newline
    spaces
    _ <- char '#'
    _ <- untilEOL
    spaces
    return ""

macroDef :: CharParser (M.Map String String) String
macroDef = do
    _ <- many newline
    spaces
    _ <- string "#define "
    mname <- many alphaNum
    spaces
    _ <- newline
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
  _ <- string "/*"
  manyTill anyChar (try (string "*/"))

lineComment :: CharParser u String
lineComment = do -- between (string "//") newline (many anyToken)
  _ <- string "//"
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
      case runParser preprocess M.empty "preprocessor" inp of
        Left  err -> putStrLn $ "Could not preprocess: " ++ show err
        Right prp -> do
          print $ runParser header [] "Header" prp

