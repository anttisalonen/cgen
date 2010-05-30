module Main()
where

import System.IO
import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative hiding (many, (<|>), optional)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

type Type = String

data ParamDecl = ParamDecl {
    varname   :: String
  , vartype   :: Type
  , varvalue  :: Maybe String
  }
  deriving (Eq, Read, Show)

data Object = FunDecl {
                funname      :: String
              , rettype      :: Type
              , params       :: [ParamDecl]
              , fnnamespace  :: [String]
              , fnvisibility :: Maybe (InheritLevel, String)
              }
            | Namespace String [Object]
            | TypeDef (String, String)
            | ClassDecl {
                classname     :: String
              , classinherits :: [InheritDecl]
              , classobjects  :: [Object]
              }
            | VarDecl ParamDecl (Maybe (InheritLevel, String))
            | EnumDef String [EnumVal]
    deriving (Eq, Read, Show)

data InheritDecl = InheritDecl {
    inheritname  :: String
  , inheritlevel :: InheritLevel
  }
  deriving (Eq, Read, Show)

data InheritLevel = Public | Protected | Private
  deriving (Eq, Read, Show, Enum, Bounded)

data HeaderState = HeaderState {
    namespacestack :: [String]
  , classstack     :: [(InheritLevel, String)]
  }
  deriving (Eq, Read, Show)

data EnumVal = EnumVal {
    enumvaluename :: String
  , enumvalue     :: Maybe Int
  }
  deriving (Eq, Read, Show)

pushNamespace n = 
  updateState (\h -> h{namespacestack = n:(namespacestack h)})

popNamespace =
  updateState (\h -> h{namespacestack = tail (namespacestack h)})

pushClass n = 
  updateState (\h -> h{classstack = n:(classstack h)})

popClass =
  updateState (\h -> h{classstack = tail (classstack h)})

setLevel l = do
  n <- classstack <$> getState
  case n of
    ((_, c):ms) -> do
       let cn = ((l,c):ms)
       updateState (\h -> h{classstack = cn})
    _      -> return ()

type Header = [Object]

header :: CharParser HeaderState Header
header = many oneobj

oneobj :: CharParser HeaderState Object
oneobj = do
  spaces
  w <- gettype
  case w of
    "namespace" -> namespace (many1 oneobj)
    "class"     -> classDecl 
    "typedef"   -> typedef 
    "enum"      -> enum
    _           -> varFunDecl w

enum = do
    _ <- many1 space
    n <- identifier
    spaces
    _ <- char '{'
    vals <- sepBy1 enumVal (char ',')
    spaces
    optional (char ',')
    spaces
    _ <- char '}'
    spaces
    _ <- char ';'
    spaces
    return $ EnumDef n vals

enumVal = do
    spaces
    ev <- identifier
    spaces
    val <- optionMaybe (char '=' >> spaces >> many1 digit >>= return . read)
    spaces
    return $ EnumVal ev val

typedef :: CharParser HeaderState Object
typedef = do
    allchars <- many1 typedefchar
    spaces
    _ <- char ';'
    let ns = words allchars
    return $ TypeDef (intercalate " " (init ns), last ns)

gettype = many1 typechar

typechar = oneOf (idChar ++ "*:<>&")

typechars = idChar ++ "*:<>&"

typedefchar = oneOf (typechars ++ " \t")

classDecl = do
    _ <- many1 space
    n <- identifier
    spaces
    inherits <- option [] inheritDecls
    spaces
    _ <- char '{'
    spaces
    pushClass (Private, n)
    ret <- clcontents
    popClass
    spaces
    _ <- char '}'
    spaces
    _ <- char ';'
    spaces
    return $ ClassDecl n inherits ret

clcontents :: CharParser HeaderState [Object]
clcontents = spaces >> many (optional setinheritlevel >> (try specialClassFunction <|> oneobj))

-- constructor or destructor.
specialClassFunction = do
  spaces
  cname <- (snd . head . classstack) <$> getState
  fn <- ((string ('~' : cname)) <|> string cname)
  funDecl fn ""

inheritDecls = char ':' >> spaces >> sepBy inh (char ',')
  where inh = do
          spaces
          l <- inheritl
          spaces
          n <- many1 typedefchar
          spaces
          return $ InheritDecl n l

inheritance = do
    _ <- char ':'
    spaces
    inheritl

capitalize [] = []
capitalize (x:xs) = toUpper x : xs

inheritl = do 
    spaces
    try (string "public" >> return Public) <|> try (string "protected" >> return Protected) <|> (string "private" >> return Private)

setinheritlevel = do
    spaces
    str <- try (string "public") <|> try (string "protected") <|> string "private" 
    spaces
    _ <- char ':'
    spaces
    setLevel $ case str of
               "public"    -> Public
               "protected" -> Protected
               _           -> Private

namespace :: CharParser HeaderState [Object] -> CharParser HeaderState Object
namespace nscont = do
    _ <- many1 space
    n <- option "" identifier
    spaces
    _ <- char '{'
    spaces
    pushNamespace n
    ret <- nscont
    popNamespace
    spaces
    _ <- char '}'
    spaces
    return $ Namespace n ret

identList = sepBy1 gettype (many1 space) <?> "type"

varFunDecl :: String -> CharParser HeaderState Object
varFunDecl ft = do
  _ <- many1 space
  is <- identList
  spaces
  let alls = (ft:is)
      nm = last alls
      ns = intercalate " " (init alls)
  vis <- getVisibility <$> getState
  (char ';' >> spaces >> return (VarDecl (ParamDecl nm ns Nothing) vis))
    <|>
    funDecl nm ns

getVisibility :: HeaderState -> Maybe (InheritLevel, String)
getVisibility h = 
  let cs = classstack h
  in case cs of
       (c:_) -> Just c
       _     -> Nothing

funDecl :: String -> String -> CharParser HeaderState Object
funDecl fn ft = do
    spaces
    _ <- char '(' <?> "start of function parameter list: ("
    spaces
    pars <- (try (spaces >> string "void" >> spaces >> return [])) <|> sepBy varDecl (char ',' >> spaces)
    _ <- char ')' <?> "end of function parameter list: )"
    spaces
    optional (many (identifier >> spaces))
    _ <- string ";" <|> (char '{' >> manyTill anyChar (char '}'))
    spaces
    ns <- namespacestack <$> getState
    vs <- getVisibility <$> getState
    return $ FunDecl fn ft pars ns vs

varDecl = do
    pts <- many1 (ptrStar <|> (gettype >>= \n -> spaces >> return n))
    spaces
    val <- optionMaybe (char '=' >> spaces >> many1 typedefchar)
    return $ ParamDecl (last pts) (intercalate " " (init pts)) val

ptrStar :: CharParser u String
ptrStar = do
  ns <- many1 $ char '*'
  spaces
  return ns

idCharInit = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
idChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

identifier :: CharParser u String
identifier = do
  m <- oneOf idCharInit
  n <- many $ oneOf idChar
  return (m:n)

untilEOL :: CharParser u String
untilEOL = manyTill (anyChar) (eof <|> try (char '\n' >> return ()))

escapedEOL :: CharParser u Char
escapedEOL = char '\\' >> newline

preprocess :: CharParser (M.Map String String) String
preprocess = do
  spaces
  concat <$> many (spaces >> ((char '#' >> preprocessorLine) <|> otherLine))

preprocessorLine = do
  spaces
  n <- (string "define" >> macroDef) <|> otherMacro
  spaces
  return n

otherMacro = untilEOL >> return ""

macroDef :: CharParser (M.Map String String) String
macroDef = do
    spaces
    mname <- identifier
    mval <- option "" (many1 (oneOf " \t") >> untilEOL)
    _ <- char '\n'
    updateState (M.insert mname mval)
    return ""

otherLine :: CharParser (M.Map String String) String
otherLine = do
  n <- concat <$> many1 expandMacro
  spaces
  return n

expandMacro = do
    ns <- many $ noneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "#"
    mexp <- try expandWord
    ns2 <- many $ noneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "#"
    return $ ns ++ mexp ++ ns2

expandWord = do
    wd <- many1 alphaNum
    ms <- getState
    return $ fromMaybe wd (M.lookup wd ms)

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
          putStrLn prp
          print $ runParser header (HeaderState [] []) "Header" prp

