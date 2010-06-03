module HeaderParser(parseHeader)
where

import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative hiding (many, (<|>), optional)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

import HeaderData

data HeaderState = HeaderState {
    namespacestack :: [String]
  , classstack     :: [(InheritLevel, String)]
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

header :: CharParser HeaderState Header
header = many oneobj

oneobj :: CharParser HeaderState Object
oneobj = do
  spaces
  w <- gettype
  case w of
    "namespace" -> namespace (many1 oneobj)
    "class"     -> classDecl 
    "struct"    -> structDecl
    "typedef"   -> typedef 
    "enum"      -> enum
    _           -> macro w <|> varFunDecl w

-- macro is a hack.
macro w = do
  _ <- try (char '(' >> spaces >> optional (paramDecl Nothing) >> spaces >> char ')')
  spaces
  _ <- optional (char ';')
  return $ VarDecl (ParamDecl w "macro" Nothing Nothing) Nothing

enum = do
    _ <- many1 whitespace
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

gettype :: CharParser u String
gettype = concat <$> many1 (many1 typechar <|> templ)

templ = do
  _ <- char '<'
  v <- manyTill anyChar (char '>')
  return ('<' : (v ++ ">"))

getvalue :: CharParser u String
getvalue = quoted <|> many1 valuechar

quoted :: CharParser u String
quoted = do
  _ <- char '"'
  v <- manyTill anyChar (char '"')
  return ('"' : (v ++ "\""))

valuechar = oneOf valuechars

valuechars = typechars ++ "."

typechar = oneOf typechars

typechars = idChar ++ "*:&"

typedefchar = oneOf typedefchars

typedefchars = typechars ++ " \t,<>\r\n"

structDecl = classDecl' Public

classDecl = classDecl' Private

classDecl' lev = do
    _ <- many1 whitespace
    optional (char '_' >> identifier >> spaces)
    n <- identifier
    spaces
    inherits <- option [] inheritDecls
    spaces
    (char ';' >> return (ClassDecl n inherits [])) <|> clconts n inherits lev

clconts n inherits lev = do
    _ <- char '{'
    spaces
    pushClass (lev, n)
    ret <- clcontents
    popClass
    spaces
    _ <- char '}'
    spaces
    _ <- char ';'
    spaces
    return $ ClassDecl n inherits ret

clcontents :: CharParser HeaderState [Object]
clcontents = do
  spaces
  many $ do
    spaces
    optional (many1 (setinheritlevel <|> frienddecl))
    try specialClassFunction <|> oneobj

-- constructor or destructor.
specialClassFunction = do
  spaces
  optional (string "virtual")
  spaces
  cname <- (snd . head . classstack) <$> getState
  fn <- ((string ('~' : cname)) <|> string cname)
  spaces
  funDecl fn ""

inheritDecls = char ':' >> spaces >> sepBy inh (char ',')
  where inh = do
          spaces
          l <- inheritl
          spaces
          n <- gettype
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

frienddecl = do
    _ <- string "friend"
    spaces
    _ <- string "class"
    spaces
    _ <- identifier
    spaces
    _ <- char ';'
    spaces
    return ()

setinheritlevel = do
    str <- try (string "public") <|> try (string "protected") <|> string "private" 
    spaces
    _ <- char ':'
    spaces
    setLevel $ case str of
               "public"    -> Public
               "protected" -> Protected
               _           -> Private

whitespace = oneOf (" \t\n\r")

namespace :: CharParser HeaderState [Object] -> CharParser HeaderState Object
namespace nscont = do
    _ <- many1 whitespace
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

identList = sepBy1 gettype (many1 whitespace) <?> "type"

varFunDecl :: String -> CharParser HeaderState Object
varFunDecl ft = do
  _ <- many1 whitespace
  is <- identList
  spaces
  let alls = (ft:is)
      nm = last alls
      ns = intercalate " " (init alls)
  vis <- getVisibility <$> getState
  if nm == "operator"
    then funDecl nm ns
    else do
      pdecl <- paramDecl (Just alls)
      (optional (char '=' >> spaces >> getvalue >> spaces) >> 
            char ';' >> spaces >> return (VarDecl pdecl vis))
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
    n <- if fn == "operator"
           then spaces >> option "" (try (string "()") <|> many1 (oneOf "!+-=/*.-><"))
           else return ""
    spaces
    _ <- char '(' <?> "start of function parameter list: ("
    spaces
    pars <- (try (spaces >> string "void" >> spaces >> return [])) <|> sepBy (paramDecl Nothing) (char ',' >> spaces)
    _ <- char ')' <?> "end of function parameter list: )"
    spaces
    optional (many (identifier >> spaces))
    optional (char ':' >> many (many1 (oneOf ("()" ++ typedefchars)) >> spaces))
    abstr <- (char '{' >> ignoreBraces >> return False) <|> 
             (char ';' >> return False) <|> 
             (char '=' >> spaces >> char '0' >> 
              spaces >> char ';' >> return True)
    spaces
    ns <- namespacestack <$> getState
    vs <- getVisibility <$> getState
    return $ FunDecl (fn ++ n) ft pars ns vs abstr

ignoreBraces :: CharParser u ()
ignoreBraces = ignoreBraces' (0 :: Int)
  where ignoreBraces' n = do
          skipMany $ noneOf "{}"
          v <- oneOf "{}"
          case v of
            '}' -> case n of
                     0 -> return ()
                     _ -> ignoreBraces' (n - 1)
            _   -> ignoreBraces' (n + 1)

paramDecl :: Maybe [String] -> CharParser HeaderState ParamDecl
paramDecl mv = do
    pts <- case mv of
      Nothing -> many1 (ptrStar <|> (gettype >>= \n -> spaces >> return n))
      Just v  -> return v
    spaces
    val <- optionMaybe optionalParams 
    arr <- optionMaybe (between (char '[') (char ']') (many (noneOf "]")) >>= \v -> spaces >> return v)
    return $ ParamDecl (last pts) (intercalate " " (init pts)) val arr

optionalParams = do
  _ <- char '='
  spaces 
  v <- getvalue 
  spaces 
  r <- option "" (string "(" >> spaces >> string ")" >> return "()")
  spaces 
  return (v ++ r)

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
blockComment = do
  _ <- try (string "/*")
  manyTill anyChar (try (string "*/"))

eol :: CharParser u ()
eol = 
  (try (string "\r\n") >> return ()) <|> (oneOf "\r\n" >> return ())

lineComment :: CharParser u String
lineComment = do
  _ <- try (string "//")
  manyTill anyChar (try eol)

parseHeader :: String -> Either (String, ParseError) Header
parseHeader input = 
  case parse removeComments "removeComments" input of
    Left  err -> Left (input, err)
    Right inp -> 
      case runParser preprocess M.empty "preprocessor" inp of
        Left  err2 -> Left (inp, err2)
        Right prp  -> case runParser header (HeaderState [] []) "Header" prp of
                        Left err  -> Left (prp, err)
                        Right v   -> Right v

