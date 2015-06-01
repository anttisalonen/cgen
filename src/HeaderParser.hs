{-# LANGUAGE
    FlexibleContexts
  #-}

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
  _ <- many eos
  w <- gettype
  case w of
    "namespace" -> namespace (many1 oneobj)
    "class"     -> classDecl
    "struct"    -> try structDecl <|> varFunDecl "struct"
    "typedef"   -> typedef
    "enum"      -> enum
    "extern"    -> extern
    "using"     -> using
    _           -> macro w <|> varFunDecl w

extern :: CharParser HeaderState Object
extern = do
    spaces
    en <- quoted
    spaces
    objs <- try manyext <|> (do o <- oneobj; return [o])
    return $ ExternDecl en objs

  where manyext = do
            _ <- char '{'
            os <- many oneobj
            spaces
            _ <- char '}'
            spaces
            return os

using :: CharParser HeaderState Object
using = do
    _ <- many1 whitespace
    (n, iden) <- try (do
                   _ <- string "namespace"
                   _ <- many1 whitespace
                   i <- identifier
                   return (True, i))
                 <|> (do
                   i <- typename
                   return (False, i))
    spaces
    _ <- eos
    return $ Using n iden

-- macro is a hack.
macro w = do
  _ <- try (char '(' >> spaces >> optional (paramDecl Nothing) >> spaces >> char ')')
  spaces
  _ <- optional (eos)
  return $ VarDecl (ParamDecl w "macro" Nothing Nothing) Nothing

enum :: CharParser HeaderState Object
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
    _ <- eos
    spaces
    cls <- classstack <$> getState
    return $ EnumDef n vals cls

enumVal = do
    spaces
    ev <- identifier
    spaces
    val <- optionMaybe (char '=' >> spaces >> many1 valuechar)
    spaces
    return $ EnumVal ev val

typedef :: CharParser HeaderState Object
typedef = do
    allchars <- many1 typedefchar
    spaces
    _ <- eos
    let ns = words allchars
    return $ TypeDef (intercalate " " (init ns), last ns)

gettype :: CharParser u String
gettype = concat <$> many1 (typename <|> templ)

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

valuechars = typechars ++ ".-"

typename = many1 typechar

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
    ns <- namespacestack <$> getState
    cs <- classstack <$> getState
    (eos >> return (ClassDecl n inherits cs ns [])) <|> clconts cs ns n inherits lev

clconts cs ns n inherits lev = do
    _ <- char '{'
    spaces
    pushClass (lev, n)
    ret <- clcontents
    popClass
    spaces
    _ <- char '}'
    spaces
    _ <- eos
    spaces
    return $ ClassDecl n inherits cs ns ret

clcontents :: CharParser HeaderState [(InheritLevel, Object)]
clcontents = do
  spaces
  objs <- many $ do
    spaces
    mparse <- optionMaybe (many1 (try setinheritlevel <|> try frienddecl))
    case mparse of
      Just _  -> return Nothing -- ignore friend declarations
      Nothing -> do
        obj <- try specialClassFunction <|> oneobj
        vis <- getVisibility <$> getState
        let vn = case vis of
                   Nothing     -> Public
                   Just (v, _) -> v
        return $ Just (vn, obj)
  return $ catMaybes objs

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
    _ <- eos
    spaces
    return ()

-- end of statement
eos :: CharParser u ()
eos = char ';' >> spaces >> many eos >> return ()

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

identList = many1 ((concat <$> (many1 (try opr <|> gettype))) >>= \t -> spaces >> return t) <?> "type"

opr = do
    st <- string "operator"
    spaces
    vl <- oprchars
    return $ st ++ vl

oprchars :: CharParser u String
oprchars = try ((string"()") <|> many1 (oneOf "!+-=/*.-><[]|&"))

getassign = do
    v1 <- getvalue
    spaces
    v2 <- try (char '(' >> spaces >> char ')' >> return "()")
             <|> option "" (do
                   os <- concat <$> many1 oprchars
                   spaces
                   nm <- getassign
                   return $ os ++ nm)
    return $ v1 ++ v2

varFunDecl :: String -> CharParser HeaderState Object
varFunDecl ft = do
  _ <- many1 whitespace
  is <- identList
  spaces
  let alls = (ft:is)
      ptrs = length $ takeWhile (== '*') (last alls)
      nm = drop ptrs $ last alls
      ns = intercalate " " (init alls) ++ replicate ptrs '*'
  vis <- getVisibility <$> getState
  if isOperator nm
    then funDecl nm ns
    else do
      pdecl <- paramDecl (Just alls)
      (optional (char '=' >> spaces >> getassign) >> spaces >>
            (eos <|> (char ',' >> untilEOS >> return ())) >> -- just ignore all other declarations
             spaces >> return (VarDecl pdecl vis))
        <|>
        funDecl nm ns

isOperator :: String -> Bool
isOperator "operator" = True
isOperator t          = takeWhile isAlpha (drop 2 (dropWhile (/= ':') t)) == "operator"

getVisibility :: HeaderState -> Maybe (InheritLevel, String)
getVisibility h =
  let cs = classstack h
  in case cs of
       (c:_) -> Just c
       _     -> Nothing

funDecl :: String -> String -> CharParser HeaderState Object
funDecl fn ft = do
    n <- if isOperator fn
           then spaces >> option "" oprchars
           else return ""
    spaces
    _ <- char '(' <?> "start of function parameter list: ("
    spaces
    pars <- (try (spaces >> string "void" >> spaces >> return [])) <|> sepBy (paramDecl Nothing) (char ',' >> spaces)
    _ <- char ')' <?> "end of function parameter list: )"
    spaces
    constfun <- option False (try (string "const" >> spaces) >> return True)
    optional (many (identifier >> spaces))
    -- constructing member variables
    optional (char ':' >> many (many1 (digit <|> oneOf ("(.-+)[]" ++ typedefchars)) >> spaces))
    abstr <- (char '{' >> ignoreBraces >> skipMany eos >> return False) <|>
             (eos >> return False) <|>
             (char '=' >> spaces >> char '0' >>
              spaces >> eos >> return True)
    spaces
    ns <- namespacestack <$> getState
    vs <- getVisibility <$> getState
    return $ FunDecl (fn ++ n) ft pars ns vs constfun abstr

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
      Nothing -> many1 (refMark <|> ptrStar <|> (gettype >>= \n -> spaces >> return n))
      Just v  -> return v
    spaces
    val <- optionMaybe optionalParams
    arr <- optionMaybe (concat <$> many1 (between (char '[') (char ']') (many (noneOf "]")) >>= \v -> spaces >> return v))
    if '*' `elem` last pts
      -- pointer as "variable name" => no variable name given
      then return $ ParamDecl "" (intercalate " " pts) val arr
      else return $ ParamDecl (last pts) (intercalate " " (init pts)) val arr

optionalParams = do
  _ <- char '='
  spaces
  v <- getassign
  spaces
  r <- option "" (string "(" >> spaces >> string ")" >> return "()")
  spaces
  return (v ++ r)

refMark :: CharParser u String
refMark = do
  c <- char '&'
  spaces
  return [c]

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

untilEOS :: CharParser u String
untilEOS = manyTill (anyChar) (eof <|> try (char ';' >> return ()))

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
