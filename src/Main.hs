{-# LANGUAGE TemplateHaskell #-}
module Main()
where

import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath
import System.Directory
import System.IO
import Data.Either
import Data.List
import Data.Char
import Control.Monad
import Text.Printf

import Text.Regex.Posix
import Safe

import HeaderParser
import HeaderData
import DeriveMod

getFuns :: [Object] -> [Object]
getFuns [] = []
getFuns (o:os) = 
  case o of
    (FunDecl _ _ _ _ _ _) -> o : getFuns os
    (Namespace _ os2)     -> getFuns os2 ++ getFuns os
    (ClassDecl _ _ os2)   -> getFuns os2 ++ getFuns os
    _                     -> getFuns os

getObjName :: Object -> String
getObjName (FunDecl n _ _ _ _ _) = n
getObjName (Namespace n _ )      = n
getObjName (TypeDef (n, _))      = n
getObjName (ClassDecl n _ _)     = n
getObjName (VarDecl p _)         = varname p
getObjName (EnumDef n _)         = n

publicMemberFunction :: Object -> Bool
publicMemberFunction (FunDecl _ _ _ _ (Just (Public, _)) _) = True
publicMemberFunction _                                      = False

data Options = Options
  {
    outputdir       :: FilePath
  , inputfiles      :: [FilePath]
  , includefiles    :: [FilePath]
  , excludepatterns :: [String] 
  }
$(deriveMods ''Options)

defaultOptions :: Options
defaultOptions = Options "" [] [] []

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"]  (ReqArg (setOutputdir) "Directory")             "output directory for the C files"
  , Option []    ["header"]  (ReqArg (\l -> modIncludefiles   (l:)) "File")  "file to include in the generated headers"
  , Option []    ["exclude"] (ReqArg (\l -> modExcludepatterns (l:)) "File") "exclude pattern for function names"
  ]

main :: IO ()
main = do 
  args <- getArgs
  let (actions, rest, errs) = getOpt Permute options args
  when (not (null errs)) $ do
    mapM_ print errs
    exitWith (ExitFailure 1)
  let opts = foldl' (flip ($)) defaultOptions actions
  contents <- mapM readFile rest
  let parses = map parseHeader contents
      (perrs, press) = partitionEithers parses
  case perrs of
    ((str, err):_) -> do
        putStrLn str
        putStrLn $ "Could not parse: " ++ show err
        exitWith (ExitFailure 1)
    _              -> do
      handleParses (outputdir opts) (includefiles opts) (excludepatterns opts) $ zip (map takeFileName rest) press

handleParses :: FilePath -> [FilePath] -> [String] -> [(FilePath, [Object])] -> IO ()
handleParses outdir incfiles excls objs = do
    createDirectoryIfMissing True outdir
    mapM_ (uncurry $ handleHeader outdir incfiles excls) objs
    exitWith ExitSuccess

toCapital = map toUpper

showP (ParamDecl pn pt _ _) = pt ++ " " ++ pn

paramFormat (p1:p2:ps) = showP p1 ++ ", " ++ paramFormat (p2:ps)
paramFormat [p1]       = showP p1
paramFormat []         = ""

correctType :: String -> String
correctType t =
  let ns = words t
  in case ns of
       []  -> ""
       ms  -> intercalate " " $ sepChars "*" $ filter isType ms

sepChars :: String -> [String] -> [String]
sepChars st = map (sepChar st)

sepChar :: String -> String -> String
sepChar st (x:y:xs)    = if x /= ' ' && x `notElem` st && y `elem` st
                           then x : ' ' : sepChar st (y:xs)
                           else x : sepChar st (y:xs)
sepChar _  l           = l

isType :: String -> Bool
isType "virtual" = False
isType "static"  = False
isType "const"   = False
isType "mutable" = False
isType "struct"  = False
isType "union"   = False
isType "inline"  = False
isType _         = True

handleHeader :: FilePath -> [FilePath] -> [String] -> FilePath -> [Object] -> IO ()
handleHeader outdir incfiles excls headername objs = 
    withFile outfile WriteMode $ \h -> do
        hPrintf h "#ifndef CGEN_%s_H\n" (toCapital (takeBaseName headername))
        hPrintf h "#define CGEN_%s_H\n" (toCapital (takeBaseName headername))
        hPrintf h "\n"
        forM_ incfiles $ \inc -> do
            hPrintf h "#include <%s>\n" inc
        hPrintf h "\n"
        hPrintf h "extern \"C\"\n"
        hPrintf h "{\n"
        hPrintf h "#ifndef CGEN_OUTPUT_INTERN\n"
        hPrintf h "\n"
        forM_ classes $ \cl -> do
            hPrintf h "struct %s;\n" cl
        hPrintf h "\n"
        hPrintf h "#else\n"
        hPrintf h "\n"
        forM_ namespaces $ \ns -> do
            hPrintf h "using namespace %s;\n" ns
        hPrintf h "\n"
        hPrintf h "#endif\n"
        hPrintf h "\n"
        forM_ (mangle funs) $ \origfun -> do
            let exclude = lastDef ' ' (correctType $ rettype origfun) == '&' || 
                          or (map (\e -> funname origfun =~ e) excls) ||
                          take 8 (funname origfun) == "operator"
                fun     = finalName . extendFunc $ origfun
            when (not exclude) $ hPrintf h "%s %s(%s);\n" (correctType $ rettype fun) (funname fun) (paramFormat (params fun))
        hPrintf h "\n"
        hPrintf h "}\n"
        hPrintf h "\n"
        hPrintf h "#endif\n"
        hPutStrLn stderr $ "Wrote file " ++ outfile

  where outfile    = (outdir </> headername)
        funs       = filter (\f -> publicMemberFunction f && not (abstract f)) (getFuns objs)
        namespaces = filter (not . null) $ nub $ map (headDef "") (map fnnamespace funs)
        classes    = filter (not . null) $ nub $ map getClname funs

getClname :: Object -> String
getClname (FunDecl _ _ _ _ (Just (_, n)) _) = n
getClname _                                 = ""

finalName :: Object -> Object
finalName f@(FunDecl fname _ _ funns _ _) =
  let clname = getClname f
      nsname = headDef "" funns
      updname = nsname ++ (if not (null nsname) then "_" else "") ++ 
                clname ++ (if not (null clname) then "_" else "") ++ fname
  in f{funname = updname}
finalName n = n

extendFunc :: Object -> Object
extendFunc f@(FunDecl fname _ _ _ (Just (_, clname)) _) 
  | fname == clname = f{funname = "new",
                        rettype = fname ++ " *"}
  | fname == '~':clname = f{funname = "delete",
                            rettype = "void"}
  | otherwise           = f
extendFunc n = n

-- o(n).
mangle :: [Object] -> [Object]
mangle []     = []
mangle (n:ns) = 
  let num = length $ filter (== funname n) $ map funname ns
      m   = n{funname = funname n ++ show num}
  in if num == 0
       then n : mangle ns
       else m : mangle ns

