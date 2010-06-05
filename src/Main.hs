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
        forM_ funs $ \fun -> do
            let exclude = or $ map (\e -> funname fun =~ e) excls
            when (not exclude) $ do
                hPrintf h "%s %s(%s);\n" (rettype fun) (funname fun) (paramFormat (params fun))
        hPrintf h "\n"
        hPrintf h "}\n"
        hPrintf h "\n"
        hPrintf h "#endif\n"
        hPutStrLn stderr $ "Wrote file " ++ outfile

  where outfile    = (outdir </> headername)
        funs       = filter publicMemberFunction $ getFuns objs
        namespaces = filter (not . null) $ nub $ map (headDef "") (map fnnamespace funs)
        classes    = filter (not . null) $ nub $ map getClname (map fnvisibility funs)
        getClname Nothing       = ""
        getClname (Just (_, n)) = n

