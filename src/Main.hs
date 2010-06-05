{-# LANGUAGE TemplateHaskell #-}
module Main()
where

import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Data.Either
import Data.List
import Control.Monad

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
publicMemberFunction (FunDecl n _ _ _ (Just (Public, _)) _) = case n of
  ('_':_) -> False
  _       -> True
publicMemberFunction _                                      = False

data Options = Options
  {
    outputdir  :: FilePath
  , inputfiles :: [FilePath]
  }
$(deriveMods ''Options)

defaultOptions :: Options
defaultOptions = Options "" []

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"] (ReqArg (setOutputdir) "DIRECTORY") "output directory for the C files"
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
      handleParses (outputdir opts) $ zip (map takeBaseName args) (concat press)

handleParses :: FilePath -> [(FilePath, Object)] -> IO ()
handleParses outdir objs = do
    createDirectoryIfMissing True outdir
    let funs = getFuns (map snd objs)
    return (map getObjName $ filter publicMemberFunction $ funs) >>= print
    exitWith ExitSuccess

