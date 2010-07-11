{-# LANGUAGE TemplateHaskell #-}
module Main
where

import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Either
import Data.List
import Control.Monad
import Control.Monad.State
import Text.Printf

import Text.Regex.Posix

import HeaderData
import DeriveMod
import HeaderParser
import Options

data Options = Options
  {
    outputfile        :: FilePath
  , interfacefile     :: String
  , excludepatterns   :: [String] 
  }
  deriving (Show)
$(deriveMods ''Options)

defaultOptions :: Options
defaultOptions = Options "" "" []

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"]        (ReqArg (setOutputfile) "file")                       "output file for the graph"
  , Option []    ["interface"]     (ReqArg (setInterfacefile) "file")                    "define input interface file"
  , Option []    ["exclude"]       (ReqArg (\l -> modExcludepatterns (l:)) "expression") "exclude pattern for class names"
  ]

main :: IO ()
main = do 
  args <- getArgs
  let (actions, rest, errs) = getOpt Permute options args
  when (not (null errs) || null rest) $ do
    mapM_ putStrLn errs
    pr <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ pr ++ " <options> <C++ header files>") options
    exitWith (ExitFailure 1)
  let prevopts = foldl' (flip ($)) defaultOptions actions
  opts <- handleInterfaceFile (interfacefile prevopts) None handleOptionsLine prevopts
  contents <- mapM readFile rest
  let parses = map parseHeader contents
      (perrs, press) = partitionEithers parses
  case perrs of
    ((str, err):_) -> do
        putStrLn str
        putStrLn $ "Could not parse: " ++ show err
        exitWith (ExitFailure 1)
    []             -> do
          handleParses (outputfile opts) 
                       (excludepatterns opts) 
                       (concat press)
          exitWith ExitSuccess

handleOptionsLine :: String -> State InterfaceState (Options -> Options)
handleOptionsLine = 
  processor None
           [(Exclude,      \n -> modExcludepatterns (n:))]
           [("@exclude", Exclude)]

data InterfaceState = None | Exclude
  deriving (Eq)

handleParses :: FilePath -> [String] -> [Object] -> IO ()
handleParses outfile excls objs =
    writeFile outfile $ createGraphFile excls objs

createGraphFile :: [String] -> [Object] -> String
createGraphFile excls objs = 
  let incclass n      = not $ or (map (\e -> n =~ e) excls)
      mkline (ClassDecl cname inhs _ _ _) = 
        if incclass cname
          then printf "%s|%s\n" cname (intercalate "," (map inheritname inhs))
          else ""
      mkline _ = ""
  in concatMap mkline $ filter (not . isEmptyClass) $ getClasses objs

