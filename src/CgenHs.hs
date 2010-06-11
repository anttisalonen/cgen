{-# LANGUAGE TemplateHaskell #-}
module Main
where

import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Data.Either
import Data.List
import Control.Monad
import Control.Monad.State

import HeaderParser
import HaskellGen
import Options
import DeriveMod

data Options = Options
  {
    outputdir         :: FilePath
  , interfacefile     :: String
  , excludepatterns   :: [String] 
  }
  deriving (Show)
$(deriveMods ''Options)

defaultOptions :: Options
defaultOptions = Options "" "" []

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"]        (ReqArg (setOutputdir) "Directory")                  "output directory for the Haskell files"
  , Option []    ["interface"]     (ReqArg (setInterfacefile) "file")                   "define input interface file for Haskell"
  , Option []    ["exclude"]       (ReqArg (\l -> modExcludepatterns (l:)) "Function")  "exclude pattern for function names"
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
  handleHaskell (outputdir opts) (excludepatterns opts) rest
  exitWith ExitSuccess

data InterfaceState = None | Exclude

handleOptionsLine :: String -> State InterfaceState (Options -> Options)
handleOptionsLine l = do
    case l of
      ('#':_)          -> return id            -- comment
      "@exclude"       -> put Exclude >> return id
      ""               -> return id
      ('~':_)          -> put None >> return id
      n -> do
        v <- get
        return $ case v of
          Exclude      -> modExcludepatterns (n:)
          None         -> id

handleHaskell :: FilePath -> [String] -> [FilePath] -> IO ()
handleHaskell hsout excls filenames = do
    gencontents <- mapM readFile filenames
    let genparses = map parseHeader gencontents
        (genperrs, genpress) = partitionEithers genparses
    case genperrs of
      ((str, err):_) -> do
          putStrLn str
          putStrLn $ "Could not parse generated file (!): " ++ show err
          exitWith (ExitFailure 2)
      []             -> do
          createDirectoryIfMissing True hsout
          haskellGen hsout excls $ zip (map takeFileName filenames) genpress
          exitWith ExitSuccess

