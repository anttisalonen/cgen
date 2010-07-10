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

defaultOptions :: Options
defaultOptions = Options "" "" [] [] [] [] []

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"]        (ReqArg (setOutputdir) "directory")                   "output directory for the Haskell files"
  , Option []    ["interface"]     (ReqArg (setInterfacefile) "file")                    "define input interface file for Haskell"
  , Option []    ["exclude"]       (ReqArg (\l -> modExcludepatterns (l:)) "expression") "exclude pattern for function names"
  ]

main :: IO ()
main = do 
  args <- getArgs
  let (actions, rest, errs) = getOpt Permute options args
  when (not (null errs) || null rest) $ do
    mapM_ putStrLn errs
    pr <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ pr ++ " <options> <C/C++ header files>") options
    exitWith (ExitFailure 1)
  let prevopts = foldl' (flip ($)) defaultOptions actions
  opts <- handleInterfaceFile (interfacefile prevopts) None handleOptionsLine prevopts
  handleHaskell opts rest
  exitWith ExitSuccess

data InterfaceState = None | Exclude | DefaultIn | DefaultOut | InParam | OutParam
  deriving (Eq)

handleOptionsLine :: String -> State InterfaceState (Options -> Options)
handleOptionsLine = 
  processor None 
            [(Exclude,    \n -> modExcludepatterns (n:))
            ,(DefaultIn,  \n -> modDefaultins (n:))
            ,(DefaultOut, \n -> modDefaultouts (n:))
            ,(InParam,    \n -> modInparameters (n:))
            ,(OutParam,   \n -> modOutparameters (n:))]
            [("@exclude",     Exclude)
            ,("@default-in",  DefaultIn)
            ,("@default-out", DefaultOut)
            ,("@in-param",    InParam)
            ,("@out-param",   OutParam)]

handleHaskell :: Options -> [FilePath] -> IO ()
handleHaskell opts filenames = do
    gencontents <- mapM readFile filenames
    let genparses = map parseHeader gencontents
        (genperrs, genpress) = partitionEithers genparses
    case genperrs of
      ((str, err):_) -> do
          putStrLn str
          putStrLn $ "Could not parse generated file (!): " ++ show err
          exitWith (ExitFailure 2)
      []             -> do
          createDirectoryIfMissing True (outputdir opts)
          haskellGen opts $ zip (map takeFileName filenames) genpress
          exitWith ExitSuccess

