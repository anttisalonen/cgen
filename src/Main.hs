{-# LANGUAGE TemplateHaskell #-}
module Main(main)
where

import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath
import System.Directory
import System.IO
import Data.Either
import Data.List
import Control.Monad
import Control.Monad.State
import Text.Printf
import qualified Data.Set as S

import Text.Regex.Posix

import HeaderParser
import HeaderData
import CppGen
import Utils
import Options
import DeriveMod

data Options = Options
  {
    outputdir         :: FilePath
  , inputfiles        :: [FilePath]
  , includefiles      :: [FilePath]
  , includedir        :: FilePath
  , excludepatterns   :: [String] 
  , excludebases      :: [String]
  , checksuperclasses :: Bool
  , renamedtypes      :: [(String, String)]
  , excludeclasses    :: [String]
  , interfacefile     :: String
  , hsoutpath         :: Maybe FilePath
  , dumpmode          :: Bool
  , showhelp          :: Bool
  }
  deriving (Show)
$(deriveMods ''Options)

defaultOptions :: Options
defaultOptions = Options "cgen" [] [] "" [] [] False [] [] "" Nothing False False

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"]        (ReqArg (setOutputdir) "Directory")                  "output directory for the C files"
  , Option []    ["header"]        (ReqArg (\l -> modIncludefiles    (l:)) "File")      "file to include in the generated headers"
  , Option []    ["exclude"]       (ReqArg (\l -> modExcludepatterns (l:)) "Function")  "exclude pattern for function names"
  , Option []    ["exclude-base"]  (ReqArg (\l -> modExcludebases    (l:)) "Class")     "exclude pattern for required base classes (with check-super)"
  , Option []    ["exclude-class"] (ReqArg (\l -> modExcludeclasses  (l:)) "Class")     "exclude pattern for classes"
  , Option []    ["check-super"]   (NoArg  (setChecksuperclasses True))                 "report error if super classes aren't found"
  , Option []    ["rename"]        (ReqArg (addRenamedTypes) "oldtype|newtype")         "rename a type by another one"
  , Option ['i'] ["interface"]     (ReqArg (setInterfacefile) "file")                   "define input interface file"
  , Option []    ["dump"]          (NoArg  (setDumpmode True))                          "simply dump the parsed data of the header"
  , Option ['I'] ["include"]       (ReqArg (setIncludedir) "Directory")                 "include path for the header files"
  , Option []    ["help"]          (NoArg  (setShowhelp True))                          "display this help"
  ]

addRenamedTypes :: String -> Options -> Options
addRenamedTypes l = 
  case splitBy '|' l of
    [t1,t2] -> modRenamedtypes ((t1,t2):)
    _       -> id

main :: IO ()
main = do 
  args <- getArgs
  let (actions, rest, errs) = getOpt Permute options args
      prevopts              = foldl' (flip ($)) defaultOptions actions
  when (showhelp prevopts) $
    usage options ExitSuccess
  when (not (null errs) || null rest) $ do
    mapM_ (hPutStrLn stderr) errs
    usage options (ExitFailure 1)
  opts <- handleInterfaceFile (interfacefile prevopts) None handleOptionsLine prevopts
  contents <- mapM readFile (map (if null (includedir opts) then id else (includedir opts </>)) rest)
  let parses = map parseHeader contents
      (perrs, press) = partitionEithers parses
  case perrs of
    ((str, err):_) -> do
        putStrLn str
        putStrLn $ "Could not parse: " ++ show err
        exitWith (ExitFailure 1)
    []             -> do
      if dumpmode opts
        then print press
        else do 
          handleParses (outputdir opts) 
                       (includefiles opts) 
                       (excludepatterns opts) 
                       (excludeclasses opts) 
                       (if checksuperclasses opts 
                          then Just (excludebases opts) 
                          else Nothing) 
                       (renamedtypes opts) 
                $ zip (map takeFileName rest) press
          exitWith ExitSuccess

handleOptionsLine :: String -> State InterfaceState (Options -> Options)
handleOptionsLine = 
  processor None
           [(Exclude,      \n -> modExcludepatterns (n:)),
            (Header,       \n -> modIncludefiles (n:)),
            (Rename,       \n -> addRenamedTypes n),
            (ExcludeClass, \n -> modExcludeclasses (n:))]
           [("@exclude", Exclude),
            ("@header", Header),
            ("@rename", Rename),
            ("@exclude-class", ExcludeClass)]

data InterfaceState = None | Exclude | ExcludeClass | Header | Rename
  deriving (Eq)

handleParses :: FilePath -> [FilePath] -> [String] -> [String] -> Maybe [String] -> [(String, String)] -> [(FilePath, [Object])] -> IO ()
handleParses outdir incfiles excls exclclasses exclbases rens objs = do
    createDirectoryIfMissing True outdir
    case exclbases of
      Just ex -> checkSuperClasses ex (concatMap snd objs)
      Nothing -> return ()
    mapM_ (uncurry $ handleHeader outdir incfiles exclclasses excls rens) objs

checkSuperClasses :: [String] -> [Object] -> IO ()
checkSuperClasses excls objs = do
    let classes       = nub $ getClasses objs
        superclasses  = map inheritname $ filter (\i -> inheritlevel i == Public) $ concatMap classinherits classes
        missing       = S.difference (S.fromList superclasses) (S.fromList $ map getObjName classes)
        missingrest   = S.filter (\s -> not $ or (map (\e -> s =~ e) excls)) missing
    when (not $ S.null missingrest) $ do
        hPutStrLn stderr "Error: the following base classes could not be found: "
        forM_ (S.toList missingrest) $ \s -> do
            hPrintf stderr "    %-40s\n" s
        exitWith (ExitFailure 3)

