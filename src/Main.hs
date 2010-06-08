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
import Control.Monad
import Control.Monad.State
import Text.Printf
import qualified Data.Set as S

import Text.Regex.Posix

import HeaderParser
import HeaderData
import CppGen
import HaskellGen
import Utils
import DeriveMod

data Options = Options
  {
    outputdir         :: FilePath
  , inputfiles        :: [FilePath]
  , includefiles      :: [FilePath]
  , excludepatterns   :: [String] 
  , excludebases      :: [String]
  , checksuperclasses :: Bool
  , renamedtypes      :: [(String, String)]
  , excludeclasses    :: [String]
  , interfacefile     :: String
  , hsoutpath         :: Maybe FilePath
  , dumpmode          :: Bool
  }
  deriving (Show)
$(deriveMods ''Options)

defaultOptions :: Options
defaultOptions = Options "" [] [] [] [] False [] [] "" Nothing False

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"]        (ReqArg (setOutputdir) "Directory")                  "output directory for the C files"
  , Option []    ["header"]        (ReqArg (\l -> modIncludefiles    (l:)) "File")      "file to include in the generated headers"
  , Option []    ["exclude"]       (ReqArg (\l -> modExcludepatterns (l:)) "Function")  "exclude pattern for function names"
  , Option []    ["exclude-base"]  (ReqArg (\l -> modExcludebases    (l:)) "Class")     "exclude pattern for required base classes (with check-super)"
  , Option []    ["exclude-class"] (ReqArg (\l -> modExcludeclasses  (l:)) "Class")     "exclude pattern for classes"
  , Option []    ["check-super"]   (NoArg  (setChecksuperclasses True))                 "report error if super classes aren't found"
  , Option []    ["rename"]        (ReqArg (addRenamedTypes) "oldtype|newtype")         "rename a type by another one"
  , Option []    ["interface"]     (ReqArg (setInterfacefile) "file")                   "define input interface file"
  , Option []    ["hs-output"]     (ReqArg (setHsoutpath . Just) "Directory")           "enable generation and set the output directory for Haskell files"
  , Option []    ["dump"]          (NoArg  (setDumpmode True))                          "simply dump the parsed data of the header"
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
  when (not (null errs) || null rest) $ do
    mapM_ putStrLn errs
    pr <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ pr ++ " <options> <C++ header files>") options
    exitWith (ExitFailure 1)
  let prevopts = foldl' (flip ($)) defaultOptions actions
  opts <- handleInterfaceFile prevopts
  contents <- mapM readFile rest
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
          case hsoutpath opts of
            Nothing    -> exitWith ExitSuccess
            Just hsout -> handleHaskell hsout (map takeFileName rest) (outputdir opts)

handleHaskell :: FilePath -> [FilePath] -> FilePath -> IO ()
handleHaskell hsout initfilenames indir = do
    let filenames = map (indir </>) initfilenames
    gencontents <- mapM readFile filenames
    let genparses = map parseHeader gencontents
        (genperrs, genpress) = partitionEithers genparses
    case genperrs of
      ((str, err):_) -> do
          putStrLn str
          putStrLn $ "Could not parse generated file (!): " ++ show err
          exitWith (ExitFailure 3)
      []             -> do
          createDirectoryIfMissing True hsout
          haskellGen hsout $ zip (map takeFileName filenames) genpress
          exitWith ExitSuccess

handleInterfaceFile :: Options -> IO Options
handleInterfaceFile oldopts | null (interfacefile oldopts) = return oldopts
                            | otherwise = do
    contents <- readFile (interfacefile oldopts)
    let ls = lines contents
    return $ flip evalState None (parseInterfaceFile ls oldopts)

parseInterfaceFile :: [String] -> Options -> State InterfaceState Options
parseInterfaceFile []     opts = return opts
parseInterfaceFile (l:ls) opts = do
    case l of
      ('#':_)          ->                     parseInterfaceFile ls opts -- comment
      "@exclude"       -> put Exclude      >> parseInterfaceFile ls opts
      "@header"        -> put Header       >> parseInterfaceFile ls opts
      "@rename"        -> put Rename       >> parseInterfaceFile ls opts
      "@exclude-class" -> put ExcludeClass >> parseInterfaceFile ls opts
      ""               ->                     parseInterfaceFile ls opts
      ('~':_)          -> put None         >> parseInterfaceFile ls opts
      n -> do
        v <- get
        let ofun = case v of
                   Exclude      -> modExcludepatterns (n:)
                   Header       -> modIncludefiles (n:)
                   Rename       -> addRenamedTypes n
                   ExcludeClass -> modExcludeclasses (n:)
                   None         -> id
        parseInterfaceFile ls (ofun opts)

data InterfaceState = None | Exclude | ExcludeClass | Header | Rename

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

