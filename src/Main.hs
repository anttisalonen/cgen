module Main()
where

import System.Environment
import System.Exit
import Data.Either

import HeaderParser
import HeaderData

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

main :: IO ()
main = do 
  filenames <- getArgs
  contents <- mapM readFile filenames
  let parses = map parseHeader contents
      (perrs, press) = partitionEithers parses
  case perrs of
    ((str, err):_) -> do
        putStrLn str
        putStrLn $ "Could not parse: " ++ show err
        exitWith (ExitFailure 1)
    _              -> do
      handleParses (concat press)

handleParses :: [Object] -> IO ()
handleParses objs = do
    let funs = getFuns objs
    return (map getObjName $ filter publicMemberFunction $ funs) >>= print
    exitWith ExitSuccess

