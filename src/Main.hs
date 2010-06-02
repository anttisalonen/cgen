module Main()
where

import HeaderParser
import HeaderData

getFuns :: [Object] -> [Object]
getFuns [] = []
getFuns (o:os) = 
  case o of
    (FunDecl _ _ _ _ _) -> o : getFuns os
    (Namespace _ os2)   -> getFuns os2 ++ getFuns os
    (ClassDecl _ _ os2) -> getFuns os2 ++ getFuns os
    _                   -> getFuns os

getObjName :: Object -> String
getObjName (FunDecl n _ _ _ _) = n
getObjName (Namespace n _ )    = n
getObjName (TypeDef (n, _))    = n
getObjName (ClassDecl n _ _)   = n
getObjName (VarDecl p _)       = varname p
getObjName (EnumDef n _)       = n

publicMemberFunction :: Object -> Bool
publicMemberFunction (FunDecl n _ _ _ (Just (Public, _))) = case n of
  ('_':_) -> False
  _       -> True
publicMemberFunction _                                  = False

main :: IO ()
main = do 
  input <- getContents
  case parseHeader input of
    Left  (_, err) -> putStrLn $ "Could not parse: " ++ show err
    Right hdr      -> do
      print hdr
      let funs = getFuns hdr
      return (map getObjName $ filter publicMemberFunction $ funs) >>= print

