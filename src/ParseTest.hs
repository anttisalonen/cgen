module Main
where

import System.Exit

import HeaderParser

main :: IO ()
main = do 
  input <- getContents
  case parseHeader input of
    Left  (str, err) -> do
      putStrLn str
      putStrLn $ "Could not parse: " ++ show err
      exitWith (ExitFailure 1)
    Right hdr -> do
      print hdr
      exitWith ExitSuccess

