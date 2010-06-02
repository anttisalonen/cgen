module Main
where

import HeaderParser

main :: IO ()
main = do 
  input <- getContents
  case parseHeader input of
    Left  (str, err) -> do
      putStrLn str
      putStrLn $ "Could not parse: " ++ show err
    Right hdr -> do
      print hdr

