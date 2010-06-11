module Options(handleInterfaceFile)
where

import Control.Monad.State

handleInterfaceFile :: FilePath -> b -> (String -> State b (a -> a)) -> a -> IO a
handleInterfaceFile inputFile initState fun oldopts 
  | null inputFile = return oldopts
  | otherwise = do
    contents <- readFile inputFile
    let ls = lines contents
    return $ flip evalState initState (parseInterfaceFile fun ls oldopts)

parseInterfaceFile :: (String -> State b (a -> a)) -> [String] -> a -> State b a
parseInterfaceFile _   []     opts = return opts
parseInterfaceFile fun (l:ls) opts = do
    ofun <- fun l
    parseInterfaceFile fun ls (ofun opts)

