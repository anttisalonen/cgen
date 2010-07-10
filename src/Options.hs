module Options(handleInterfaceFile,
  processor)
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

processor :: (Eq b) => b -> [(b, (String -> a -> a))] -> [(String, b)] -> String -> State b (a -> a)
processor none actions labels l = do
  case l of
      ('#':_) -> return id            -- comment
      ""      -> return id
      ('~':_) -> put none >> return id
      txt     -> case lookup l labels of
                   Just lb -> put lb >> return id
                   Nothing -> do
                     lb <- get
                     return $ case lookup lb actions of
                       Just act -> act txt
                       Nothing  -> id

