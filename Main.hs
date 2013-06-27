module Main
where

import Control.Monad (replicateM_)

import Types
import Animal
import Dog
import Sheep

main = do
  dog_with $ \d -> 
    sheep_with 1 $ \s -> do
      dog_make_sound d
      sheep_make_sound s
      sheep_shear s
      replicateM_ 5 $ animal_increment_age (toAnimal s)
      animal_get_age (toAnimal s) >>= \a -> putStrLn $ "The sheep is now " ++
          show a ++ " years old!"
