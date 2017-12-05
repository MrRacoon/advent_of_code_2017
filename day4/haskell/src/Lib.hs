module Lib
    ( someFunc
    ) where

import Data.List (nub)

valid :: String -> Bool
valid str = ws == (nub ws)
  where ws = words str

calc :: String -> Int
calc = length . filter id . fmap valid . lines

someFunc :: IO ()
someFunc = do
  contents <- readFile "./src/input"
  putStrLn $ show $ calc contents
