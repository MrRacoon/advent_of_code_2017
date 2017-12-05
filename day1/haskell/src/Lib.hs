module Lib (someFunc) where

import Data.Char (ord)

keepOrDitch :: Int -> Int -> Int
keepOrDitch x y =
  if x == y
    then x
    else 0

calc :: [Int] -> Int
calc xs = sum
     $ zipWith keepOrDitch xs
     $ tail
     $ cycle xs

someFunc :: IO ()
someFunc = do
  contents <- readFile "./input.txt"
  putStrLn $ show $ calc $ toListOfNums $ head $ lines contents
    where
      toListOfNums :: String -> [Int]
      toListOfNums = fmap ((\x -> x - 48) . ord)
