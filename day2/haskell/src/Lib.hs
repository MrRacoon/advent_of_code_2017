module Lib
    ( someFunc
    ) where

-- =============================================================================

data MinMax
  = None
  | One Int
  | Both Int Int


minMaxSum :: MinMax -> Int
minMaxSum None       = 0
minMaxSum (One a)    = a
minMaxSum (Both a b) = b - a

-- =============================================================================

accumulateMinMax :: MinMax -> Int -> MinMax
accumulateMinMax None x = One x
accumulateMinMax (One a) x =
  if x < a
    then Both x a
    else Both a x
accumulateMinMax (Both a b) x =
  if x < a
    then Both x b
    else if x > b
      then Both a x
      else Both a b

-- =============================================================================

findMinMax :: [Int] -> MinMax
findMinMax xs = findMinMax' None xs

findMinMax' mm [] = mm
findMinMax' mm (x:xs) = findMinMax' (accumulateMinMax mm x) xs


-- =============================================================================

findMinMaxes :: [[Int]] -> [Int]
findMinMaxes = fmap (minMaxSum . findMinMax)

-- =============================================================================

someFunc :: IO ()
someFunc = do
    contents <- readFile "./src/input"
    putStrLn $ show $ sum $ findMinMaxes $ toNums $ toWords contents
  where
    toWords :: String -> [[String]]
    toWords = fmap words . lines

    toNums :: [[String]] -> [[Int]]
    toNums = fmap (fmap read)
