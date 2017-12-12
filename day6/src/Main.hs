module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- =============================================================================

type Key       = Int
type Value     = Int
type Iteration = Int
type Address   = (Key, Value)

type Board     = Map.Map Key Value
type Past      = Set.Set Board

data State = State
  { iter    :: Iteration
  , past    :: Past
  , current :: Board
  } deriving (Show)

-- =============================================================================

makeBoard :: String -> Board
makeBoard = Map.fromList . zip [0, 1..] . fmap read . words

initState :: State
initState = State 0 initPast initBoard
  where
    initBoard = makeBoard "2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14"
    initPast = Set.empty

-- =============================================================================

step :: State -> State
step st = st
    { iter = iter st + 1
    , past = Set.insert (current st) (past st)
    , current = spreadMemory addr (current st)
    }
  where addr = findMax (current st)


findMax :: Board -> Address
findMax b = findMax' (0, 0) (Map.toList b)
  where
    findMax' p [] = p
    findMax' (mxk, mxv) ((k, v):bs) =
      if mxv >= v
        then findMax' (mxk, mxv) bs
        else findMax' (k, v) bs


spreadMemory :: Address -> Board -> Board
spreadMemory (mxk, mxv) board = incdMemory
  where
    nextkey    = succ mxk
    len        = Map.size board
    zerod      = Map.insert mxk 0 board
    targets    = take mxv [nextkey..]
    incMem t b = Map.adjust succ (t `mod` len) b
    incdMemory = foldr incMem zerod targets

iterCondition :: State -> Bool
iterCondition st = not $ Set.member (current st) (past st)

itter :: State -> State
itter state =
  if iterCondition state
    then itter $ step state
    else state

-- =============================================================================

main :: IO ()
main = do
  let finalState = itter initState
  (putStrLn . show) $ iter finalState
