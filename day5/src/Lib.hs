module Lib ( someFunc ) where

-- An urgent interrupt arrives from the CPU: it's trapped in a maze of jump instructions, and it would like assistance from any programs with spare cycles to help find the exit.
--
-- The message includes a list of the offsets for each jump. Jumps are relative: -1 moves to the previous instruction, and 2 skips the next one. Start at the first instruction in the list. The goal is to follow the jumps until one leads outside the list.
--
-- In addition, these instructions are a little strange; after each jump, the offset of that instruction increases by 1. So, if you come across an offset of 3, you would move three instructions forward, but change it to a 4 for the next time it is encountered.
--
-- For example, consider the following list of jump offsets:
--
-- 0
-- 3
-- 0
-- 1
-- -3
-- Positive jumps ("forward") move downward; negative jumps move upward. For legibility in this example, these offset values will be written all on one line, with the current instruction marked in parentheses. The following steps would be taken before an exit is found:
--
-- (0) 3  0  1  -3  - before we have taken any steps.
-- (1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
--  2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
--  2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
--  2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
--  2  5  0  1  -2  - jump 4 steps forward, escaping the maze.
-- In this example, the exit is reached in 5 steps.
--
-- How many steps does it take to reach the exit?

import Control.Monad.Trans.State.Lazy
import Data.Map.Strict

-- =============================================================================

type Board   = Map Int Int
type Count   = Int
type Current = Int

-- =============================================================================

data Day5 = Day5
  { current :: Current
  , count   :: Count
  , board   :: Board
  } deriving (Show)

initialDay :: Board -> Day5
initialDay board = Day5 0 0 board

type DayState a = State Day5 a

-- =============================================================================

step :: DayState Day5
step = do
  state <- get
  let nextCurrent = (current state) + ((board state) ! current state)
      nextCount   = count state + 1
      nextBoard   = adjust succ (current state) (board state)
      nextState   = state
        { current = nextCurrent
        , board   = nextBoard
        , count   = nextCount
        }
  put nextState
  return nextState

stopCondition :: DayState Bool
stopCondition = do
  state <- get
  return $ not $ member (current state) $ board state

iterateOnProblem :: DayState Day5
iterateOnProblem = do
  nextBoard <- step
  done      <- stopCondition
  if done
    then return nextBoard
    else iterateOnProblem

-- =============================================================================

someFunc :: IO ()
someFunc = do
  bInit <- readInFile "./src/input"
  let board = initialDay bInit
      final = fst $ runState iterateOnProblem board
  putStrLn $ show board
  putStrLn $ show final
  putStrLn $ show $ count final


readInFile :: String -> IO Board
readInFile file = do
    contents <- readFile file
    return $ fromList $ addIndexes $ toInts $ lines contents
  where
    toInts = fmap read
    addIndexes xs = zip [0,1..] xs
