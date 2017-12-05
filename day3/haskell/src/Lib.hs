module Lib ( someFunc ) where

-- =============================================================================
-- Problem

-- You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
--
-- Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
--
-- 17  16  15  14  13
-- 18   5   4   3  12
-- 19   6   1   2  11
-- 20   7   8   9  10
-- 21  22  23---> ...
-- While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
--
-- For example:
--
-- Data from square 1 is carried 0 steps, since it's at the access port.
-- Data from square 12 is carried 3 steps, such as: down, left, left.
-- Data from square 23 is carried only 2 steps: up twice.
-- Data from square 1024 must be carried 31 steps.
-- How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

-- =============================================================================
-- Solution

-- Mimic a hardisk, use odd squares to create an origin, calculate from

--
--
--   |<------ Width ------>|
--   +---------------------+
--   | 17  16  15  14  13  |
--   | 18   5   4   3  12  |
--   | 19   6   1   2  11  |
--   | 20   7   8   9  10 _|_
--   | 21  22  23  24  25 Track
--   +----------------->|
--            |<midstep>|
--   |<--- step --->|
--

-- =============================================================================
-- Tracks

type Index    = Float
type Min      = Float
type Max      = Float
type Width    = Float
type Address  = Float
type Distance = Float

data Track = Track
  { index :: Index
  , tmin  :: Min
  , tmax  :: Max
  , twidth :: Width
  } deriving (Show)

center = Track 0 1 1 0

tracks :: [Track]
tracks = center : fmap makeTrack indexedMinMax
  where
    makeTrack (i, (n, m)) = Track i n m (sqrt m)
    indexedMinMax         = zip [1..] $ zip incSquares $ tail squares
    incSquares            = map succ squares
    squares               = filter isOdd [ (x * x) | x <- [1..] ]

findTrack :: Address -> Track
findTrack = findTrack' tracks
  where
    findTrack' [] x = error "Infinite List is not Infinite"
    findTrack' (t:ts) x =
      if x > tmax t
        then findTrack' ts x
        else t

findTrackMids :: Track -> [Address]
findTrackMids t = [n, e, s, w]
  where
    init    = tmax t
    step    = twidth t - 1
    midstep = step / 2
    s       = init - midstep
    w       = s - step
    n       = w - step
    e       = n - step


spindleDistance :: Address -> Distance
spindleDistance x
    = ((index track) +)
    $ minimum
    $ map (distance x) mids
  where
    track = findTrack x
    mids  = findTrackMids track
    distance a b = abs $ a - b


-- =============================================================================
-- Main

input :: Float
input = 265149

someFunc :: IO ()
someFunc = putStrLn $ show $ spindleDistance input

-- =============================================================================
-- Utils

isOdd :: Float -> Bool
isOdd = (/= 0) . (`mod` 2) . round
