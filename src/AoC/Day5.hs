{-# LANGUAGE TupleSections #-}

{-
--- Day 5: Hydrothermal Venture ---
You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:

0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:

An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the following diagram:

.......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111....
In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

Your puzzle answer was 5576.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---
Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:

An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
Considering all lines from the above example would now produce the following diagram:

1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....
You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.

Consider all of the lines. At how many points do at least two lines overlap?

Your puzzle answer was 18144.

Both parts of this puzzle are complete! They provide two gold stars: **
-}

module AoC.Day5 where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Coords = (Int, Int)

type Line = (Coords, Coords)

type Input = [Line]

type Output = Int

---

-- |
-- >>> parseLine "1,2 -> 3,4"
-- ((1,2),(3,4))
parseLine :: String -> Line
parseLine =
  tupleFromList
    . fmap
      ( tupleFromList
          . fmap read
          . splitOn ","
      )
    . splitOn "->"
  where
    tupleFromList [x, y] = (x, y)
    tupleFromList xs = error $ "invalid input: " <> show xs

isHorizontal :: Line -> Bool
isHorizontal ((x1, _), (x2, _)) = x1 == x2

isVertical :: Line -> Bool
isVertical ((_, y1), (_, y2)) = y1 == y2

-- |
-- >>> toPoints ((3,1), (3,7))
-- >>> toPoints ((3,1), (5,1))
-- >>> toPoints ((5,1), (3,1))
-- >>> toPoints ((1,1), (3,3))
-- >>> toPoints ((9,7), (7,9))
-- [(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)]
-- [(3,1),(4,1),(5,1)]
-- [(3,1),(4,1),(5,1)]
-- [(1,1),(2,2),(3,3)]
-- [(7,7),(8,8),(9,9)]
toPoints :: Line -> [Coords]
toPoints l@((x1, y1), (x2, y2)) = let
  incr a b = if a > b then (reverse, (b, a)) else (id, (a, b))
  (transX, (xA, xB)) = incr x1 x2
  (transY, (yA, yB)) = incr y1 y2
  in if xA-xB == yA-yB
     then zip (transX [xA..xB]) (transY [yA..yB])
     else if (xA /= xB) && (yA /= yB)
          then error "Invalid line type"
          else liftA2 (,) [xA..xB] [yA..yB]


overlappingPoints :: (Line -> Bool) -> Input -> Output
overlappingPoints fltr lines =
  let filtered = filter fltr lines
      allCoords = concatMap toPoints filtered
      coordsMap = foldl' (\m c -> M.insertWith (+) c 1 m) M.empty allCoords
   in M.size $ M.filter (> 1) coordsMap
---

fstStar :: Input -> Output
fstStar = overlappingPoints (\x -> or $ fmap ($x) [isHorizontal, isVertical])

sndStar :: Input -> Output
sndStar = overlappingPoints (const True)

---

main :: IO ()
main = do
  input <- fmap parseLine . lines <$> readFile "src/input/day5"
  print . (fstStar &&& sndStar) $ input
