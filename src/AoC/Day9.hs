{-
--- Day 9: Smoke Basin ---
These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678
Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?

Your puzzle answer was 524.

The first half of this puzzle is complete! It provides one gold star: *
-}

module AoC.Day9 where

import Control.Arrow ((&&&))
import Debug.Trace (trace)

type Input = [[Int]]

type Output = Int

---

triples :: Int -> Input -> Output
triples acc [x11 : x12 : x13 : x1, x21 : x22 : x23 : x2, x31 : x32 : x33 : x3] =
  if x22 < minimum [x12, x32, x21, x23]
    then triples (acc + x22 + 1) [x12 : x13 : x1, x22 : x23 : x2, x32 : x33 : x3]
    else triples acc [x12 : x13 : x1, x22 : x23 : x2, x32 : x33 : x3]
triples acc _ = acc

borderY :: [Int] -> [Int]
borderY xs = [maxBound] <> xs <> [maxBound]

borderX :: [Int]
borderX = repeat maxBound

---

fstStar :: Input -> Output
fstStar ls@(x : y : z : xs) = sum $ zipWith3 (\a b c -> triples 0 [a, b, c]) ls (y : z : xs) (z : xs)
fstStar _ = error "Data input is not valid"

sndStar :: Input -> Output
sndStar = undefined

addBorders :: [[Int]] -> [[Int]]
addBorders xs = [borderX] <> fmap borderY xs <> [borderX]

---

sample :: IO String
sample =
  pure $
    unlines
      [ "2199943210",
        "3987894921",
        "9856789892",
        "8767896789",
        "9899965678"
      ]

main :: IO ()
main = do
  input <- addBorders . fmap (fmap (read . pure)) . lines <$> readFile "src/input/day9" -- sample
  print . (fstStar &&& sndStar) $ input
