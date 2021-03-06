{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-
--- Day 4: Giant Squid ---
You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:

7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
Finally, 24 is drawn:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

Your puzzle answer was 87456.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---
On the other hand, it might be wise to try a different strategy: let the giant squid win.

You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?

Your puzzle answer was 15561.

Both parts of this puzzle are complete! They provide two gold stars: **
-}

module AoC.Day4 where

import Control.Arrow ((&&&))
import Data.Array.IArray (Array, assocs, listArray)
import Data.Bool (bool)
import Data.Either (fromLeft, isRight)
import Data.Foldable (foldr')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', partition)
import Data.List.Split (splitOn, splitWhen)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Debug.Trace (trace)

type Input = [String]

type Output = Int

type Numbers = [Int]

type Boards = [Board]

type Coords = (Int, Int)

type Board = IntMap (Coords, Bool)

type Matches = Map XY Int -- Int # of occurences

data BoardState = BoardState
  { board :: Board,
    matches :: Matches
  }
  deriving (Show)

type Game = [BoardState]

data XY = X Int | Y Int deriving (Show, Eq, Ord)

---

dims :: Int
dims = 5

-- |
-- >>> parseGame ["1,2,3,4", "", "10 20 30 40", "11 12 13 12"]
parseGame :: [String] -> (Numbers, Game)
parseGame lines =
  let ([numsS] : boardsS) = splitWhen (== "") lines -- split on empty lines
      nums = (read @Int) <$> splitOn "," numsS
      boards = fmap parseBoard boardsS
      states = fmap (`BoardState` M.empty) boards
   in (nums, states)

-- |
-- >>> parseBoard ["1 2 3 4", "11 12 13 14", "21 22 23 24", "31 32 33 34"]
parseBoard :: [String] -> Board
parseBoard lines =
  let board = fmap (fmap (read @Int) . words) lines
      rows = length . head $ board
      cols = length board
      coords = concatMap (\x -> fmap (x,) [1 .. cols]) [1 .. rows]
      asList = zip coords (concat board)
   in IM.fromList $ fmap (\((r, c), v) -> (v, ((r, c), False))) asList

drawNumberToWin :: Game -> Int -> Either Int Game
drawNumberToWin bs n =
  let newBs = fmap (`addNumberToBoard` n) bs
      winners = filter wins newBs
   in if null winners
        then Right newBs
        else Left $ points (board $ head winners) n

drawNumberToWinToLoose :: Game -> Int -> Either Int Game
drawNumberToWinToLoose bs n =
  let newBs = fmap (`addNumberToBoard` n) bs
      (winners, loosers) = partition wins newBs
   in if not (null loosers)
        then Right loosers
        else Left $ points (board $ head winners) n

addNumberToBoard :: BoardState -> Int -> BoardState
addNumberToBoard b@BoardState {..} k =
  case IM.lookup k board of
    Nothing -> b
    Just (c@(x, y), _) ->
      let newBoard = IM.insert k (c, True) board
          newMatches = M.insertWith (+) (X x) 1 matches
          newMatches' = M.insertWith (+) (Y y) 1 newMatches
       in BoardState newBoard newMatches'

wins :: BoardState -> Bool
wins BoardState {..} = not . null $ M.filter (== dims) matches

points :: Board -> Int -> Int
points b v =
  let filtered = IM.filter (not . snd) b
   in v * sum (IM.keys filtered)

playTheGame :: Bool -> (Numbers, Game) -> Int
playTheGame winFlag (nums, game) =
  let strategy = bool drawNumberToWinToLoose drawNumberToWin winFlag
      f g n = either (const g) (`strategy` n) g
      result = foldl' f (Right game) nums
   in fromLeft 1 result

---

fstStar :: Input -> Output
fstStar = playTheGame True . parseGame

sndStar :: Input -> Output
sndStar = playTheGame False . parseGame

main :: IO ()
main = do
  input <- lines <$> readFile "src/input/day4"
  print . (fstStar &&& sndStar) $ input
