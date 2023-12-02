module Day02.Day02 (day02) where

import Prelude hiding (lookup)

import AOC.AOC
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Map (Map, fromList, findWithDefault, unionsWith)

import Control.Applicative

-- Common functionality Part 1 and Part 2

parseError = error "parse error (unexpected condition)"

beforeParseInput = id
afterParseInput = id


parseBalldraw :: [String] -> Balldraw
parseBalldraw balldraw = fromList (map ((\a -> (a !! 1, fromMaybe parseError $ strToMaybeInt (head a))) . splitOn " ") balldraw)

parseInputLine line = Just $ Game id draws
    where
        splittedLine = splitOn ": " line
        id = fromMaybe 0 (strToMaybeInt $ splitOn " " (head splittedLine) !! 1)
        draws = map (parseBalldraw . splitOn ", ") (splitOn "; " (splittedLine !! 1))


type Balldraw = Map String Int

data Game = Game {
    gameId :: Int,
    balldraws :: [Balldraw]
} deriving (Show, Eq)


-- Solution Part 1

isPossibleGame :: Int -> Int -> Int -> Game -> Bool
isPossibleGame red green blue game = all (\balldraw -> findWithDefault 0  "red" balldraw <= red && findWithDefault 0  "green" balldraw <= green && findWithDefault 0  "blue" balldraw <= blue) (balldraws game)

solvePart1 games = sum $ map gameId $ filter (isPossibleGame 12 13 14) games


-- Solution Part 2

power :: Balldraw -> Int
power balldraw = findWithDefault 0 "red" balldraw * findWithDefault 0 "green" balldraw * findWithDefault 0 "blue" balldraw


solvePart2 games = sum $ map ((power . unionsWith max) . balldraws) games


-- Show Solution for Examples & Puzzles

problems = [
        Example 1 solver1 (expectedValue 8)
        , Puzzle solver1 (expectedValue Just 2528)
        , Example 1 solver2 (expectedValue 2286)
        , Puzzle solver2 (expectedValue Just 67363)
        ]
    where
        solver1 = Solver 1 parseInputLine beforeParseInput afterParseInput solvePart1
        solver2 = Solver 2 parseInputLine beforeParseInput afterParseInput solvePart2


day02 :: IO ()
day02 = solveDay 02 problems