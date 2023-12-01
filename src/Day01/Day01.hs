module Day01.Day01 (day01) where

import AOC.AOC
import Data.List (findIndex, isPrefixOf)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

-- Common functionality Part 1 and Part 2

beforeParseInput = id
afterParseInput = id
parseInputLine = Just

allDigits = [
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"]

noCalibrationValue = error "Line does not contain calibration value"

findFirstDigit digitsToFind line
  | null line = noCalibrationValue
  | isDigit (head line) = head line
  | otherwise = maybe (findFirstDigit digitsToFind (tail line)) (head . show . (+1)) (findIndex (`isPrefixOf` line) digitsToFind)

findLastDigit digitsToFind line = findFirstDigit (map reverse digitsToFind) (reverse line)

calibrationValue digitsToFind line = fromMaybe noCalibrationValue $ strToMaybeInt [findFirstDigit digitsToFind line, findLastDigit digitsToFind line]

sumOfCalibrationValues digitsToFind lines  = sum $ map (calibrationValue digitsToFind) lines


-- Solution Part 1

solvePart1 = sumOfCalibrationValues []


-- Solution Part 2

solvePart2 = sumOfCalibrationValues allDigits



-- Show Solution for Examples & Puzzles

problems = [
    Example 1 solver1 (expectedValue 142)
    , Puzzle solver1 (expectedValue Just 53334)
    , Example 2 solver2 (expectedValue 281)
    , Puzzle solver2 (expectedValue Just 52834)
    ]
  where
    solver1 = Solver 1 parseInputLine beforeParseInput afterParseInput solvePart1
    solver2 = Solver 2 parseInputLine beforeParseInput afterParseInput solvePart2


day01 :: IO ()
day01 = solveDay 01 problems