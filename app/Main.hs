{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import AOC.AOC
import System.Command
import Data.Maybe (fromMaybe)
import ReadArgs (readArgs)
import Day01.Day01
import Data.List (intersperse)
import Data.IntMap (fromList, elems, lookup)

main :: IO ()
main = do
  (day :: Maybe Int) <- readArgs
  _ <- clearTerminal
  _ <- boldLine "Advent of Code 2023"
  _ <- blankLine 1
  sequence_ (intersperse (blankLine 1) (getSolutions day)) >> blankLine 2


getSolutions :: Maybe Int -> [IO()]
getSolutions md = case md of
    Nothing -> elems solutions
    Just day -> [fromMaybe (putStr (red ("Day" ++ show day ++ " is missing from the days list in main.hs"))) (Data.IntMap.lookup day solutions)]
  where solutions = fromList days

days = [
  (1, day01)
 ]
