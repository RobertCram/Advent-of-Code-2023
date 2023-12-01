module AOC.AOC (Solver (..), PuzzleType (..), solveDay, solveParts, strToMaybeInt, breakFold, countMap, safeHead, blankLine, boldLine, clearTerminal, expectedValue, red, green, t3_1, t3_2, t3_3) where
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Monad (replicateM_, foldM)
import Data.Map(Map, insertWith)
import qualified Data.Map(empty)
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, diffUTCTime)


data TerminalColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving Enum

data PuzzleType a b = Example Int (Solver a b) b | Puzzle (Solver a b) (Maybe b)

data Solver a b =  Solver { part :: Int, parseLine :: String -> Maybe a, beforeParse :: [String] -> [String], afterParse :: [a] -> [a], solve :: [a] -> b }

defaultSolver = Solver { part = 0, parseLine = Just, beforeParse = id, afterParse = id , solve = const }

inputFileName :: Show b => PuzzleType a b -> Int -> String
inputFileName puzzleType day = case puzzleType of
      Example n _ _ -> "./src/" ++ dayStr ++ "/testinput" ++ show n ++ ".txt"
      Puzzle {} -> "./src/" ++ dayStr ++ "/puzzleinput.txt"
      where dayStr = "Day" ++ printf "%.2d" (day :: Int) :: String

execute :: (Show b, Eq b) => Int -> PuzzleType a b -> IO ()
execute day puzzleType = do
  let fn = inputFileName puzzleType day
  fileExists <- doesFileExist fn
  if not fileExists
    then do
      putStrLn $ red ("puzzel dag " ++ show day ++ ": " ++ "Invoerbestand [" ++ fn ++ "] bestaat nog niet")
    else do
      -- startTime <- getCurrentTime
      contents <- readFile fn
      let solver = (case puzzleType of
                      Example _ solver _  -> solver
                      Puzzle solver _ -> solver)
          input = fromMaybe [] (mapM (parseLine solver) (beforeParse solver (lines contents)))
          n = part solver
          actual = solve solver (afterParse solver input)
          showActual | null input = "[geen of foutieve invoer]"
                     | otherwise = show actual
      case puzzleType of
        Example n _ expected | expected == actual -> putStrLn (green ("Het antwoord van puzzel dag " ++ show day ++ " - test " ++ show n ++ " is: " ++ showActual))
        Example n _ expected | expected /= actual -> putStrLn (red ("Het antwoord van puzzel dag " ++ show day ++ " - test " ++ show n ++ " is: " ++ showActual ++ ", maar moet zijn: " ++ show expected))
        Example {} -> return () -- keep compiler happy
        Puzzle _ answer ->
          case answer of
            Just expected | expected == actual -> putStrLn (green ("Het antwoord van puzzel dag " ++ show day ++ " - deel " ++ show n ++ " is: " ++ showActual))
            Just expected | expected /= actual -> putStrLn (red ("het antwoord van puzzel dag " ++ show day ++ " - deel " ++ show n ++ " is: " ++ showActual ++ ", maar moet zijn: " ++ show expected))
            Just {} -> return () -- keep compiler happy
            Nothing -> putStrLn ("Het vermoedelijke antwoord van puzzel dag " ++ show day ++ " - deel " ++ show n ++ " is: " ++ showActual)
      -- endTime <- getCurrentTime
      -- putStrLn("Execution Time: " ++ show (diffUTCTime endTime startTime))

solveDay :: (Show b, Eq b) => Int -> [PuzzleType a b] -> IO ()
solveDay day problems =
  putStrLn ("Dag " ++ show day) >>
  mapM_ (execute day) problems

solveParts :: (Show b, Show c, Eq b, Eq c) => Int -> [PuzzleType a b] -> [PuzzleType a c] -> IO ()
solveParts day part1Problems part2Problems =
  putStrLn ("Dag " ++ show day) >>
  mapM_ (execute day) part1Problems >>
  mapM_ (execute day) part2Problems


-- Misc Utility Functions

bold :: String -> String
bold str = "\ESC[1m" ++ str ++ "\ESC[0m"

gray :: String -> String
gray str = "\ESC[2m" ++ str ++ "\ESC[0m"

colorize :: String -> TerminalColor -> String
colorize str terminalColor = "\ESC[" ++ show (30 + fromEnum terminalColor) ++ "m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = colorize str Red

green :: String -> String
green str = colorize str Green

testTerminal :: IO ()
testTerminal = clearTerminal >>
  putStrLn "Advent of Code" >>
  putStrLn (green "Advent of Code (green)") >>
  putStrLn (red "Advent of Code (red)") >>
  putStrLn (gray "Advent of Code (gray)") >>
  putStrLn (bold "Advent of Code (bold)")

blankLine :: Int -> IO ()
blankLine n = replicateM_ n (putStrLn "")

boldLine :: String -> IO ()
boldLine str = putStrLn (bold str)

clearTerminal :: IO ()
clearTerminal = putStr "\ESCc"

expectedValue :: a -> a
expectedValue = id

strToMaybeInt :: String -> Maybe Int
strToMaybeInt ('+':s) = Text.Read.readMaybe s
strToMaybeInt s = Text.Read.readMaybe s


breakFoldWithIndex step initialValue exitCondition exitFunction list =
  either id (exitFunction (length  list) (last list)) (foldM f initialValue (zip [0..] list))
    where f acc (index,x)
            | exitCondition index x acc = Left (exitFunction index x acc)
            | otherwise = Right (step index x acc)

breakFold step initialValue exitCondition exitFunction list =
  either id (exitFunction (last list)) (foldM f initialValue list)
    where f acc x
            | exitCondition x acc = Left (exitFunction x acc)
            | otherwise = Right (step x acc)


countMap :: Ord a => [a] -> Map a Int
countMap = foldr upsert Data.Map.empty
  where upsert a = insertWith (+) a 1

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead as = Just (head as)

t3_1 :: (a, b, c) -> a
t3_1 (value, _, _) = value

t3_2 :: (a, b, c) -> b
t3_2 (_, value, _) = value

t3_3 :: (a, b, c) -> c
t3_3 (_, _, value) = value



