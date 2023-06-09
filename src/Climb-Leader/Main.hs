{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Ord
import qualified Data.Maybe

--
-- Complete the 'climbingLeaderboard' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER_ARRAY ranked
--  2. INTEGER_ARRAY player
returnResult :: Maybe [Int] -> [Int]
returnResult res
  | res == Nothing = []
  | otherwise = Data.Maybe.fromJust res 

-- TODO binary search
customFindIndex :: (a -> Bool) -> [a] -> Maybe Int
customFindIndex _ [] = Nothing
customFindIndex f ls = 
  case Data.List.findIndex f ls of
    Nothing -> Just $ Data.List.length ls + 1
    Just x -> Just (x + 1)
climbingLeaderboard :: [Int] -> [Int] -> Maybe [Int]
climbingLeaderboard ranked player = do
  let filterdList = Data.Set.toList $ Data.Set.fromList ranked
      finalMerged = Data.List.sortBy (\a b -> compare (Data.Ord.Down a) (Data.Ord.Down b) ) filterdList
  --Just finalMerged
  sequence $ fmap (\x -> (customFindIndex (<= x)) finalMerged)  player
  
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
  print $ climbingLeaderboard [100, 100, 50, 40, 40, 20, 10]  [5, 25, 50, 120]
  -- 6 4 2 1
    --stdout <- getEnv "OUTPUT_PATH"
    --fptr <- openFile stdout WriteMode

    --rankedCountTemp <- getLine
    --let rankedCount = read $ lstrip $ rstrip rankedCountTemp :: Int

    --rankedTemp <- getLine

    --let ranked = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip rankedTemp

    --playerCountTemp <- getLine
    --let playerCount = read $ lstrip $ rstrip playerCountTemp :: Int

    --playerTemp <- getLine

    --let player = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip playerTemp

    --let result = climbingLeaderboard ranked player

    --hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map (\x -> show x) $ returnResult result

    --hFlush fptr
    --hClose fptr
