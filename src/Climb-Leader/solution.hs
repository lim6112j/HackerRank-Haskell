{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main(main) where

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
climbingLeaderboard :: [Int] -> [Int] -> [Int]
climbingLeaderboard = flip g . Data.List.reverse . Data.List.zip [1..] . fmap Data.List.head . Data.List.group
  where
    g [] _      = []
    g [x] []    = [1]
    g (x:xs) [] = 1 : g xs []
    g xs@(x:xs') ts@((i,p):ts') 
        | x < p     = (i+1) : g xs' ts 
        | x == p    = i : g xs' ts
        | otherwise = g xs ts'
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
 
