{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

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

--
-- Complete the 'matchingStrings' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. STRING_ARRAY stringList
--  2. STRING_ARRAY queries
--
matchingStrings :: [String] -> [String] -> [Int]
matchingStrings stringList queries = do
  let ch = Data.List.foldr (++) "" stringList
  [1, 2, 3]

-- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray (n - 1)
  return (line : rest)

main :: IO ()
main = do
  print $ matchingStrings ["ab", "ab", "abc"] ["ab", "abc", "bc"]

-- stdout <- getEnv "OUTPUT_PATH"
-- fptr <- openFile stdout WriteMode
--
-- stringListCountTemp <- getLine
-- let stringListCount = read $ lstrip $ rstrip stringListCountTemp :: Int
--
-- stringList <- readMultipleLinesAsStringArray stringListCount
--
-- queriesCountTemp <- getLine
-- let queriesCount = read $ lstrip $ rstrip queriesCountTemp :: Int
--
-- queries <- readMultipleLinesAsStringArray queriesCount
--
-- let res = matchingStrings stringList queries
--
-- hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map (\x -> show x) $ res
--
-- hFlush fptr
-- hClose fptr
