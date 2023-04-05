{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

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
import qualified Control.Monad as Data.List

--
-- Complete the 'matrixRotation' function below.
--
-- The function accepts following parameters:
--  1. 2D_INTEGER_ARRAY matrix
--  2. INTEGER r
--
-- 1 2 3 4
-- 5 6 7 8
-- 9 10 11 12
-- 13 14 15 16
--
-- 1 rotation
-- 2 3 4 8
-- 1 7 11 12
-- 5 6 10 16
-- 9 13 14 15
--
-- 2 rotation
-- 3 4 8 12
-- 2 11 10 16
-- 1 7 6 15
-- 5 9 13 14
--
-- 3 rotation
--
-- 4 8 12 16
-- 3 10 6 15
-- 2 11 7 14
-- 1 5 9 13
--
--new arr
--  1  2  3  4  5  6
--  7  8  9 10 11 12
-- 13 14 15 16 17 18
-- 19 20 21 22 23 24
-- 25 26 27 28 29 30
-- 31 32 33 34 35 36
--
-- 1 rotation
--
--  2  3  4  5  6 12
--  1  9 10 11 17 18
--  7  8 16 22 23 24
-- 13 14 15 21 29 30 
-- 19 20 26 27 28 36
-- 25 31 32 33 34 35 
--
fnLeft :: (Int, Int) -> Int -> (Int, Int) -> (Int, Int)
fnLeft (m, n) minValue (i, j)
  | i == minValue && j == minValue = (minValue, minValue+1)
  | i == minValue && j == n = (minValue+1, n)
  | i == minValue  = (minValue, j+1)
  | i == m && j == n = (m, n-1)
  | i == m && j == minValue = (m-1, minValue)
  | i == m  = (m, j-1)
  | j == minValue = (i-1, minValue)
  | j == n = (i+1, n)
  | otherwise = fnLeft (m-1, n-1) (minValue + 1) (i, j)
listRotate :: Array (Int, Int) Int -> Int -> Array (Int, Int) Int
listRotate listarray r = do
  let arrBounds = Data.Array.bounds listarray

  let res | r == 0 = listarray
          | otherwise = listRotate (ixmap arrBounds (fnLeft (snd arrBounds) 0) listarray)  (r - 1)
  res
printListWitoutBrackets :: [Int] -> IO()
printListWitoutBrackets aList =  putStrLn $ (Data.List.unwords . Data.List.map show) aList
printMatrix :: Int -> Int -> Array (Int, Int) Int -> IO()
printMatrix m n matrix  = do
  -- matrix row by row list
  let listMatrix = Data.List.map (\x -> Data.List.map (matrix!) x) (Data.List.map (\x -> Data.List.map (\y -> (x,y)) [0..n-1]) [0..m-1])
  mapM_ printListWitoutBrackets listMatrix

matrixRotation matrix r = do
    -- Write your code here
    let m = Data.List.length matrix
    let n = Data.List.length (Data.List.head matrix)
    let matrix' = listArray ((0,0),(m-1,n-1)) (Data.List.concat matrix)
    let listMatrix' = listRotate matrix' r
    printMatrix m n listMatrix'

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray (n - 1)
    return (line : rest)

main :: IO()
main = do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let m = read (firstMultipleInput !! 0) :: Int
    -- print with text
    print ("m: " ++ show m)
    let n = read (firstMultipleInput !! 1) :: Int
    print ("n: " ++ show n)
    let r = read (firstMultipleInput !! 2) :: Int
    print ("r: " ++ show r)
    matrixTemp <- readMultipleLinesAsStringArray m
    --let r = 2 :: Int
    --let matrixTemp = ["1 2 3 4", "5 6 7 8", "9 10 11 12", "13 14 15 16"]
    --let matrixTemp = ["1 2 3 4 5 6", "7 8 9 10 11 12", "13 14 15 16 17 18", "19 20 21 22 23 24", "25 26 27 28 29 30", "31 32 33 34 35 36"]
    let matrix = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) matrixTemp

    matrixRotation matrix r
