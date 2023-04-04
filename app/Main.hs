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

--
-- Complete the 'matrixRotation' function below.
--
-- The function accepts following parameters:
--  1. 2D_INTEGER_ARRAY matrix
--  2. INTEGER r
--

matrixRotation matrix r = do
    -- Write your code here
    let m = Data.List.length matrix
    let n = Data.List.length (Data.List.head matrix)
    let min = Prelude.min m n
    let max = Prelude.max m n
    let layers = min `div` 2
    let matrix' = listArray ((0,0),(m-1,n-1)) (Data.List.concat matrix)
    print matrix'

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
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
    let matrix = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) matrixTemp

    matrixRotation matrix r
