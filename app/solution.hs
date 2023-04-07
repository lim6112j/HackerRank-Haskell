import Control.Monad (forM, liftM, liftM2)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

makeLoopFor :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
makeLoopFor (i, j) (m, n) = 
    fromTo i0 i1 `product` [j0]   ++   [i1] `product` fromTo j0 j1  ++
    fromTo i1 i0 `product` [j1]   ++   [i0] `product` fromTo j1 j0
    where (i0, i1) = (l, m - l - 1) -- i bounds
          (j0, j1) = (l, n - l - 1) -- j bounds
          l = minimum [i, (m - i - 1), j, (n - j - 1)] -- loop Index
          product = liftM2 (,)
          fromTo a b = [a, a + s .. b - s] where s = signum $ b - a

rotateLayer :: [[Int]] -> Int -> Int -> Int -> [[Int]]
rotateLayer matrix m n r =
    for [0 .. m - 1] $ \i ->
        for [0 .. n - 1] $ \j ->
            let pos = (i, j)
                loop = makeLoopFor pos (m, n)
                index = fromJust $ elemIndex pos $ loop
                (i', j') = loop !! mod (index - r) (length loop) in
            matrix !! i' !! j'
    where for = flip map

main :: IO ()
main = do
    [m, n, r] <- readIntList -- rows, cols, rot
    matrix <- sequence $ replicate m readIntList
    let rotated = rotateLayer matrix m n r
    putStr . unlines . map (unwords . map show) $ rotated
    where readIntList = (map read . words) `liftM` getLine
