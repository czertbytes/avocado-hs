module QPCR 
    ( avgAndStdDev
    ) where

import Data.List.Split
import System.IO
import Data.Char(toUpper)

mean :: (Floating a) => [a] -> a
mean xs = mean' 0 0 xs
    where
      mean' :: (Floating a) => a -> Int -> [a] -> a
      mean' sum len []     = sum / fromIntegral len
      mean' sum len (x:xs) = mean' (sum + x) (succ len) xs

avgAndStdDev :: (Floating a) => [a] -> (a,a)
avgAndStdDev xs = (avg,stdDev)
    where
        avg = mean xs
        square x = x * x
        stdDev = sqrt . mean $ map (square . subtract avg) xs

loadCSV :: String -> IO ()
loadCSV path = do
    hFile <- openFile path ReadMode
    contents <- hGetContents hFile
    let csvLines = lines contents
    print $ process csvLines
    hClose hFile

process :: [String] -> Int
process xs = length (xs !! 0)
