module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import System.IO
import Control.Monad


main = do
    cnt <- B.readFile "in.csv" 
    let sections = tokenise (BC.pack "\r\n\r\n")cnt
    BC.putStr $ sections !! 10


tokenise :: B.ByteString -> B.ByteString -> [B.ByteString]
tokenise x y = h : if B.null t then [] else tokenise x (B.drop (B.length x) t)
    where (h,t) = B.breakSubstring x y

