{-# LANGUAGE BangPatterns #-}

module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
    let n = 10000000
    print . IS.findMax . IS.filter (< n) . happyNumbers $ n

squareOfDigits :: Int -> Int
squareOfDigits !num = go 0 . quotRem num $ 10
    where
        go !acc (!q, !r)
            | q == 0 = acc +  r * r
            | otherwise =  go (r * r + acc) . quotRem q $ 10

isHappy :: Int -> (Bool, IntSet)
isHappy !n = go n IS.empty
    where
        go !n !seen
            | n == 1 = (True, seen)
            | n `IS.member` seen = (False, seen)
            | otherwise =
                let !square = squareOfDigits n
                in  go square (IS.insert n seen)

--Find all happy-numbers up to n (and some more)
happyNumbers :: Int -> IntSet
happyNumbers !n = go n 1 (IS.singleton 1) IS.empty
    where
        go !n !current !happy !sad
            | current >= n = happy
            | current `IS.member` happy || current `IS.member` sad =
                go n (current + 1) happy sad
            | otherwise =
                let (!ishappy, !seen) = isHappy current
                    !next = current + 1
                in  if ishappy then go n next (happy `IS.union` seen) sad
                    else go n next happy (sad `IS.union` seen)
