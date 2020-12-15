#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude                 hiding ( readFile )
import           Data.Text                      ( Text
                                                , unpack
                                                , strip
                                                )
import           Data.Text.IO                   ( readFile )
import qualified Text.Parsec                   as P
import qualified Text.Parsec.Char              as PC
import           Data.Either                    ( either )
import           Control.Exception              ( PatternMatchFail(..)
                                                , throw
                                                )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.List                      ( foldl'
                                                , elemIndex
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as M
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as S
import           Data.Bits                      ( setBit
                                                , clearBit
                                                )
import           Criterion.Main                 ( defaultMain
                                                , bench
                                                , whnf
                                                )
import           Debug.Trace                    ( traceShowId )

memoryGame :: [Int] -> [Int]
memoryGame []            = []
memoryGame xs@(x : rest) = case elemIndex x rest of
    Nothing -> 0 : memoryGame (0 : xs)
    Just i  -> (i + 1) : memoryGame ((i + 1) : xs)

nthMemoryGame :: Int -> [Int] -> Int
nthMemoryGame n start = go initialIx (last start) initialMap
    where
        -- key: the number
        -- val: its most recent index
        initialMap = M.fromList $ zip (init start) [0..]
        initialIx = length start
        go :: Int -> Int -> IntMap Int -> Int
        go ix lastVal map
          | ix == (n - 1) = newVal
          | otherwise = go (ix + 1) newVal newMap
            where
                (maybeLastIx, newMap) = M.insertLookupWithKey constWithKey lastVal (ix - 1) map
                constWithKey k new old = new
                newVal = case maybeLastIx of
                           Nothing -> 0
                           Just lastIx -> ix - lastIx - 1

solve :: [Int] -> Int
solve = nthMemoryGame 2020

solve2 :: [Int] -> Int
solve2 = nthMemoryGame 30000000

main = do
    let input = [8, 11, 0, 19, 1, 2]
    runTestTT $ TestCase $ do
        take 10 (memoryGame [6, 3, 0]) @?= [0, 3, 3, 1, 0, 4, 0, 2, 0, 2]
        nthMemoryGame 9 [0, 3, 6] @?= 4
        nthMemoryGame 10 [0, 3, 6] @?= 0
        solve [1, 3, 2] @?= 1
        solve [2, 1, 3] @?= 10
        solve [1, 2, 3] @?= 27
        solve [2, 3, 1] @?= 78
        solve [3, 2, 1] @?= 438
        solve [3, 1, 2] @?= 1836
        solve input @?= 447
        -- solve2 [0, 3, 6] @?= 175593
        -- solve2 input @?= 447
    defaultMain
        [ bench "10^3" (whnf (nthMemoryGame 1000) input)
        , bench "10^4" (whnf (nthMemoryGame 10000) input)
        , bench "10^5" (whnf (nthMemoryGame 100000) input)
        ]
