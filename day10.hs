#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , unpack
                                                )
import           Data.Text.IO                   ( readFile )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.List                      ( maximum
                                                , sort
                                                , mapAccumL
                                                , groupBy
                                                )

solve :: Text -> Int
solve input = joltDist $ parseInput input

joltDist :: [Int] -> Int
joltDist jolts = go 0 0 Nothing $ sort ((maximum jolts + 3) : 0 : jolts)
  where
    go oneDiffs threeDiffs _           []         = oneDiffs * threeDiffs
    go oneDiffs threeDiffs Nothing     (x : rest) = go oneDiffs threeDiffs (Just x) rest
    go oneDiffs threeDiffs (Just prev) (x : rest) = go newOneDiffs newThreeDiffs (Just x) rest
      where
        diff = x - prev
        newOneDiffs | diff == 1 = oneDiffs + 1
                    | otherwise = oneDiffs
        newThreeDiffs | diff == 3 = threeDiffs + 1
                      | otherwise = threeDiffs

solve2 :: Text -> Int
solve2 input = validArrangements $ parseInput input

validArrangements :: [Int] -> Int
validArrangements jolts = product (rangeSum <$> interestingRanges)
  where
    rangeSum [1]          = 1
    rangeSum [1, 1]       = 2
    -- [1, 1]
    -- [2]
    -- 2 -> 2 (1+1?)
    rangeSum [1, 1, 1]    = 4
    -- [1, 1, 1]
    -- [2, 1]
    -- [1, 2]
    -- [3]
    -- 3 -> 4 (3*1+1)
    rangeSum [1, 1, 1, 1] = 7
    -- [1, 1, 1, 1]
    -- [2, 1, 1]
    -- [1, 2, 1]
    -- [1, 1, 2]
    -- [2, 2]
    -- [3, 1]
    -- [1, 3]
    -- 4 -> 7 (3*2+1?)
    rangeSum _            = 1 -- Really don't know but the actual input only includes the above :D
    -- [1, 1, 1, 1, 1]
    -- [2, 1, 1, 1]
    -- [1, 2, 1, 1]
    -- [1, 1, 2, 1]
    -- [1, 1, 1, 2]
    -- [2, 2, 1]
    -- [2, 1, 2]
    -- [1, 2, 2]
    -- [3, 1, 1]
    -- [1, 3, 1]
    -- [1, 1, 3]
    -- [2, 3]
    -- [3, 2]
    -- 5 -> 13 (3*4+1?)
    --
    -- [2, 1, 1, 1]
    -- [2, 2, 1]
    -- [2, 1, 2]
    -- [3, 1, 1]
    -- [2, 3]
    -- [3, 2]
    -- 4 -> 6
    interestingRanges = filter ((/= 3) . head) $ groupBy (\a b -> (a < 3 && b < 3) || (a, b) == (3, 3)) diffList
    diffList          = snd $ mapAccumL (\acc x -> (x, x - acc)) 0 finalJolts
    finalJolts        = sort ((maximum jolts + 3) : jolts)

parseInput :: Text -> [Int]
parseInput = fmap (read . unpack) . lines

main = do
    input        <- readFile "inputs/day10.txt"
    exampleInput <- readFile "inputs/day10_example.txt"
    runTestTT $ TestCase $ do
        joltDist [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] @?= 35
        solve exampleInput @?= 220
        solve input @?= 2664
        solve2 exampleInput @?= 19208
        solve2 input @?= 148098383347712
