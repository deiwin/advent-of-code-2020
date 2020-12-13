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
                                                , splitOn
                                                )
import           Data.Text.IO                   ( readFile )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.List                      ( minimumBy
                                                , maximumBy
                                                , foldl1'
                                                )
import           Data.Ord                       ( comparing )
import           Control.Arrow                  ( second )

slowEarliestConsecutive :: [(Int, Int)] -> [Int]
slowEarliestConsecutive goals = go (step - firstTime) step
  where
    (firstTime, step) = maximumBy (comparing snd) goals
    go time step | all (uncurry (==) . second (time `modInverse`)) goals = time : go (time + step) step
                 | otherwise = go (time + step) step

reduce :: (Int, Int) -> (Int, Int) -> (Int, Int)
reduce a b = (step - first, step)
  where
    (first : second : _) = slowEarliestConsecutive [a, b]
    step                 = second - first

fastEarliestConsecutive :: [(Int, Int)] -> Int
fastEarliestConsecutive goals = y - x where (x, y) = foldl1' reduce goals

solve :: Text -> Int
solve input = earliestBusId * waitTime
  where
    (begin, ids')             = parseInput input
    ids                       = snd <$> ids'
    (earliestBusId, waitTime) = minimumBy (comparing snd) $ zip ids ((begin `modInverse`) <$> ids)

solve2 :: Text -> Int
solve2 input = fastEarliestConsecutive (normalize <$> goals)
  where
    goals = snd $ parseInput input
    normalize (a, b) = (a `mod` b, b)

parseInput :: Text -> (Int, [(Int, Int)])
parseInput input = parse $ lines input
  where
    parse (begin : ids : _) = (read (unpack begin), parseIds ids)
    parseIds ids = readTuple <$> filter ((/= "x") . snd) (zip [0 ..] (splitOn "," ids))
    readTuple (i, x) = (i, read (unpack x))

modInverse :: Int -> Int -> Int
modInverse a b | (a `mod` b) == 0 = 0
               | otherwise        = b - (a `mod` b)

main = do
    input <- readFile "inputs/day13.txt"
    runTestTT $ TestCase $ do
        939 `mod` 7 @?= 1
        939 `modInverse` 7 @?= 6
        (939 `modInverse`) <$> [7, 13, 59, 31, 19] @?= [6, 10, 5, 22, 11]
        solve input @?= 2845

        take 2 (slowEarliestConsecutive [(0, 7), (1, 13)]) @?= [77, 168]
        reduce (0, 7) (1, 13) @?= (14, 91)
        reduce (14, 91) (4, 59) @?= (5019, 5369)
        reduce (5019, 5369) (6, 31) @?= (96292, 166439)
        reduce (96292, 166439) (7, 19) @?= (2093560, 3162341)
        foldl1' reduce [(0, 7), (1, 13), (4, 59), (6, 31), (7, 19)] @?= (2093560, 3162341)
        3162341 - 2093560 @?= 1068781
        fastEarliestConsecutive [(0, 7), (1, 13), (4, 59), (6, 31), (7, 19)] @?= 1068781
        fastEarliestConsecutive [(0, 7), (1, 13)] @?= 77

        solve2 input @?= 487905974205117
