#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.List                      ( minimum
                                                , maximum
                                                , inits
                                                , tails
                                                )
import           Data.Maybe                     ( catMaybes )

solve2 :: Int -> Text -> Int
solve2 preambleLength input = minimum matchingList + maximum matchingList
  where
    numbers      = parseInput input
    goal         = solve preambleLength input
    matchingList = firstContigiuousPrefixSum numbers goal

firstContigiuousPrefixSum :: [Int] -> Int -> [Int]
firstContigiuousPrefixSum xs goal = head $ catMaybes ((`contigiuousPrefixSum` goal) <$> tails xs)

contigiuousPrefixSum :: [Int] -> Int -> Maybe [Int]
contigiuousPrefixSum [] _    = Nothing
contigiuousPrefixSum xs goal = go (inits xs)
  where
    go [] = Nothing
    go (ys : rest) | sum ys == goal = Just ys
                   | sum ys > goal  = Nothing
                   | otherwise      = go rest

solve :: Int -> Text -> Int
solve preambleLength input = go numbers (drop preambleLength numbers)
  where
    numbers = parseInput input
    go _ [] = -1
    go previous (x : rest) | sumOfOthers (take preambleLength previous) x = go (drop 1 previous) rest
                           | otherwise = x

sumOfOthers :: [Int] -> Int -> Bool
sumOfOthers xs expectation = go (expectation : xs)
  where
    go [] = False
    go (x : rest) | (expectation - x) `elem` rest = True
                  | otherwise                     = go rest

parseInput :: Text -> [Int]
parseInput = fmap (read . unpack) . lines

main = do
    input        <- readFile "inputs/day09.txt"
    exampleInput <- readFile "inputs/day09_example.txt"
    runTestTT $ TestCase $ do
        sumOfOthers [1, 2] 3 @?= True
        sumOfOthers [1, 3] 3 @?= False
        sumOfOthers [0, 3] 3 @?= True
        sumOfOthers [0, 9] 3 @?= True
        solve 5 exampleInput @?= 127
        solve 25 input @?= 258585477
        contigiuousPrefixSum [1, 2] 3 @?= Just [1, 2]
        contigiuousPrefixSum [1, 2, 5] 3 @?= Just [1, 2]
        contigiuousPrefixSum [1, 3] 3 @?= Nothing
        contigiuousPrefixSum [1, 4] 3 @?= Nothing
        contigiuousPrefixSum [4, 1, 2] 3 @?= Nothing
        firstContigiuousPrefixSum [1, 2] 3 @?= [1, 2]
        firstContigiuousPrefixSum [1, 3] 3 @?= [3]
        firstContigiuousPrefixSum [4, 1, 2] 3 @?= [1, 2]
        firstContigiuousPrefixSum [4, 1, 2] 3 @?= [1, 2]
        firstContigiuousPrefixSum [4, 1, 2, 5] 3 @?= [1, 2]
        solve2 5 exampleInput @?= 62
        solve2 25 input @?= 36981213
