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
import           Data.List                      ( minimumBy )
import           Data.Ord                       ( comparing )

solve :: Text -> Int
solve input = earliestBusId * waitTime
  where
    (begin        , ids     ) = parseInput input
    (earliestBusId, waitTime) = minimumBy (comparing snd) $ zip ids ((begin `modInverse`) <$> ids)

parseInput :: Text -> (Int, [Int])
parseInput input = parse $ lines input
  where
    parse (begin : ids : _) = (read (unpack begin), parseIds ids)
    parseIds ids = read . unpack <$> filter (/= "x") (splitOn "," ids)

modInverse :: Int -> Int -> Int
modInverse a b = b - (a `mod` b)

main = do
    input <- readFile "inputs/day13.txt"
    runTestTT $ TestCase $ do
        939 `mod` 7 @?= 1
        939 `modInverse` 7 @?= 6
        (939 `modInverse`) <$> [7, 13, 59, 31, 19] @?= [6, 10, 5, 22, 11]
        solve input @?= 2845
