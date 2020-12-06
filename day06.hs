#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package containers
{-# LANGUAGE OverloadedStrings #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , unpack
                                                , splitOn
                                                )
import           Data.Text.IO                   ( readFile )
import           Data.List                      ( nub )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )

countUnique :: [String] -> Int
countUnique = Set.size . foldl1 Set.union . fmap Set.fromList

countAll :: [String] -> Int
countAll = Set.size . foldl1 Set.intersection . fmap Set.fromList

solve :: Text -> Int
solve input = sum (countUnique . fmap unpack . lines <$> splitOn "\n\n" input)

solve2 :: Text -> Int
solve2 input = sum (countAll . fmap unpack . lines <$> splitOn "\n\n" input)

main = do
    input <- readFile "inputs/day06.txt"
    runTestTT $ TestCase $ do
        countUnique ["abbc"] @?= 3
        countUnique ["abbc", "a"] @?= 3
        countUnique ["abbc", "ad"] @?= 4
        solve input @?= 6703
        countAll ["abc"] @?= 3
        countAll ["abc", "a"] @?= 1
        countAll ["abc", "ad"] @?= 1
        solve2 "abc" @?= 3
        solve2 "a\nb\nc" @?= 0
        solve2 "abc\n\na\nb\nc" @?= 3
        solve2 input @?= 3430
