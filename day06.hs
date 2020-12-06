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
import           Data.List                      ( sortOn
                                                , sort
                                                , group
                                                , nub
                                                )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Debug.Trace                    ( traceShow )
import           Data.Char                      ( isLetter )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Arrow                  ( (>>>) )

countUnique :: String -> Int
countUnique = filter isLetter
          >>> Set.fromList
          >>> Set.size

countAll :: [String] -> Int
countAll answers = length $ filter f $ nub $ concat answers
    where f answ = all (elem answ) answers

solve :: Text -> Int
solve input = sum (countUnique . unpack <$> splitOn "\n\n" input)

solve2 :: Text -> Int
solve2 input = sum (countAll . fmap unpack . lines <$> splitOn "\n\n" input)

main = do
    input <- readFile "inputs/day06.txt"
    runTestTT $ TestCase $ do
        countUnique "abbc" @?= 3
        countUnique "abbc\na" @?= 3
        countUnique "abbc\nad" @?= 4
        solve input @?= 6703
        countAll ["abc"] @?= 3
        countAll ["abc", "a"] @?= 1
        countAll ["abc", "ad"] @?= 1
        solve2 "abc" @?= 3
        solve2 "a\nb\nc" @?= 0
        solve2 "abc\n\na\nb\nc" @?= 3
        solve2 input @?= 3430
