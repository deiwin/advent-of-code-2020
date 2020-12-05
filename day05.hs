#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package containers
{-# LANGUAGE OverloadedStrings #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , unpack
                                                )
import           Data.Text.IO                   ( readFile )
import           Data.List                      ( maximum )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Debug.Trace                    ( traceShow )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

seatID :: String -> Int
seatID pass = (row * 8) + col
  where
    col                = bSearch 0 7 ((== 'R') <$> colPass)
    row                = bSearch 0 127 ((== 'B') <$> rowPass)
    (rowPass, colPass) = splitAt 7 pass

bSearch :: Int -> Int -> [Bool] -> Int
bSearch bot top [] = bot
bSearch bot top (pickHigher : rest) | pickHigher = bSearch (split + 1) top rest
                                    | otherwise  = bSearch bot split rest
    where split = bot + ((top - bot) `div` 2)

solve :: Text -> Int
solve input = maximum (seatID . unpack <$> lines input)

solve2 :: Text -> Int
solve2 input = mySeat
  where
    takenSeats   = seatID . unpack <$> lines input
    takenSeatSet = Set.fromList takenSeats
    missingKeys  = filter (not . (`Set.member` takenSeatSet)) [0 .. (127 * 8 + 7)]
    mySeat       = head $ filter hasNeighbours missingKeys
    hasNeighbours seat = (seat - 1) `Set.member` takenSeatSet && (seat + 1) `Set.member` takenSeatSet

main = do
    input <- readFile "inputs/day05.txt"
    runTestTT $ TestCase $ do
        7 `div` 2 @?= 3
        bSearch 0 7 [True, False, True] @?= 5
        seatID "FBFBBFFRLR" @?= 357
        solve input @?= 826
        solve2 input @?= 678
