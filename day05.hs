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
import           Data.Bits                      ( Bits )
import qualified Data.Bits                     as Bits

seatID :: String -> Int
seatID = toBits . fmap (`elem` ("RB" :: String))

toBits :: [Bool] -> Int
toBits bits = toBits' 0 0 $ reverse bits
toBits' :: Int -> Int -> [Bool] -> Int
toBits' bits i []             = fromIntegral bits
toBits' bits i (True  : rest) = toBits' (bits `Bits.setBit` i) (i + 1) rest
toBits' bits i (False : rest) = toBits' bits (i + 1) rest

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
        toBits [True, False, True] @?= 5
        seatID "FBFBBFFRLR" @?= 357
        solve input @?= 826
        solve2 input @?= 678
