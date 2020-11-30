#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package parsec
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, lines)
import Data.Text (Text, lines, unpack)
import Data.Text.IO (readFile)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT
import Data.Traversable (sequenceA)
import Data.Either (either)
import Control.Exception (PatternMatchFail(..), throw)

import Test.HUnit.Text (runTestTT)
import Test.HUnit.Base (Test(TestCase), (@?=))

fuelReq :: Integer -> Integer
fuelReq mass = (mass `div` 3) - 2

recFuelReq :: Integer -> Integer
recFuelReq = recFuelReq' 0
recFuelReq' :: Integer -> Integer -> Integer
recFuelReq' acc mass
  | fuelMass > 0 = recFuelReq' (acc + fuelMass) fuelMass
  | otherwise = acc
  where
      fuelMass = fuelReq mass

solve :: Text -> Integer
solve input = sum $ fuelReq <$> parseInput input

solve2 :: Text -> Integer
solve2 input = sum $ recFuelReq <$> parseInput input

parseInput :: Text -> [Integer]
parseInput input = readEithers (parseLine <$> lines input)
    where
        readEithers = either (throw . PatternMatchFail . show) id . sequenceA
        parseLine = P.parse parser "" . unpack
        lexer = PT.makeTokenParser haskellDef
        integer = PT.integer lexer
        parser = integer

main = do
    input <- readFile "inputs/prep.txt"
    runTestTT $ TestCase $ do
     fuelReq 12 @?= 2
     fuelReq 14 @?= 2
     fuelReq 1969 @?= 654
     fuelReq 100756 @?= 33583
     solve input @?= 3249817
     recFuelReq 14 @?= 2
     recFuelReq 100756 @?= 50346
     solve2 input @?= 4871866
