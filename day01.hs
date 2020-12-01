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

sum2020Mul :: [Integer] -> Integer
sum2020Mul = uncurry (*) . head . filter ((== 2020) . uncurry (+)) . selfCartProd

threeSum2020Mul :: [Integer] -> Integer
threeSum2020Mul = product . head . filter ((== 2020) . sum) . threeSelfCartProd

threeSelfCartProd :: [a] -> [[a]]
threeSelfCartProd xs = [[x, y, z] | x <- xs, y <- xs, z <- xs]

selfCartProd :: [a] -> [(a, a)]
selfCartProd xs = cartProd xs xs

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

solve :: Text -> Integer
solve input = sum2020Mul $ parseInput input

solve2 :: Text -> Integer
solve2 input = threeSum2020Mul $ parseInput input

parseInput :: Text -> [Integer]
parseInput input = readEithers (parseLine <$> lines input)
    where
        readEithers = either (throw . PatternMatchFail . show) id . sequenceA
        parseLine = P.parse parser "" . unpack
        lexer = PT.makeTokenParser haskellDef
        integer = PT.integer lexer
        parser = integer

main = do
    input <- readFile "inputs/day01.txt"
    runTestTT $ TestCase $ do
     sum2020Mul [1721, 979, 366, 299, 675, 1456] @?= 514579
     solve input @?= 926464
     solve2 input @?= 65656536
