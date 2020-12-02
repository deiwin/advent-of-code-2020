#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package parsec
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , unpack
                                                )
import           Data.Text.IO                   ( readFile )
import           Text.Parsec.Language           ( haskellDef )
import qualified Text.Parsec                   as P
import qualified Text.Parsec.Token             as PT
import qualified Text.Parsec.Char              as PC
import           Data.Traversable               ( sequenceA )
import           Data.Either                    ( either )
import           Control.Exception              ( PatternMatchFail(..)
                                                , throw
                                                )

import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )

data Password = Password { lowerBound :: !Integer
                         , upperBound :: !Integer
                         , requiredChar :: !Char
                         , pass :: String
                         } deriving (Show, Eq)

isValid :: Password -> Bool
isValid p = requiredCharCount >= fromInteger (lowerBound p) && requiredCharCount <= fromInteger (upperBound p)
    where requiredCharCount = length $ filter (== requiredChar p) (pass p)

isValid2 :: Password -> Bool
isValid2 p = releventCharCount == 1
  where
    releventCharCount = length $ filter (== requiredChar p) relevantChars
    relevantChars     = [pass p !! (fromInteger (lowerBound p) - 1), pass p !! (fromInteger (upperBound p) - 1)]

solve :: Text -> Int
solve input = length $ filter isValid $ parseInput input

solve2 :: Text -> Int
solve2 input = length $ filter isValid2 $ parseInput input

parseInput :: Text -> [Password]
parseInput input = readEithers (parseLine <$> lines input)
  where
    readEithers   = either (throw . PatternMatchFail . show) id . sequenceA
    parseLine     = P.parse parser "" . unpack
    lexer         = PT.makeTokenParser haskellDef
    integer       = PT.integer lexer
    symbol        = PT.symbol lexer
    whiteSpace    = PT.whiteSpace lexer
    charLiteral   = PT.charLiteral lexer
    stringLiteral = PT.stringLiteral lexer
    parser        = do
        lowerBound   <- integer <* symbol "-"
        upperBound   <- integer
        requiredChar <- whiteSpace *> PC.letter <* symbol ":"
        pass         <- whiteSpace *> P.many1 PC.anyChar
        return Password { .. }

main = do
    input <- readFile "inputs/day02.txt"
    runTestTT $ TestCase $ do
        parseInput "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
            @?= [ Password { lowerBound = 1, upperBound = 3, requiredChar = 'a', pass = "abcde" }
                , Password { lowerBound = 1, upperBound = 3, requiredChar = 'b', pass = "cdefg" }
                , Password { lowerBound = 2, upperBound = 9, requiredChar = 'c', pass = "ccccccccc" }
                ]
        solve "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" @?= 2
        solve input @?= 660
        solve2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" @?= 1
        solve2 input @?= 530
