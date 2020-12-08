#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package containers
{-# LANGUAGE OverloadedStrings #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                , words
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , words
                                                , unpack
                                                )
import           Data.Either                    ( either
                                                , rights
                                                )
import           Data.Text.IO                   ( readFile )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )

data Instruction = Instruction { operation :: !String
                               , argument :: !Int
                               } deriving (Show, Eq)

solve :: Text -> Either Int Int
solve input = run $ parseInput input

run :: Vector Instruction -> Either Int Int
run = go 0 0 Map.empty
  where
    go acc i visitedMap instructions | i >= V.length instructions = Right acc
                                     | i `Map.member` visitedMap  = Left acc
                                     | otherwise                  = go newAcc newI newVisitedMap instructions
      where
        Instruction { operation = op, argument = arg } = instructions V.! i
        newAcc | op == "acc" = acc + arg
               | otherwise   = acc
        newI | op == "jmp" = i + arg
             | otherwise   = i + 1
        newVisitedMap = Map.insertWith (+) i 1 visitedMap

solve2 :: Text -> Int
solve2 input = head $ rights (run <$> allPossibleMutations)
  where
    instructions         = parseInput input
    allPossibleMutations = withFlippedInstruction <$> operationsToChange
    withFlippedInstruction (i, "nop") =
        instructions V.// [(i, Instruction { operation = "jmp", argument = argument (instructions V.! i) })]
    withFlippedInstruction (i, "jmp") =
        instructions V.// [(i, Instruction { operation = "nop", argument = argument (instructions V.! i) })]
    withFlippedInstruction (i, _) = instructions
    operationsToChange = filter ((`elem` ["jmp", "nop"]) . snd) $ zip [0 ..] (operation <$> V.toList instructions)

parseInput :: Text -> Vector Instruction
parseInput input = V.fromList (parseLine <$> lines input)
  where
    parseLine = parseWords . words
    parseWords (operation : argument : _) =
        Instruction { operation = unpack operation, argument = read $ filter (/= '+') $ unpack argument }

main = do
    input        <- readFile "inputs/day08.txt"
    exampleInput <- readFile "inputs/day08_example.txt"
    runTestTT $ TestCase $ do
        parseInput exampleInput @?= V.fromList
            [ Instruction { operation = "nop", argument = 0 }
            , Instruction { operation = "acc", argument = 1 }
            , Instruction { operation = "jmp", argument = 4 }
            , Instruction { operation = "acc", argument = 3 }
            , Instruction { operation = "jmp", argument = -3 }
            , Instruction { operation = "acc", argument = -99 }
            , Instruction { operation = "acc", argument = 1 }
            , Instruction { operation = "jmp", argument = -4 }
            , Instruction { operation = "acc", argument = 6 }
            ]
        solve exampleInput @?= Left 5
        solve input @?= Left 1614
        solve2 exampleInput @?= 8
        solve2 input @?= 1260
