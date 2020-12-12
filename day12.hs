#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

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
import           Data.List                      ( maximum
                                                , sort
                                                , mapAccumL
                                                , groupBy
                                                , foldl'
                                                , cycle
                                                , partition
                                                , iterate
                                                )
import           Linear.V2                      ( V2(..)
                                                , perp
                                                )
import           Linear.Vector                  ( (^*) )

data CardinalDirection = North | East | South | West deriving (Show, Eq)
data Rotation = Clockwise | CounterClockwise deriving (Show, Eq)
data Direction = C CardinalDirection | R Rotation | Forward deriving (Show, Eq)
data Instruction = Instruction { direction :: Direction
                               , amount :: Int
                               } deriving (Show, Eq)

type Coord = V2 Int
data State2 = State2 { wayPointRel :: Coord
                     , coord2 :: Coord
                     } deriving (Show, Eq)

cardinalVector :: CardinalDirection -> V2 Int
cardinalVector = \case
    North -> V2 1 0
    South -> V2 (-1) 0
    East  -> V2 0 1
    West  -> V2 0 (-1)

clockwiseRotation :: Rotation -> Int -> Int
clockwiseRotation Clockwise        x = x
clockwiseRotation CounterClockwise x = 360 - (x `mod` 360)

move2 :: State2 -> Instruction -> State2
move2 state instruction = case direction of
    C carDir   -> state { wayPointRel = wayPointRel + (cardinalVector carDir ^* amount) }
    R rotation -> state { wayPointRel = clockwiseRotatedWayPointRel (clockwiseRotation rotation amount) }
    Forward    -> state { coord2 = coord2 + (wayPointRel ^* amount) }
  where
    State2 { wayPointRel = wayPointRel, coord2 = coord2 }  = state
    Instruction { direction = direction, amount = amount } = instruction
    clockwiseRotatedWayPointRel degrees = iterate perp wayPointRel !! (degrees `div` 90)

solve2 :: Text -> Int
solve2 input = manhattan $ coord2 finalState
  where
    instructions = parseInput input
    initialState = State2 { wayPointRel = V2 1 10, coord2 = V2 0 0 }
    finalState   = foldl' move2 initialState instructions
    manhattan (V2 y x) = abs y + abs x

parseInput :: Text -> [Instruction]
parseInput input = readInstruction . unpack <$> lines input
  where
    readInstruction (d : a) = Instruction { direction = readDirection d, amount = read a }
    readDirection 'N' = C North
    readDirection 'S' = C South
    readDirection 'E' = C East
    readDirection 'W' = C West
    readDirection 'L' = R CounterClockwise
    readDirection 'R' = R Clockwise
    readDirection 'F' = Forward

main = do
    exampleInput <- readFile "inputs/day12_example.txt"
    input        <- readFile "inputs/day12.txt"
    runTestTT $ TestCase $ do
        solve2 exampleInput @?= 286
        V2 1 0 ^* 8 @?= V2 8 0
        solve2 input @?= 39140
