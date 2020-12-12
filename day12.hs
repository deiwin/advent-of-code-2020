#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

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

data PolarDirection = PNorth | PEast | PSouth | PWest deriving (Show, Eq)
data Direction = North | East | South | West | DLeft | DRight | Forward deriving (Show, Eq)
data Instruction = Instruction { direction :: Direction
                               , amount :: Int
                               } deriving (Show, Eq)

type Coord = V2 Int
data State = State { wayPointRel :: Coord
                   , coord :: Coord
                   } deriving (Show, Eq)

move :: State -> Instruction -> State
move state instruction =
    let moveNorth = state { wayPointRel = wayPointRel state + V2 (amount instruction) 0 }
        moveSouth = state { wayPointRel = wayPointRel state + V2 (-1 * amount instruction) 0 }
        moveEast  = state { wayPointRel = wayPointRel state + V2 0 (amount instruction) }
        moveWest  = state { wayPointRel = wayPointRel state + V2 0 (-1 * amount instruction) }
    in  case direction instruction of
            North   -> moveNorth
            South   -> moveSouth
            East    -> moveEast
            West    -> moveWest
            Forward -> state { coord = coord state + (wayPointRel state ^* amount instruction) }
            DLeft   -> state { wayPointRel = iterate perp (wayPointRel state) !! ((360 - amount instruction) `div` 90) }
            DRight  -> state { wayPointRel = iterate perp (wayPointRel state) !! ((amount instruction) `div` 90) }

solve :: Text -> Int
solve input = manhattan $ coord finalState
  where
    instructions = parseInput input
    initialState = State { wayPointRel = V2 1 10, coord = V2 0 0 }
    finalState   = foldl' move initialState instructions
    -- initialState = State { facingDirection = PEast, coord = polarMovementCoord }
    -- finalState   = foldl' move initialState dirMovement
    -- polarMovementCoord = foldl' (+) (V2 0 0) (moveVector <$> polarMovement)
    -- moveVector instruction = dirVector (direction instruction) ^* amount instruction
    -- (polarMovement, dirMovement) = partition ((`elem` [North, South, East, West]). direction) instructions
    -- dirVector North = V2 1 0
    -- dirVector South = V2 (-1) 0
    -- dirVector East = V2 0 1
    -- dirVector West = V2 0 (-1)
    manhattan (V2 y x) = abs y + abs x

parseInput :: Text -> [Instruction]
parseInput input = readInstruction . unpack <$> lines input
  where
    readInstruction (d : a) = Instruction { direction = readDirection d, amount = read a }
    readDirection 'N' = North
    readDirection 'S' = South
    readDirection 'E' = East
    readDirection 'W' = West
    readDirection 'L' = DLeft
    readDirection 'R' = DRight
    readDirection 'F' = Forward

main = do
    exampleInput <- readFile "inputs/day12_example.txt"
    input        <- readFile "inputs/day12.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 286
        V2 1 0 ^* 8 @?= V2 8 0
        solve input @?= 39140
