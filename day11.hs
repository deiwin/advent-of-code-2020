#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package parsec --package array --package linear
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
import           Linear.V2                      ( V2(..) )
import qualified Data.Array.IArray             as Arr
import           Data.Ix                        ( inRange
                                                , range
                                                )

import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.List                      ( intersperse
                                                , iterate
                                                )

data Cell = Floor | Empty | Occupied deriving (Show, Eq)
type Coord = V2 Int
type Direction = V2 Int
type Grid = Arr.Array Coord Cell

play :: Grid -> Grid
play grid = newGrid
  where
    newGrid = grid Arr.// updates
    bounds  = Arr.bounds grid
    updates = cellUpdate <$> range bounds
    cellUpdate i = (i, newType (grid Arr.! i) ((grid Arr.!) <$> adjacent i))
    newType :: Cell -> [Cell] -> Cell
    newType Empty neighbors | Occupied `notElem` neighbors = Occupied
                            | otherwise                    = Empty
    newType Occupied neighbors | length (filter (== Occupied) neighbors) >= 4 = Empty
                               | otherwise = Occupied
    newType cell _ = cell
    adjacent :: Coord -> [Coord]
    adjacent i = filter
        (inRange bounds)
        ((+ i) <$> [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1, V2 0 (-1), V2 0 1, V2 1 (-1), V2 1 0, V2 1 1])

play2 :: Grid -> Grid
play2 grid = newGrid
  where
    newGrid = grid Arr.// updates
    bounds  = Arr.bounds grid
    updates = cellUpdate <$> range bounds
    cellUpdate i@(V2 y x) = (i, newType (grid Arr.! i) ((grid Arr.!) <$> visible i))
    newType :: Cell -> [Cell] -> Cell
    newType Empty neighbors | Occupied `notElem` neighbors = Occupied
                            | otherwise                    = Empty
    newType Occupied neighbors | length (filter (== Occupied) neighbors) >= 5 = Empty
                               | otherwise = Occupied
    newType cell _ = cell
    directions = [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1, V2 0 (-1), V2 0 1, V2 1 (-1), V2 1 0, V2 1 1]
    visible :: Coord -> [Coord]
    visible i = filter (inRange bounds) (concatMap (firstSeatInDirection i) directions)
    firstSeatInDirection :: Coord -> Direction -> [Coord]
    firstSeatInDirection i dir | not (inRange bounds newI)  = []
                               | (grid Arr.! newI) == Floor = firstSeatInDirection newI dir
                               | otherwise                  = [newI]
        where newI = i + dir

solve :: Text -> Int
solve input = occupiedSeats firstRepeatingState
  where
    occupiedSeats :: Grid -> Int
    occupiedSeats grid = length $ filter (== Occupied) ((grid Arr.!) <$> range (Arr.bounds grid))
    firstRepeatingState = fst $ head $ filter (uncurry (==)) $ zip allStates (tail allStates)
    allStates           = iterate play grid
    grid                = createGrid $ parseInput input

solve2 :: Text -> Int
solve2 input = occupiedSeats firstRepeatingState
  where
    occupiedSeats :: Grid -> Int
    occupiedSeats grid = length $ filter (== Occupied) ((grid Arr.!) <$> range (Arr.bounds grid))
    firstRepeatingState = fst $ head $ filter (uncurry (==)) $ zip allStates (tail allStates)
    allStates           = iterate play2 grid
    grid                = createGrid $ parseInput input

createGrid :: [[Cell]] -> Grid
createGrid rows = Arr.listArray bounds $ concat rows
    where bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))

parseInput :: Text -> [[Cell]]
parseInput input = readEithers (parseLine <$> lines input)
  where
    readEithers = either (throw . PatternMatchFail . show) id . sequenceA
    parseLine   = P.parse parser "" . unpack
    lexer       = PT.makeTokenParser haskellDef
    integer     = PT.integer lexer
    symbol      = PT.symbol lexer
    parser      = P.many1 (readCell <$> (symbol "#" P.<|> symbol "." P.<|> symbol "L"))
    readCell "#" = Occupied
    readCell "." = Floor
    readCell "L" = Empty

showGrid :: Grid -> String
showGrid grid = unlines rows
  where
    bounds            = Arr.bounds grid
    (_, V2 maxY maxX) = bounds
    rows              = showRow <$> [0 .. maxY]
    showRow y = showCell y <$> [0 .. maxX]
    showCell y x = toChar $ grid Arr.! V2 y x
    toChar Empty    = 'L'
    toChar Floor    = '.'
    toChar Occupied = '#'

main = do
    exampleInput <- readFile "inputs/day11_example.txt"
    input        <- readFile "inputs/day11.txt"
    putStrLn $ showGrid $ play2 $ play2 $ createGrid $ parseInput exampleInput
    runTestTT $ TestCase $ do
        createGrid (parseInput exampleInput) == createGrid (parseInput exampleInput) @?= True
        solve exampleInput @?= 37
        solve input @?= 2273
        solve2 exampleInput @?= 26
        solve2 input @?= 2064
