#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package parsec --package array --package linear
{-# LANGUAGE OverloadedStrings #-}

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
                                                )

import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )

data Cell = Tree | Open deriving (Show, Eq)
type Coord = V2 Int
type Slope = V2 Int
type Grid = Arr.Array Coord Cell

countTrees :: Slope -> Grid -> Int
countTrees = countTrees' 0 (V2 0 0)
countTrees' :: Int -> Coord -> Slope -> Grid -> Int
countTrees' acc ix slope grid | inRange bounds ix = countTrees' newAcc newIx slope grid
                              | otherwise         = acc
  where
    (_, V2 _ maxX)     = Arr.bounds grid
    (V2 y      x     ) = ix
    (V2 slopeY slopeX) = slope
    newIx              = V2 (y + slopeY) newX
    newX               = (x + slopeX) `mod` (maxX + 1)
    newAcc             = case grid Arr.! ix of
        Tree -> acc + 1
        Open -> acc
    bounds = Arr.bounds grid

solve :: Text -> Int
solve input = countTrees (V2 1 3) $ createGrid $ parseInput input

solve2 :: Text -> Int
solve2 input = product ((`countTrees` grid) <$> slopes)
  where
    slopes = [V2 1 1, V2 1 3, V2 1 5, V2 1 7, V2 2 1]
    grid   = createGrid $ parseInput input

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
    parser      = P.many1 (readCell <$> (symbol "#" P.<|> symbol "."))
    readCell "#" = Tree
    readCell "." = Open

main = do
    exampleInput <- readFile "inputs/day03_example.txt"
    input        <- readFile "inputs/day03.txt"
    runTestTT $ TestCase $ do
        1 @?= 1
        parseInput "#." @?= [[Tree, Open]]
        solve "..#.\n.#.#" @?= 1
        solve "..#.\n.#.." @?= 0
        ((3 + 3) `mod` 4) @?= 2
        solve "..#.\n.#.#\n.#.#" @?= 1
        solve "..#.\n.#.#\n.###" @?= 2
        createGrid (parseInput "..#.\n.#.#\n.###") Arr.! V2 0 0 @?= Open
        createGrid (parseInput "..#.\n.#.#\n.###") Arr.! V2 0 1 @?= Open
        createGrid (parseInput "..#.\n.#.#\n.###") Arr.! V2 0 2 @?= Tree
        solve exampleInput @?= 7
        solve input @?= 286
        solve2 exampleInput @?= 336
        solve2 input @?= 3638606400
