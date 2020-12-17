#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Prelude                 hiding ( readFile )
import           Data.Text                      ( Text )
import           Data.Text.IO                   ( readFile )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Linear.V3                      ( V3(..) )
import           Linear.V4                      ( V4(..) )
import           Data.Ix                        ( range )
import           Data.Void                      ( Void )
import           Data.List                      ( nub
                                                , foldl'
                                                )
import           Control.Arrow                  ( second )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Control.Monad                  ( guard )

type Alive = Bool
type Coord = V4 Int
type Space = Map Coord ()

neighborDiffs3 :: [V3 Int]
neighborDiffs3 = do
    x <- [-1, 0, 1]
    y <- [-1, 0, 1]
    z <- [-1, 0, 1]
    guard (not (x == 0 && y == 0 && z == 0))
    return (V3 z y x)

neighborDiffs4 :: [V4 Int]
neighborDiffs4 = do
    x <- [-1, 0, 1]
    y <- [-1, 0, 1]
    z <- [-1, 0, 1]
    w <- [-1, 0, 1]
    guard (not (x == 0 && y == 0 && z == 0 && w == 0))
    return (V4 w z y x)

lift :: V3 Int -> V4 Int
lift (V3 z y x) = V4 0 z y x

play :: [V4 Int] -> Space -> Space
play neighborDiffs space = newSpace
  where
    newSpace = foldl' update space updates
    update s (c, True ) = M.insert c () s
    update s (c, False) = M.delete c s
    updates          = (\c -> (c, newState c)) <$> coordsToConsider
    coordsToConsider = nub $ concatMap (\c -> c : fmap (+ c) neighborDiffs) (fst <$> M.toList space)
    newState :: Coord -> Alive
    newState coord | currentlyAlive = aliveNeighborCount `elem` [2, 3]
                   | otherwise      = aliveNeighborCount == 3
      where
        currentlyAlive     = isJust (space M.!? coord)
        neighborCoords     = (+ coord) <$> neighborDiffs
        aliveNeighborCount = length $ catMaybes ((space M.!?) <$> neighborCoords)

solve :: Text -> Int
solve input = M.size (cycles !! 6)
  where
    space         = buildSpace $ parse input
    cycles        = iterate (play neighborDiffs) space
    neighborDiffs = lift <$> neighborDiffs3

solve2 :: Text -> Int
solve2 input = M.size (cycles !! 6)
  where
    space         = buildSpace $ parse input
    cycles        = iterate (play neighborDiffs4) space

buildSpace :: [[Alive]] -> Space
buildSpace rows = M.fromList $ second (const ()) <$> filter snd (zip (range bounds) (concat rows))
    where bounds = (V4 0 0 0 0, V4 0 0 (length rows - 1) (length (head rows) - 1))

parse :: Text -> [[Alive]]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = P.endBy1 cell P.eol
    cell   = P.some ((== '#') <$> (P.char '.' P.<|> P.char '#'))

main = do
    input        <- readFile "inputs/day17.txt"
    exampleInput <- readFile "inputs/day17_example.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 112
        solve input @?= 362
        solve2 exampleInput @?= 848
        solve2 input @?= 1980
