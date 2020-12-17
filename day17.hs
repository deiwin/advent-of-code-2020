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
import qualified Text.Megaparsec.Char.Lexer    as PL
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Linear.V3                      ( V3(..) )
import           Data.Ix                        ( inRange
                                                , range
                                                )
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl1'
                                                , isPrefixOf
                                                , nub
                                                , foldl'
                                                )
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( second )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )

type Alive = Bool
type Coord = V3 Int
type Direction = V3 Int
type Space = Map Coord ()


allDirections :: [Direction]
allDirections =
    [ V3 0    (-1) (-1)
    , V3 0    (-1) 0
    , V3 0    (-1) 1
    , V3 0    0    (-1)
    , V3 0    0    1
    , V3 0    1    (-1)
    , V3 0    1    0
    , V3 0    1    1
    , V3 1    (-1) (-1)
    , V3 1    (-1) 0
    , V3 1    (-1) 1
    , V3 1    0    (-1)
    , V3 1    0    0
    , V3 1    0    1
    , V3 1    1    (-1)
    , V3 1    1    0
    , V3 1    1    1
    , V3 (-1) (-1) (-1)
    , V3 (-1) (-1) 0
    , V3 (-1) (-1) 1
    , V3 (-1) 0    (-1)
    , V3 (-1) 0    0
    , V3 (-1) 0    1
    , V3 (-1) 1    (-1)
    , V3 (-1) 1    0
    , V3 (-1) 1    1
    ]

play :: Space -> Space
play space = newSpace
  where
    newSpace = foldl' update space updates
    update s (c, True ) = M.insert c () s
    update s (c, False) = M.delete c s
    updates          = (\c -> (c, newState c)) <$> coordsToConsider
    coordsToConsider = nub $ concatMap (\c -> c : fmap (+ c) allDirections) (fst <$> M.toList space)
    newState :: Coord -> Alive
    newState coord | currentlyAlive = aliveNeighborCount `elem` [2, 3]
                   | otherwise      = aliveNeighborCount == 3
      where
        currentlyAlive     = isJust (space M.!? coord)
        neighborCoords     = (+ coord) <$> allDirections
        aliveNeighborCount = length $ catMaybes ((space M.!?) <$> neighborCoords)

solve :: Text -> Int
solve input = M.size (cycles !! 6)
  where
    space  = buildSpace $ parse input
    cycles = iterate play space

buildSpace :: [[Alive]] -> Space
buildSpace rows = M.fromList $ second (const ()) <$> filter snd (zip (range bounds) (concat rows))
    where bounds = (V3 0 0 0, V3 0 (length rows - 1) (length (head rows) - 1))

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
