#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import           Prelude                 hiding ( readFile )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( readFile )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Criterion.Main                 ( defaultMain
                                                , bench
                                                , whnf
                                                )
import           Debug.Trace                    ( traceShow
                                                , traceShowId
                                                )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as PL
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Ix                        ( range )
import           Linear.V2                      ( V2(..) )
import           Linear.V3                      ( V3(..) )
import           Linear.V4                      ( V4(..) )
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl1'
                                                , foldl'
                                                , isPrefixOf
                                                , iterate
                                                , cycle
                                                , nub
                                                )
import           Data.Ord                       ( comparing )
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( (>>>), second )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Control.Monad                  ( guard )

type Parser = P.Parsec Void Text
data Tile = Tile { tileID :: Int
                 , borders :: [[Bool]]
                 } deriving (Eq, Show)

topBorder = head . borders
rightBorder = (!! 1) . borders
botBorder = (!! 2) . borders
leftBorder = (!! 3) . borders

solve :: Text -> _
solve input = product $ fmap fst $ filter ((== 2) . snd) $ zip (tileID <$> ts) (length . filter (not . null) . matches ts <$> ts)
    where
        ts = parse input

matches :: [Tile] -> Tile -> [[Tile]]
matches ts t = [topMatches, rightMatches, botMatches, leftMatches]
    where
        leftMatches = filter ((== reverse (leftBorder t)) . rightBorder) potentialMatches
        botMatches = filter ((== reverse (botBorder t)) . topBorder) potentialMatches
        rightMatches = filter ((== reverse (rightBorder t)) . leftBorder) potentialMatches
        topMatches = filter ((== reverse (topBorder t)) . botBorder) potentialMatches
        potentialMatches = concatMap allPermutations otherTiles
        curID = tileID t
        [top,right,bot,left] = borders t
        otherTiles = filter ((/= curID) . tileID) ts

squareToClockwiseBorders :: [[a]] -> [[a]]
squareToClockwiseBorders xs = [top, right, bot, left]
    where
        top = head xs
        right = last <$> xs
        bot = reverse $ last xs
        left = reverse (head <$> xs)

allPermutations :: Tile -> [Tile]
allPermutations = nub . concatMap allRotations . sequenceA [id, flipVerically, flipHorizontally]
    where
        flipVerically t = t { borders = zipWith ($) [id, reverse, id, reverse] (borders t)}
        flipHorizontally t = t { borders = zipWith ($) [reverse, id, reverse, id] (borders t)}

allRotations :: Tile -> [Tile]
allRotations t = (`rotateTile` t) <$> [0..3]

rotateTile :: Int -> Tile -> Tile
rotateTile n t = t { borders = rotate n (borders t) }

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n $ cycle xs

parse :: Text -> [Tile]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = tile `P.endBy1` P.space1
    tile = do
        id <- symbol "Tile" *> number <* symbol ":" <* P.space1
        pixels <- P.some pixel `P.sepBy` P.try singleEol
        return (Tile id (squareToClockwiseBorders pixels))
    pixel = P.try (False <$ P.char '.') P.<|> (True <$ P.char '#')
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol

main = do
    input <- readFile "inputs/day20.txt"
    exampleInput <- readFile "inputs/day20_example.txt"
    let ts = parse exampleInput
    print  ts
    print $ matches ts (head ts)
    print $ solve exampleInput
    runTestTT $ TestCase $ do
        rotate 0 [1,2,3] @?= [1,2,3]
        rotate 1 [1,2,3] @?= [2,3,1]
        rotate 2 [1,2,3] @?= [3,1,2]
        rotate 3 [1,2,3] @?= [1,2,3]
        solve exampleInput @?= 20899048083289
        solve input @?= 174206308298779
