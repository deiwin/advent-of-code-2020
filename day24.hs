#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Prelude                 hiding ( readFile )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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
import           Linear.V2                      ( V2(..) )
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( iterate )

type Parser = P.Parsec Void Text

solve :: Text -> Int
solve input = S.size $ buildBlackGrid $ parse input

solve2 :: Text -> Int
solve2 input = S.size (iterate update (buildBlackGrid $ parse input) !! 100)

directions :: [V2 Int]
directions = [V2 (-1) 0, V2 0 1, V2 1 1, V2 1 0, V2 0 (-1), V2 (-1) (-1)]

update :: Set (V2 Int) -> Set (V2 Int)
update blackGrid = S.filter toBlack toConsider
  where
    toConsider = S.foldr (\c s -> foldr S.insert s (neighbours c)) blackGrid blackGrid
    neighbours c = (+ c) <$> directions
    toBlack c | isBlack   = blackNeighbourCount `elem` [1, 2]
              | otherwise = blackNeighbourCount == 2
      where
        isBlack             = c `S.member` blackGrid
        blackNeighbourCount = length $ filter (`S.member` blackGrid) $ neighbours c

buildBlackGrid :: [[V2 Int]] -> Set (V2 Int)
buildBlackGrid directions = S.fromList $ M.keys blackTileMap
  where
    blackTileMap = M.filter ((== 1) . (`mod` 2)) flipCountMap
    flipCountMap = M.fromListWith (+) $ zip (sum <$> directions) (repeat 1)

parse :: Text -> [[V2 Int]]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = P.some directions `P.endBy1` P.space1
    directions =
        readDir
            <$> (     P.try (symbol "se")
                P.<|> P.try (symbol "ne")
                P.<|> P.try (symbol "e")
                P.<|> P.try (symbol "sw")
                P.<|> P.try (symbol "nw")
                P.<|> P.try (symbol "w")
                )
    readDir = \case
        "ne" -> V2 (-1) 0
        "e"  -> V2 0 1
        "se" -> V2 1 1
        "sw" -> V2 1 0
        "w"  -> V2 0 (-1)
        "nw" -> V2 (-1) (-1)
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol

main = do
    input        <- readFile "inputs/day24.txt"
    exampleInput <- readFile "inputs/day24_example.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 10
        solve input @?= 282
        solve2 exampleInput @?= 2208
        solve2 input @?= 3445
