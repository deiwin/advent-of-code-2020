#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Sequence                  ( Seq(..)
                                                , (|>)
                                                , (<|)
                                                )
import qualified Data.Sequence                 as Seq
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Array.IArray              ( Array )
import qualified Data.Array.IArray             as A
import           Data.Ix                        ( range
                                                , inRange
                                                )
import           Linear.V2                      ( V2(..) )
import           Linear.V3                      ( V3(..) )
import           Linear.V4                      ( V4(..) )
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl1'
                                                , foldl'
                                                , isPrefixOf
                                                , iterate
                                                )
import           Data.Ord                       ( comparing )
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( (>>>), second )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Control.Monad                  ( guard )

type Parser = P.Parsec Void Text

solve :: Text -> _
solve input = M.size $ M.filter ((== 1) . (`mod` 2)) $ M.fromListWith (+) $ zip (sum <$> parse input) (repeat 1)

parse :: Text -> [[V2 Int]]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = P.some directions `P.endBy1` P.space1
    directions = readDir <$> ( P.try (symbol "se")
                       P.<|>   P.try (symbol "ne")
                       P.<|>   P.try (symbol "e")
                       P.<|>   P.try (symbol "sw")
                       P.<|>   P.try (symbol "nw")
                       P.<|>   P.try (symbol "w")
                             )
    readDir = \case
      "ne" -> V2 (-1) 0
      "e" -> V2 0 1
      "se" -> V2 1 1
      "sw" -> V2 1 0
      "w" -> V2 0 (-1)
      "nw" -> V2 (-1) (-1)
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol

main = do
    input <- readFile "inputs/day24.txt"
    exampleInput <- readFile "inputs/day24_example.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 10
        solve input @?= 282
