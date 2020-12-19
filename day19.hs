#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

import           Prelude                 hiding ( readFile )
import           Data.Text                      ( Text )
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
                                                )
import           Data.Ord                       ( comparing )
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( (>>>)
                                                , second
                                                )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Control.Monad                  ( guard )

type Parser = P.Parsec Void Text
data Rule = C Char | R [[Int]] deriving (Show, Eq)

solve :: Text -> Int
solve input = length $ catMaybes $ parse input

parse :: Text -> [Maybe String]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = do
        rules <- IM.fromList <$> rules <* P.space1
        let newParser = parserFor rules 0 <* P.notFollowedBy P.letterChar
        ((Just <$> P.try newParser) P.<|> (const Nothing <$> P.skipSome P.letterChar)) `P.endBy1` singleEol
    parserFor :: IntMap Rule -> Int -> Parser String
    parserFor m ix = go (m IM.! ix)
      where
        rule = m IM.! ix
        go   = \case
            C c        -> P.try (return <$> P.char c)
            R [rs]     -> ruleList rs
            R [as, bs] -> P.try (ruleList as) P.<|> ruleList bs
        ruleList rs = concat <$> traverse (parserFor m) rs
    rules = rule `P.sepBy1` P.try singleEol
    rule  = do
        key  <- number <* symbol ":"
        rule <- (C <$> constant) P.<|> (R <$> references)
        return (key, rule)
    references = P.some number `P.sepBy` symbol "|"
    constant   = quotes P.letterChar
    quotes     = P.between (symbol "\"") (symbol "\"")
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol

main = do
    input        <- readFile "inputs/day19.txt"
    exampleInput <- readFile "inputs/day19_example.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 2
        solve input @?= 213
        1 @?= 1
        1 @?= 1
