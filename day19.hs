#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

import           Prelude                 hiding ( readFile )
import           Data.Text                      ( Text )
import           Data.Text.IO                   ( readFile )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Debug.Trace                    ( traceShow
                                                , traceShowId
                                                )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as PL
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl1'
                                                , foldl'
                                                , isPrefixOf
                                                , iterate
                                                )
import           Data.Ord                       ( comparing )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Control.Monad                  ( guard )
import           Data.Either                    ( rights )
import           Data.Foldable                  ( asum )

type Parser = P.Parsec Void Text
data Rule = C Char | R [[Int]] deriving (Show, Eq)

solve :: Text -> Int
solve input = length $ rights parsedMessages
  where
    (rules, messages) = parse input
    parsedMessages = parseMessage rules 0 <$> messages

parseMessage :: IntMap Rule -> Int -> String -> Either String String
parseMessage rules ruleIx input = case P.parse parser "" input of
    Left  bundle -> Left (P.errorBundlePretty (bundle :: P.ParseErrorBundle String Void))
    Right result -> Right result
  where
    parser = parserFor ruleIx <* P.notFollowedBy P.letterChar
    parserFor :: Int -> P.Parsec Void String String
    parserFor ix = go (rules IM.! ix)
      where
        go = \case
            C c   -> P.try (return <$> P.char c)
            R rss -> asum (P.try . ruleList <$> rss)
        ruleList rs = concat <$> traverse parserFor rs

parse :: Text -> (IntMap Rule, [String])
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = do
        rules    <- IM.fromList <$> rules <* P.space1
        messages <- P.some P.letterChar `P.endBy1` singleEol
        return (rules, messages)
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
