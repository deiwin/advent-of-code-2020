#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Text.ParserCombinators.ReadP  as R
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( catMaybes, listToMaybe )
import           Data.Foldable                  ( asum )

type Parser = P.Parsec Void Text
data Rule = C Char | R [[Int]] deriving (Show, Eq)

solve :: Text -> Int
solve input = length $ filter (elem "") parsedMessages
  where
    (rules, messages) = parse input
    parsedMessages    = parseMessage rules 0 <$> messages

solve2 :: Text -> Int
solve2 input = length $ filter (elem "") parsedMessages
  where
    (rules, messages) = parse input
    newRules          = rules
                        & IM.insert 8 (R [[42], [42, 8]])
                        & IM.insert 11 (R [[42, 31], [42, 11, 31]])
    parsedMessages    = parseMessage newRules 0 <$> messages

parseMessage :: IntMap Rule -> Int -> String -> [String]
parseMessage rules ruleIx = go (rules IM.! ruleIx)
  where
      go _ [] = []
      go (C c) (x:xs)
        | c == x = [xs]
        | otherwise = []
      go (R rss) xs = concatMap (`goSeq` xs) rss

      goSeq [] xs = [xs]
      goSeq _ [] = []
      goSeq (r:rs) xs = parseMessage rules r xs >>= goSeq rs

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
    input         <- readFile "inputs/day19.txt"
    exampleInput  <- readFile "inputs/day19_example.txt"
    exampleInput2 <- readFile "inputs/day19_example2.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 2
        solve input @?= 213
        solve2 exampleInput2 @?= 12
        solve2 input @?= 325
