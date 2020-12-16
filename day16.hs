#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl1'
                                                , isPrefixOf
                                                )
import           Data.Function                  ( (&) )

type Range = (Int, Int)
type Constraint = (String, [Range])
type Ticket = [Int]
data Document = Document { constraintList :: [Constraint]
                         , myTicket :: Ticket
                         , nearbyTickets :: [Ticket]
                         } deriving (Show, Eq)

validForConstraint :: Constraint -> Int -> Bool
validForConstraint (_, ranges) x = any inRange ranges where inRange (from, to) = x >= from && x <= to

validField :: [Constraint] -> Int -> Bool
validField constraintList x = any (`validForConstraint` x) constraintList

validTicket :: [Constraint] -> Ticket -> Bool
validTicket constraintList = all (validField constraintList)

solve :: Text -> Int
solve input = sum $ filter f $ concat $ nearbyTickets document
  where
    document = parse input
    f x = not $ validField (constraintList document) x

possibleNames :: [Constraint] -> Ticket -> [Set String]
possibleNames constraintList xs = forField <$> xs
    where forField x = S.fromList (fst <$> filter (`validForConstraint` x) constraintList)

solve2 :: Text -> Int
solve2 input = product departureFields
  where
    departureFields = snd <$> filter (("departure" `isPrefixOf`) . fst) (zip fieldNames $ myTicket document)
    document        = parse input
    validTickets    = filter (validTicket (constraintList document)) $ nearbyTickets document
    fieldNames = (possibleNames (constraintList document) <$> validTickets)
                    & foldl1' (zipWith S.intersection)
                    & reduce
                    & fmap (head . S.toList)
    reduce :: [Set String] -> [Set String]
    reduce ss | newSs == ss = ss
              | otherwise   = reduce newSs
      where
        newSs      = f <$> ss
        fixedNames = foldl1' S.union $ filter (\s -> S.size s == 1) ss
        f s | S.size s == 1 = s
            | otherwise     = S.difference s fixedNames


parse :: Text -> Document
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = do
        constraintList <- constraint `P.sepBy1` P.try singleEol
        myTicket       <- (P.space1 >> symbol "your ticket:" >> P.space1) *> numberList
        nearbyTickets  <- (P.space1 >> symbol "nearby tickets:" >> P.space1) *> (numberList `P.endBy1` P.space1)
        return Document { .. }
    numberList = number `P.sepBy1` symbol ","
    constraint = do
        name   <- P.manyTill (P.letterChar P.<|> P.char ' ') (symbol ":")
        ranges <- ranges
        return (name, ranges)
    ranges = range `P.sepBy1` symbol "or"
    range  = do
        from <- number
        _    <- symbol "-"
        to   <- number
        return (from, to)
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol     = P.eol <* P.notFollowedBy P.eol

main = do
    input        <- readFile "inputs/day16.txt"
    exampleInput <- readFile "inputs/day16_example.txt"
    runTestTT $ TestCase $ do
        validField [("", [(1, 3)])] 0 @?= False
        validField [("", [(1, 3)])] 1 @?= True
        validField [("", [(1, 3)])] 2 @?= True
        validField [("", [(1, 3)])] 3 @?= True
        validField [("", [(1, 3)])] 4 @?= False
        validField [("", [(1, 3), (10, 11)])] 3 @?= True
        solve exampleInput @?= 71
        solve input @?= 26988
        possibleNames [("x", [(1, 3)])] [0] @?= [S.empty]
        possibleNames [("x", [(1, 3)])] [1] @?= [S.singleton "x"]
        possibleNames [("x", [(1, 3)]), ("y", [(2, 3)])] [1] @?= [S.singleton "x"]
        possibleNames [("x", [(1, 3)]), ("y", [(1, 3)])] [1] @?= [S.fromList ["x", "y"]]
        solve2 input @?= 426362917709
