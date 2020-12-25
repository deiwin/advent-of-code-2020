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
import           Control.Arrow                  ( (>>>), second, (***) )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Control.Monad                  ( guard )

type Parser = P.Parsec Void Text

modulo = 20201227

solve :: Text -> _
solve input = encryptionKey
    where
        pkeys@(pkeyA, pkeyB) = parse input
        (loopSizeA, loopSizeB) = both loopSize pkeys
        encryptionKey = if loopSizeA < loopSizeB
                           then runLoop pkeyB loopSizeA
                           else runLoop pkeyA loopSizeB

runLoop subject loopSize = iterate f subject !! (loopSize - 1)
    where f x = (x * subject) `rem` modulo

loopSize :: Int -> Int
loopSize = go 7 7 1
    where
        go !subject !val !i !goal
          | newVal == goal = newI
          | otherwise = go subject newVal newI goal
          where
              newVal = (val * subject) `rem` modulo
              newI = i + 1

both :: (a -> b) -> (a, a) -> (b, b)
both f = (***) f f

parse :: Text -> (Int, Int)
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = do
        a <- number
        b <- number
        return (a, b)
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space P.space1 empty empty
    lexeme        = PL.lexeme spaceConsumer
    number        = lexeme PL.decimal

main = do
    input <- readFile "inputs/day25.txt"
    exampleInput <- readFile "inputs/day25_example.txt"
    -- print $ solve exampleInput
    runTestTT $ TestCase $ do
        solve exampleInput @?= 14897079
        solve input @?= 9177528
