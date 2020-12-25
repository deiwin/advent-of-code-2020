#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

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
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as PL
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Control.Arrow                  ( (***) )
import           Math.NumberTheory.Powers.Modular
                                                ( powMod )
import           Math.NumberTheory.Moduli.DiscreteLogarithm
                                                ( discreteLogarithm )
import           Math.NumberTheory.Moduli.PrimitiveRoot
                                                ( isPrimitiveRoot )
import           Math.NumberTheory.Moduli.Class ( isMultElement )
import           Math.NumberTheory.Moduli.Singleton
                                                ( CyclicGroup(..)
                                                , cyclicGroup
                                                )
import           Data.Maybe                     ( fromJust )

type Parser = P.Parsec Void Text

modulo = 20201227
type Modulo = 20201227

solve :: Text -> Int
solve input = encryptionKey
  where
    pkeys@(pkeyA    , pkeyB    ) = parse input
    (      loopSizeA, loopSizeB) = both loopSize pkeys
    encryptionKey | loopSizeA < loopSizeB = runLoop pkeyB loopSizeA
                  | otherwise             = runLoop pkeyA loopSizeB

runLoop subject loopSize = powMod subject loopSize modulo

loopSize :: (Integral a) => a -> Int
loopSize subject = fromIntegral $ discreteLogarithm cg rt x
  where
    cg = fromJust cyclicGroup :: CyclicGroup Integer Modulo
    rt = fromJust (isPrimitiveRoot cg 7)
    x  = fromJust (isMultElement (fromIntegral subject))

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
    input        <- readFile "inputs/day25.txt"
    exampleInput <- readFile "inputs/day25_example.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 14897079
        solve input @?= 9177528
    defaultMain [ bench "exampleInput" (whnf solve exampleInput)
                , bench "input" (whnf solve input)
                ]
