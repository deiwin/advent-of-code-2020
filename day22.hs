#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
import           Data.Sequence                  ( Seq(..)
                                                , (|>)
                                                , (<|)
                                                )
import qualified Data.Sequence                 as Seq
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
import           Data.Foldable                  ( toList )
import           Data.Ord                       ( comparing )
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( (>>>)
                                                , second
                                                , (***)
                                                )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Control.Monad                  ( guard )

type Parser = P.Parsec Void Text

solve :: Text -> _
solve input = sum $ zipWith (*) [1 ..] $ toList $ Seq.reverse finalHand
  where
    hands            = (***) Seq.fromList Seq.fromList $ parse input
    (Left finalHand) = iterateM_ playRound hands

playRound :: (Seq Int, Seq Int) -> Either (Seq Int) (Seq Int, Seq Int)
playRound = \case
    (as      , Empty   ) -> Left as
    (Empty   , bs      ) -> Left bs
    (a :<| as, b :<| bs) -> if a > b then Right (as |> a |> b, bs) else Right (as, bs |> b |> a)

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f = g where g x = f x >>= g

parse :: Text -> ([Int], [Int])
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = do
        p1Cards <- symbol "Player 1:" *> P.space1 *> number `P.sepBy1` P.try singleEol <* P.space1
        p2Cards <- symbol "Player 2:" *> P.space1 *> number `P.endBy1` P.try singleEol
        return (p1Cards, p2Cards)
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol

main = do
    input        <- readFile "inputs/day22.txt"
    exampleInput <- readFile "inputs/day22_example.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 306
        solve input @?= 31809
