#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Sequence                  ( Seq(..)
                                                , (|>)
                                                , (<|)
                                                )
import qualified Data.Sequence                 as Seq
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.Foldable                  ( toList )
import           Control.Arrow                  ( (***) )
import           Data.Either                    ( fromLeft )

type Parser = P.Parsec Void Text
type Hand = Seq Int
type TwoHands = (Hand, Hand)

solve :: Text -> Int
solve input = score finalHand
  where
    hands     = (***) Seq.fromList Seq.fromList $ parse input
    finalHand = playGame hands

playGame :: TwoHands -> Hand
playGame = fromLeft (error "Should finish") . iterateM_ playRound
playRound :: TwoHands -> Either Hand TwoHands
playRound = \case
    (as      , Empty   ) -> Left as
    (Empty   , bs      ) -> Left bs
    (a :<| as, b :<| bs) -> if a > b then Right (as |> a |> b, bs) else Right (as, bs |> b |> a)

score :: Hand -> Int
score = sum . zipWith (*) [1 ..] . toList . Seq.reverse

solve2 :: Text -> Int
solve2 input = score finalHand
  where
    hands          = (***) Seq.fromList Seq.fromList $ parse input
    (_, finalHand) = playRecGame hands

playRecGame :: TwoHands -> (Bool, Hand)
playRecGame = fromLeft (error "Should finish") . iterateM_ playRecRound . (S.empty, )
playRecRound :: (Set TwoHands, TwoHands) -> Either (Bool, Hand) (Set TwoHands, TwoHands)
playRecRound (s, hands@(as, _)) | hands `S.member` s = Left (True, as)
playRecRound (_, (as, Empty))                        = Left (True, as)
playRecRound (_, (Empty, bs))                        = Left (False, bs)
playRecRound (s, hands@(a :<| as, b :<| bs)) | canRecurse = if aWonRec then aVictory else bVictory
                                             | a > b      = aVictory
                                             | otherwise  = bVictory
  where
    newSet       = S.insert hands s
    aVictory     = Right (newSet, (as |> a |> b, bs))
    bVictory     = Right (newSet, (as, bs |> b |> a))
    canRecurse   = Seq.length as >= a && Seq.length bs >= b
    (aWonRec, _) = playRecGame (Seq.take a as, Seq.take b bs)

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
        solve2 exampleInput @?= 291
        solve2 input @?= 32835
