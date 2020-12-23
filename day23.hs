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
import           Data.List.PointedList          ( PointedList(..) )
import qualified Data.List.PointedList         as L
import qualified Data.List.PointedList.Circular
                                               as LC
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
                                                , foldr
                                                , iterate
                                                )
import           Data.Ord                       ( comparing )
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( (>>>)
                                                , second
                                                , first
                                                )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                , fromJust
                                                )
import           Control.Monad                  ( guard )
import           Control.Lens.Getter            ( view )
import           Data.Foldable                  ( toList )

type Parser = P.Parsec Void Text
type Cups = PointedList Int

solve :: Text -> _
solve input = concat $ show <$> tail (toListCircular $ fromJust $ L.find 1 finalCups)
  where
    numbers :: [Int]
    numbers     = read . pure <$> T.unpack input
    (Just cups) = L.fromList numbers
    finalCups   = iterate playRound cups !! 100

playRound :: Cups -> Cups
playRound cups = LC.next $ fromJust $ L.find curId newCups
  where
    curId                    = view L.focus cups
    (removed, cupsWoRemoved) = takeRight 3 cups
    destinationCups          = head $ catMaybes $ (`L.find` cupsWoRemoved) <$> tail (iterate nextId curId)
    newCups                  = foldr L.insertRight destinationCups removed

toListCircular :: PointedList a -> [a]
toListCircular = reverse . go []
  where
    go acc pl = case LC.deleteRight pl of
        Just newPl -> go newAcc newPl
        Nothing    -> newAcc
        where newAcc = view L.focus pl : acc

takeRight :: Int -> PointedList a -> ([a], PointedList a)
takeRight = go []
  where
    go acc 0 pl = (acc, pl)
    go acc n pl = go newAcc (n - 1) newPl
      where
        nextPl       = LC.next pl
        takenElem    = view L.focus nextPl
        (Just newPl) = LC.deleteLeft nextPl
        newAcc       = takenElem : acc

nextId :: Int -> Int
nextId x | x <= 1    = 9
         | otherwise = x - 1

main = do
    let input        = "368195742"
    let exampleInput = "389125467"
    runTestTT $ TestCase $ do
        nextId 3 @?= 2
        nextId 1 @?= 9
        nextId 9 @?= 8
        takeRight 2 (fromJust $ L.fromList [1, 2, 3, 4, 5]) @?= ([3, 2], fromJust $ L.fromList [1, 4, 5])
        solve exampleInput @?= "67384529"
        solve input @?= "95648732"
