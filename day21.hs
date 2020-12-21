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
import Data.Either (isLeft, either, rights)

type Parser = P.Parsec Void Text

solve :: Text -> Int
solve input = M.foldl' (+) 0 unknownIngredients
    where
        menu = parse input
        unknownIngredients = M.withoutKeys allIngredients knownIngredients
        allIngredients = M.fromListWith (+) (zip (concatMap fst menu) (repeat 1))
        knownIngredients = S.fromList $ rights $ M.elems allergenMap
        allergenMap = converge reduce initialMap
        initialMap :: Map String (Either (Set String) String)
        initialMap = Left <$> foldl' f M.empty menu
        f m (ingredients, allergens) = foldl' (f' ingredients) m allergens
        f' ingredients m allergen = M.insertWith S.intersection allergen (S.fromList ingredients) m

reduce :: Map String (Either (Set String) String) -> Map String (Either (Set String) String)
reduce m = either (Left . (S.\\ elemsToReduce)) Right <$> newM
    where
        (elemsToReduce, newM) = M.mapAccumWithKey f S.empty m
        f acc key val@(Right _) = (acc, val)
        f acc key val@(Left ingredients)
          | S.size ingredients == 1 = (S.insert ingredient acc, Right ingredient)
          | otherwise = (acc, val)
          where ingredient = head $ S.toList ingredients

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

parse :: Text -> [([String], [String])]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = lineParser `P.endBy1` P.space
    lineParser = do
        ingredients <- word `P.endBy1` P.char ' '
        allergens <- parens (symbol "contains" *> (word `P.sepBy1` symbol ","))
        return (ingredients, allergens)
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol
    parens = P.between (symbol "(") (symbol ")")
    word = P.some P.letterChar

main = do
    input <- readFile "inputs/day21.txt"
    exampleInput <- readFile "inputs/day21_example.txt"
    -- print $ solve exampleInput
    runTestTT $ TestCase $ do
        solve exampleInput @?= 5
        solve input @?= 2635
