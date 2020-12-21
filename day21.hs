#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl'
                                                , intercalate
                                                )
import           Data.Either                    ( either
                                                , fromRight
                                                )

type Parser = P.Parsec Void Text

solve2 :: Text -> String
solve2 input = intercalate "," (snd <$> M.toAscList (allergenMap (parse input)))

solve :: Text -> Int
solve input = M.foldl' (+) 0 unknownIngredients
  where
    menu               = parse input
    unknownIngredients = M.withoutKeys allIngredients knownIngredients
    allIngredients     = M.fromListWith (+) (zip (concatMap fst menu) (repeat 1))
    knownIngredients   = S.fromList $ M.elems $ allergenMap menu

allergenMap :: [([String], [String])] -> Map String String
allergenMap menu = fromRight (error "Expecting rights") <$> converge reduce initialMap
  where
    initialMap :: Map String (Either (Set String) String)
    initialMap = Left <$> foldl' f M.empty menu
    f m (ingredients, allergens) = foldl' (f' ingredients) m allergens
    f' ingredients m allergen = M.insertWith S.intersection allergen (S.fromList ingredients) m


reduce :: Map String (Either (Set String) String) -> Map String (Either (Set String) String)
reduce m = either (Left . (S.\\ elemsToReduce)) Right <$> newM
  where
    (elemsToReduce, newM) = M.mapAccumWithKey f S.empty m
    f acc key val@(Right _) = (acc, val)
    f acc key val@(Left ingredients) | S.size ingredients == 1 = (S.insert ingredient acc, Right ingredient)
                                     | otherwise               = (acc, val)
        where ingredient = head $ S.toList ingredients

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

parse :: Text -> [([String], [String])]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser     = lineParser `P.endBy1` P.space
    lineParser = do
        ingredients <- word `P.endBy1` P.char ' '
        allergens   <- parens (symbol "contains" *> (word `P.sepBy1` symbol ","))
        return (ingredients, allergens)
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol
    parens    = P.between (symbol "(") (symbol ")")
    word      = P.some P.letterChar

main = do
    input        <- readFile "inputs/day21.txt"
    exampleInput <- readFile "inputs/day21_example.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 5
        solve input @?= 2635
        solve2 exampleInput @?= "mxmxvkd,sqjhc,fvjkl"
        solve2 input @?= "xncgqbcp,frkmp,qhqs,qnhjhn,dhsnxr,rzrktx,ntflq,lgnhmx"
