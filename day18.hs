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
import           Control.Monad.Combinators.Expr ( makeExprParser
                                                , Operator(..)
                                                )
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )

type Parser = P.Parsec Void Text

solve :: Text -> Int
solve input = sum $ parse operators input where operators = [[("+", (+)), ("*", (*))]]

solve2 :: Text -> Int
solve2 input = sum $ parse operators input where operators = [[("+", (+))], [("*", (*))]]

parse :: (Num a) => [[(Text, a -> a -> a)]] -> Text -> [a]
parse operators input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = P.endBy expr P.eol
    expr   = makeExprParser term table P.<?> "expression"
    term   = parens expr P.<|> integer P.<?> "term"
    table  = fmap (uncurry binary) <$> operators
    binary name f = InfixL (f <$ symbol name)
    parens = P.between (symbol "(") (symbol ")")
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    integer       = lexeme PL.decimal

main = do
    input <- readFile "inputs/day18.txt"
    runTestTT $ TestCase $ do
        solve "1 + 2 * 3 + 4 * 5 + 6\n" @?= 71
        solve "1 + (2 * 3) + (4 * (5 + 6))\n" @?= 51
        solve "2 * 3 + (4 * 5)\n" @?= 26
        solve "5 + (8 * 3 + 9 + 3 * 4 * 3)\n" @?= 437
        solve input @?= 12918250417632

        solve2 "1 + 2 * 3 + 4 * 5 + 6\n" @?= 231
        solve2 "1 + (2 * 3) + (4 * (5 + 6))\n" @?= 51
        solve2 "2 * 3 + (4 * 5)\n" @?= 46
        solve2 "5 + (8 * 3 + 9 + 3 * 4 * 3)\n" @?= 1445
        solve2 input @?= 171259538712010
