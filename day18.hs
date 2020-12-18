#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl1'
                                                , isPrefixOf
                                                )
import           Data.Function                  ( (&) )

type Parser = P.Parsec Void Text

solve :: Text -> Int
solve input = sum $ parse input

parse :: Text -> _
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = P.endBy expr P.eol
    expr = makeExprParser term table P.<?> "expression"
    term = parens expr P.<|> integer P.<?> "term"
    table = [ [ binary  "+"  (+) ]
            , [ binary  "*"  (*) ]
            ]

    binary  name f = InfixL  (f <$ symbol name)
    parens    = P.between (symbol "(") (symbol ")")
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    integer        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol

main = do
    input <- readFile "inputs/day18.txt"
    -- exampleInput <- readFile "inputs/day18_example.txt"
    print $ parse input
    runTestTT $ TestCase $ do
        solve "1 + 2 * 3 + 4 * 5 + 6\n" @?= 231
        solve "1 + (2 * 3) + (4 * (5 + 6))\n" @?= 51
        solve "2 * 3 + (4 * 5)\n" @?= 46
        solve "5 + (8 * 3 + 9 + 3 * 4 * 3)\n" @?= 1445
        solve input @?= 171259538712010
