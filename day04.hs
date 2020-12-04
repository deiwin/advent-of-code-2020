#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package parsec --package containers
{-# LANGUAGE OverloadedStrings #-}

import           Prelude                 hiding ( readFile )
import           Data.Text                      ( Text
                                                , unpack
                                                , strip
                                                )
import           Data.Text.IO                   ( readFile )
import qualified Text.Parsec                   as P
import qualified Text.Parsec.Char              as PC
import           Data.Either                    ( either )
import           Control.Exception              ( PatternMatchFail(..)
                                                , throw
                                                )
import           Control.Monad                  ( void )
import           Data.Char                      ( isSpace
                                                , isDigit
                                                , isHexDigit
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )

type Passport = Map String String

valid :: Passport -> Bool
valid passport = missingRequiredFieldCount == 0
  where
    requiredFields            = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    missingRequiredFields     = filter (not . (`Map.member` passport)) requiredFields
    missingRequiredFieldCount = length missingRequiredFields
valid2 :: Passport -> Bool
valid2 passport = valid passport && invalidRequiredFieldCount == 0
    where invalidRequiredFieldCount = Map.size $ Map.filterWithKey (\x y -> not $ validField x y) passport

validField :: String -> String -> Bool
validField "byr" year = isYear year && read year >= 1920 && read year <= 2002
validField "iyr" year = isYear year && read year >= 2010 && read year <= 2020
validField "eyr" year = isYear year && read year >= 2020 && read year <= 2030
validField "hgt" height | length height <= 2          = False
                        | not (all isHexDigit prefix) = False
                        | suffix == "cm"              = read prefix >= 150 && read prefix <= 193
                        | suffix == "in"              = read prefix >= 59 && read prefix <= 76
                        | otherwise                   = False
    where (prefix, suffix) = splitAt (length height - 2) height
validField "hcl" ('#' : hex) = length hex == 6 && all isHexDigit hex
validField "ecl" color       = color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField "pid" pid         = length pid == 9 && all isDigit pid
validField "cid" _           = True
validField _     _           = False

isYear x = length x == 4 && all isDigit x

solve :: Text -> Int
solve input = length $ filter valid $ parseInput input
solve2 :: Text -> Int
solve2 input = length $ filter valid2 $ parseInput input

parseInput :: Text -> [Passport]
parseInput input = readEither $ parse input
  where
    readEither    = either (throw . PatternMatchFail . show) id
    parse         = P.parse parser "" . unpack . strip
    parser        = passport `P.sepBy1` passportSep
    passport      = Map.fromList <$> (passportField `P.sepBy1` P.try fieldSep)
    fieldSep      = void (P.many1 (PC.char ' ')) P.<|> (PC.endOfLine >> P.notFollowedBy (P.try PC.endOfLine))
    passportSep   = P.count 2 PC.endOfLine
    notSpace      = PC.satisfy (not . isSpace) P.<?> "not space"
    passportField = do
        key   <- P.many1 P.letter <* P.char ':'
        value <- P.many1 notSpace
        return (key, value)

main = do
    exampleInput <- readFile "inputs/day04_example.txt"
    input        <- readFile "inputs/day04.txt"
    runTestTT $ TestCase $ do
        parseInput "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n"
            @?= [ Map.fromList [("ecl", "gry"), ("eyr", "2020"), ("hcl", "#fffffd"), ("pid", "860033327")]
                , Map.fromList [("byr", "1937"), ("cid", "147"), ("hgt", "183cm"), ("iyr", "2017")]
                ]
        valid
                (Map.fromList
                    [ ("ecl", "gry")
                    , ("eyr", "2020")
                    , ("hcl", "#fffffd")
                    , ("pid", "860033327")
                    , ("byr", "1937")
                    , ("cid", "147")
                    , ("hgt", "183cm")
                    , ("iyr", "2017")
                    ]
                )
            @?= True
        valid (Map.fromList [("byr", "1937"), ("cid", "147"), ("hgt", "183cm"), ("iyr", "2017")]) @?= False
        solve exampleInput @?= 2
        solve input @?= 245
        solve2 exampleInput @?= 2
        solve2 input @?= 133
