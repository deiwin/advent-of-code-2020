#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , unpack
                                                , splitOn
                                                , strip
                                                )
import           Data.Text.IO                   ( readFile )
import qualified Text.Parsec                   as P
import qualified Text.Parsec.Char              as PC
import           Data.Either                    ( either )
import           Control.Exception              ( PatternMatchFail(..)
                                                , throw
                                                )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.List                      ( minimumBy
                                                , maximumBy
                                                , foldl1'
                                                , foldl'
                                                )

import           Data.Vector.Unboxed            ( Vector )
import qualified Data.Vector.Unboxed           as V
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as M
import           Control.Arrow                  ( second )
import           Data.Bits                      ( setBit
                                                , clearBit
                                                )

data Instruction = UpdateMask [(Int, Int)] | WriteMemory (Int, Int) deriving (Eq, Show)
type Program = [Instruction]
type Memory = IntMap Int
data State = State { memory :: Memory
                   , mask :: Int -> Int
                   }

runInstruction :: State -> Instruction -> State
runInstruction state (UpdateMask bits) = state { mask = flip (foldl' f) bits }
  where
    f x (ix, bit) = case bit of
        0 -> x `clearBit` ix
        1 -> x `setBit` ix
runInstruction state (WriteMemory (ix, val)) = state { memory = newMemory }
    where newMemory = M.insert ix (mask state val) (memory state)

runProgram :: State -> [Instruction] -> State
runProgram = foldl' runInstruction

solve :: Text -> Int
solve input = M.foldr' (+) 0 (memory finalState)
  where
    initialState = State { memory = M.empty, mask = id }
    finalState   = runProgram initialState $ parseInput input

parseInput :: Text -> [Instruction]
parseInput input = readEither $ parse input
  where
    readEither = either (throw . PatternMatchFail . show) id
    parse      = P.parse parser "" . unpack . strip
    parser     = P.sepBy1 (P.try maskParser P.<|> memParser) PC.endOfLine
    maskParser = do
        _    <- PC.string "mask = "
        mask <- P.many1 (PC.oneOf "X01")
        let instruction = fmap (second (read . pure)) $ filter ((/= 'X') . snd) $ zip [0 ..] $ reverse mask
        return (UpdateMask instruction)
    memParser = do
        ix  <- PC.string "mem[" *> (read <$> P.many1 PC.digit) <* PC.string "] = "
        val <- read <$> P.many1 PC.digit
        return (WriteMemory (ix, val))

main = do
    exampleInput <- readFile "inputs/day14_example.txt"
    input        <- readFile "inputs/day14.txt"
    -- print $ parseInput exampleInput
    -- print $ parseInput input
    runTestTT $ TestCase $ do
        parseInput exampleInput
            @?= [UpdateMask [(1, 0), (6, 1)], WriteMemory (8, 11), WriteMemory (7, 101), WriteMemory (8, 0)]
        solve exampleInput @?= 165
        solve input @?= 15514035145260
