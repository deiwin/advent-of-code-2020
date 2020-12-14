#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.List                      ( foldl' )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as M
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as S
import           Data.Bits                      ( setBit
                                                , clearBit
                                                )

data Instruction = UpdateMask [(Int, Char)] | WriteMemory (Int, Int) deriving (Eq, Show)
type Program = [Instruction]
type Memory = IntMap Int
data State = State { memory :: Memory
                   , valueMask :: Int -> Int
                   , memAddressMask :: Int -> [Int]
                   }

runInstruction :: State -> Instruction -> State
runInstruction state (UpdateMask bits) = state { valueMask = flip (foldl' f) bits }
  where
    f x (ix, bit) = case bit of
        '0' -> x `clearBit` ix
        '1' -> x `setBit` ix
        _   -> x
runInstruction state (WriteMemory (ix, val)) = state { memory = newMemory }
    where newMemory = M.insert ix (valueMask state val) (memory state)

runProgram :: State -> [Instruction] -> State
runProgram = foldl' runInstruction

solve :: Text -> Int
solve input = M.foldr' (+) 0 (memory finalState)
  where
    initialState = State { memory = M.empty, valueMask = id, memAddressMask = pure }
    finalState   = runProgram initialState $ parseInput input

runInstruction2 :: State -> Instruction -> State
runInstruction2 state (UpdateMask bits) = state { memAddressMask = newMemAddressMask }
  where
    newMemAddressMask x = S.toList $ foldl' f (S.singleton x) bits
    f :: IntSet -> (Int, Char) -> IntSet
    f s (ix, bit) = case bit of
        '1' -> S.map (`setBit` ix) s
        'X' -> S.foldl' (floating ix) S.empty s
        _   -> s
    floating :: Int -> IntSet -> Int -> IntSet
    floating ix s x = S.insert (x `clearBit` ix) $ S.insert (x `setBit` ix) s
runInstruction2 state (WriteMemory (ix, val)) = state { memory = newMemory }
  where
    indices   = memAddressMask state ix
    newMemory = foldl' (\m ix -> M.insert ix val m) (memory state) indices

solve2 :: Text -> Int
solve2 input = M.foldr' (+) 0 (memory finalState)
  where
    initialState = State { memory = M.empty, valueMask = id, memAddressMask = pure }
    finalState   = foldl' runInstruction2 initialState $ parseInput input

parseInput :: Text -> [Instruction]
parseInput input = readEither $ parse input
  where
    readEither = either (throw . PatternMatchFail . show) id
    parse      = P.parse parser "" . unpack . strip
    parser     = P.sepBy1 (P.try maskParser P.<|> memParser) PC.endOfLine
    maskParser = do
        _    <- PC.string "mask = "
        mask <- P.many1 (PC.oneOf "X01")
        let instruction = zip [0 ..] $ reverse mask
        return (UpdateMask instruction)
    memParser = do
        ix  <- PC.string "mem[" *> (read <$> P.many1 PC.digit) <* PC.string "] = "
        val <- read <$> P.many1 PC.digit
        return (WriteMemory (ix, val))

main = do
    exampleInput  <- readFile "inputs/day14_example.txt"
    input         <- readFile "inputs/day14.txt"
    exampleInput2 <- readFile "inputs/day14_example2.txt"
    runTestTT $ TestCase $ do
        solve exampleInput @?= 165
        solve input @?= 15514035145260
        solve2 exampleInput2 @?= 208
        solve2 input @?= 3926790061594
