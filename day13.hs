#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , unpack
                                                , splitOn
                                                )
import           Data.Text.IO                   ( readFile )
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                , (@?)
                                                )
import           Data.List                      ( minimumBy
                                                , maximumBy
                                                , maximum
                                                , foldl'
                                                , foldl1'
                                                , iterate
                                                )
import           Data.Ord                       ( comparing )
import Control.Arrow (second)
import Debug.Trace (traceShowId, traceShow)

earliestConsecutive :: [(Int, Int)] -> Int
earliestConsecutive goals = head $ filter f $ iterate (+ firstId) 0
-- earliestConsecutive goals = [f 0, f 7, f 14, f 21]
    where
        ((0, firstId):_) = goals
        f time = all (uncurry (==)) (second (time `modInverse`) <$> goals)
        -- f time = second (time `modInverse`) <$> tail goals

earliestConsecutive' :: [(Int, Int)] -> Int
earliestConsecutive' goals = go (step - firstTime) step
    where
        (firstTime, step) = traceShowId $ maximumBy (comparing snd) goals
        go time step
          | all (\(goalMod, _, actualMod) -> goalMod == actualMod) mods = time
          | otherwise = go (time + step) step
            where
                mods = (\(goalMod, n) -> (goalMod, n, (time `modInverse` n))) <$> goals
                nextTime = traceShowId $ maximum (toNextMatch <$> mods)
                toNextMatch (goalMod, n, actualMod)
                  | actualMod < goalMod = goalMod - actualMod
                  | otherwise = goalMod + n - actualMod

earliestConsecutive''' :: [(Int, Int)] -> [Int]
earliestConsecutive''' goals = go (step - firstTime) step
    where
        (firstTime, step) = maximumBy (comparing snd) goals
        go time step
          | all (\(goalMod, _, actualMod) -> goalMod == actualMod) mods = time : (go (time + step) step)
          | otherwise = go (time + step) step
            where
                mods = (\(goalMod, n) -> (goalMod, n, (time `modInverse` n))) <$> goals
                nextTime = maximum (toNextMatch <$> mods)
                toNextMatch (goalMod, n, actualMod)
                  | actualMod < goalMod = goalMod - actualMod
                  | otherwise = goalMod + n - actualMod
        -- go time = goals

earliestConsecutive'' :: Int -> [(Int, Int)] -> Int
earliestConsecutive'' start goals = go (start + x) step
    where
        x = toNextMatch (goalMod, step, start `mod` step)
        (goalMod, step) = maximumBy (comparing snd) goals
        toNextMatch (goalMod, n, actualMod)
          | actualMod < goalMod = goalMod - actualMod
          | otherwise = goalMod + n - actualMod
        go time step
          | all (\(goalMod, _, actualMod) -> goalMod == actualMod) mods = time
          | otherwise = go (time + step) step
            where
                mods = (\(goalMod, n) -> (goalMod, n, (time `modInverse` n))) <$> goals
                nextTime = traceShowId $ maximum (toNextMatch <$> mods)
                toNextMatch (goalMod, n, actualMod)
                  | actualMod < goalMod = goalMod - actualMod
                  | otherwise = goalMod + n - actualMod

customLCM :: (Int, Int) -> (Int, Int) -> (Int, Int)
customLCM a@(remA, nA) b@(remB, nB) = (second - first - first, second - first)
    where
        (first:second:_) = take 2 $ earliestConsecutive''' [a, b]

solve :: Text -> Int
solve input = earliestBusId * waitTime
  where
    (begin, ids')             = parseInput input
    ids                       = snd <$> ids'
    (earliestBusId, waitTime) = minimumBy (comparing snd) $ zip ids ((begin `modInverse`) <$> ids)

solve2 :: Text -> Int
solve2 input = y - x
    where
        (x, y) = foldl1' customLCM goals
        goals = fmap (\(a, b) -> (a `mod` b, b)) $ snd $ parseInput input

parseInput :: Text -> (Int, [(Int, Int)])
parseInput input = parse $ lines input
  where
    parse (begin : ids : _) = (read (unpack begin), parseIds ids)
    parseIds ids = readTuple <$> filter ((/= "x") . snd) (zip [0 ..] (splitOn "," ids))
    readTuple (i, x) = (i, read (unpack x))

modInverse :: Int -> Int -> Int
modInverse a b
  | (a `mod` b) == 0 = 0
  | otherwise = b - (a `mod` b)

primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

main = do
    input <- readFile "inputs/day13.txt"
    print $ snd $ parseInput input
    print $ product $ fmap snd $ snd $ parseInput input
    print $ earliestConsecutive' [(0, 7), (1, 13), (4, 59), (6,31), (7,19)]
    runTestTT $ TestCase $ do
        939 `mod` 7 @?= 1
        939 `modInverse` 7 @?= 6
        (939 `modInverse`) <$> [7, 13, 59, 31, 19] @?= [6, 10, 5, 22, 11]
        solve input @?= 2845

        -- earliestConsecutive'' 0 [(0, 7), (1, 13)] @?= 77
        -- earliestConsecutive'' 78 [(0, 7), (1, 13)] @?= 168
        take 2 (earliestConsecutive''' [(0, 7), (1, 13)]) @?= [77, 168]
        customLCM (0, 7) (1, 13) @?= (14, 91)
        customLCM (14, 91) (4, 59) @?= (5019,5369)
        customLCM (5019,5369) (6, 31) @?= (96292,166439)
        customLCM (96292,166439) (7, 19) @?= (2093560,3162341)
        3162341 - 2093560 @?= 1068781
        foldl1' customLCM [(0, 7), (1, 13), (4, 59), (6,31), (7,19)] @?= (2093560,3162341)
        primeFactors 77 @?= [7, 11]
        lcm 6 13 @?= 78

        earliestConsecutive [(0, 7), (4, 59)] @?= 350
        primeFactors 350 @?= [2, 5, 5, 7]
        primeFactors 346 @?= [2, 173]
        primeFactors 354 @?= [2, 3, 59]
        354 `divMod` 59 @?= (6, 0)
        -- lcm 7 63 @?= 78

        -- let case1 = [(0, 7), (1, 13), (4, 59), (6,31), (7,19)]
        primeFactors 1068781 @?= [7, 61, 2503]
        1068781 `mod` 59 @?= 55
        -- let case2 = [(0, 17), (2, 13), (3, 19)]
        primeFactors 3417 @?= [3, 17, 67]
        -- let case3 = [(0,1789), (1,37), (2,47),(3,1889)]
        primeFactors 1202161486 @?= [2,79,1789,4253]
        -- let case4 = [(0,67), (1,7), (2,59),(3,61)]
        primeFactors 754018 @?= [2,17,67,331]
        -- let case5 = [(0,67), (2,7), (3,59),(4,61)]
        primeFactors 779210 @?= [2,5,67,1163]
        -- let case6 = [(0,67), (1,7), (3,59),(4,61)]
        primeFactors 1261476 @?= [2,2,3,3,67,523]

        -- let case1 = [(0, 7), (1, 13), (4, 59), (6,31), (7,19)]
        -- foldl' lcm 1 (snd <$> case1) @?= 3162341
        -- floor (sqrt (fromIntegral $ foldl' lcm 1 (snd <$> case1))) @?= 1778
        -- foldl1' gcd (snd <$> case1) @?= 1
        -- earliestConsecutive case1 @?= 1068781
        -- earliestConsecutive case1 < foldl' lcm 1 (snd <$> case1) @? "smaller than LCM case1"
        -- foldl' lcm 1 (snd <$> case1) `div` earliestConsecutive case1 @?= 2

        -- let case2 = [(0, 17), (2, 13), (3, 19)]
        -- foldl' lcm 1 (snd <$> case2) @?= 4199
        -- foldl1' gcd (snd <$> case2) @?= 1
        -- earliestConsecutive case2 @?= 3417
        -- earliestConsecutive case2 < foldl' lcm 1 (snd <$> case2) @? "smaller than LCM case2"

        -- let case3 = [(0,1789), (1,37), (2,47),(3,1889)]
        -- foldl' lcm 1 (snd <$> case3) @?= 5876813119
        -- foldl1' gcd (snd <$> case3) @?= 1
        -- earliestConsecutive case3 @?= 1202161486
        -- earliestConsecutive case3 < foldl' lcm 1 (snd <$> case3) @? "smaller than LCM case3"
        -- foldl' lcm 1 (snd <$> case3) `div` earliestConsecutive case3 @?= 4

        -- let case4 = [(0,67), (1,7), (2,59),(3,61)]
        -- foldl' lcm 1 (snd <$> case4) @?= 1687931
        -- foldl1' gcd (snd <$> case4) @?= 1
        -- earliestConsecutive case4 @?= 754018
        -- earliestConsecutive case4 < foldl' lcm 1 (snd <$> case4) @? "smaller than LCM case4"

        -- let case5 = [(0,67), (2,7), (3,59),(4,61)]
        -- foldl' lcm 1 (snd <$> case5) @?= 1687931
        -- foldl1' gcd (snd <$> case5) @?= 1
        -- earliestConsecutive case5 @?= 779210
        -- earliestConsecutive case5 < foldl' lcm 1 (snd <$> case5) @? "smaller than LCM case5"

        -- let case6 = [(0,67), (1,7), (3,59),(4,61)]
        -- foldl' lcm 1 (snd <$> case6) @?= 1687931
        -- foldl1' gcd (snd <$> case6) @?= 1
        -- earliestConsecutive case6 @?= 1261476
        -- earliestConsecutive case6 < foldl' lcm 1 (snd <$> case6) @? "smaller than LCM case6"

        -- 17,x,13,19 is 3417
        -- foldl' lcm 1 [17, 13, 19] @?= 4199
        -- earliestConsecutive [(0, 17), (2, 13), (3, 19)] @?= 3417
        -- 1789,37,47,1889 first occurs at timestamp 1202161486.
        -- earliestConsecutive [(0,1789), (1,37), (2,47),(3,1889)] @?= 1202161486
        solve2 input @?= 1
        -- customLCM (0, 7) (1,13) @?= 1
