#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.Ix                        ( Ix )
import           Control.Monad                  ( foldM_ )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import           Data.Array.ST                  ( MArray
                                                , STUArray
                                                , getBounds
                                                , newArray_
                                                , readArray
                                                , writeArray
                                                )

type Cups s = STUArray s Int Int

newPointerArray :: [Int] -> ST s (Cups s)
newPointerArray xs = do
    arr <- newArray_ (minimum xs, maximum xs)
    traverse_ (uncurry (writeArray arr)) (zip xs (drop 1 $ cycle xs))
    return arr

toCircList :: (MArray a e m, Ix e, Num e) => a e e -> e -> m [e]
toCircList a i = reverse <$> go [] a i
  where
    go acc a i = do
        j <- readArray a i
        if j == 1 then return (i : acc) else go (i : acc) a j

playRound :: (MArray a e m, Ix e, Num e) => a e e -> e -> m e
playRound a i = do
    x    <- readArray a i
    y    <- readArray a x
    z    <- readArray a y
    iSuc <- readArray a z
    j    <- destination a i [x, y, z]
    zSuc <- readArray a j
    writeArray a i iSuc
    writeArray a j x
    writeArray a z zSuc
    return iSuc
  where
    destination a i nots = do
        (lo, hi) <- getBounds a
        let j = if i == lo then hi else i - 1
        if j `elem` nots then destination a j nots else return j

solve :: Text -> String
solve input = runST $ do
    a <- newPointerArray numbers
    iterateNM_ 100 (playRound a) (head numbers)
    finalList <- tail <$> toCircList a 1
    return (concatMap show finalList)
  where
    numbers :: [Int]
    numbers = read . pure <$> T.unpack input

solve2 :: Text -> Int
solve2 input = runST $ do
    a <- newPointerArray (numbers ++ [10 .. 1000000])
    iterateNM_ 10000000 (playRound a) (head numbers)
    x <- readArray a 1
    y <- readArray a x
    return (x * y)
  where
    numbers :: [Int]
    numbers = read . pure <$> T.unpack input

iterateNM_ :: Monad m => Int -> (a -> m a) -> a -> m ()
iterateNM_ n f x = foldM_ (\i _ -> f i) x (replicate n ())

main = do
    let input        = "368195742"
    let exampleInput = "389125467"
    runTestTT $ TestCase $ do
        solve exampleInput @?= "67384529"
        solve input @?= "95648732"
        solve2 exampleInput @?= 149245887792
        solve2 input @?= 192515314252
