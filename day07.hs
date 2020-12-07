#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text --package containers
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Prelude                 hiding ( readFile
                                                , lines
                                                )
import           Data.Text                      ( Text
                                                , lines
                                                , unpack
                                                )
import           Data.Graph.Inductive.Graph     ( mkGraph
                                                , Node
                                                , lsuc
                                                )
import           Data.Graph.Inductive.Query.BFS ( bfs )
import           Data.Graph.Inductive.PatriciaTree
                                                ( Gr )
import           Data.Graph.Inductive.Basic     ( grev )
import qualified Text.Parsec                   as P
import qualified Text.Parsec.Char              as PC
import           Data.Either                    ( either )
import           Control.Exception              ( PatternMatchFail(..)
                                                , throw
                                                )
import           Data.Text.IO                   ( readFile )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Test.HUnit.Text                ( runTestTT )
import           Test.HUnit.Base                ( Test(TestCase)
                                                , (@?=)
                                                )
import           Data.Tuple                     ( swap )

type Count = Int
type Color = String
data Constraint = Constraint { color :: !Color
                             , contents :: [(Count, Color)]
                             } deriving (Show, Eq)
type G = Gr Color Count
type NodeMap = Map Color Node

solve :: Text -> Int
solve input = length reachableFromMyBag - 1
  where
    reachableFromMyBag = bfs (nodeMap Map.! "shiny gold") $ grev graph
    (graph, nodeMap)   = buildGraph $ parseInput input

solve2 :: Text -> Int
solve2 input = requiredBagCount (nodeMap Map.! "shiny gold") - 1
  where
    requiredBagCount id = 1 + sum ((\(nid, w) -> w * requiredBagCount nid) <$> lsuc graph id)
    (graph, nodeMap) = buildGraph $ parseInput input

buildGraph :: [Constraint] -> (G, NodeMap)
buildGraph constraints = (mkGraph nodes edges, nodeMap)
  where
    nodes   = zip [0 ..] $ fmap color constraints
    nodeMap = Map.fromList (swap <$> nodes)
    edges   = concatMap cToEdges constraints
    cToEdges c = edge c <$> contents c
    edge constraint (count, toColor) = (nodeId (color constraint), nodeId toColor, count)
    nodeId color = nodeMap Map.! color

parseInput :: Text -> [Constraint]
parseInput input = readEithers (parseLine <$> lines input)
  where
    readEithers   = either (throw . PatternMatchFail . show) id . sequenceA
    parseLine     = P.parse parser "" . unpack
    parseContents = P.try emptyContents P.<|> (content `P.sepBy` P.try (PC.string ", "))
    content       = do
        number <- read <$> P.many1 PC.digit <* PC.space
        color  <- P.manyTill PC.anyChar (P.try (PC.string " bag")) <* P.optional (PC.string "s")
        return (number, color)
    emptyContents = const [] <$> PC.string "no other bags"
    parser        = do
        color    <- P.manyTill PC.anyChar (P.try (PC.string " bags contain "))
        contents <- parseContents <* PC.string "."
        return Constraint { .. }

main = do
    input         <- readFile "inputs/day07.txt"
    exampleInput  <- readFile "inputs/day07_example.txt"
    exampleInput2 <- readFile "inputs/day07_example2.txt"
    runTestTT $ TestCase $ do
        parseInput "light red bags contain 1 bright white bag, 2 muted yellow bags."
            @?= [Constraint { color = "light red", contents = [(1, "bright white"), (2, "muted yellow")] }]
        solve exampleInput @?= 4
        solve input @?= 296
        solve2 exampleInput @?= 32
        solve2 exampleInput2 @?= 126
        solve2 input @?= 9339
