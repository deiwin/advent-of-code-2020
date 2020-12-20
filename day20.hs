#!/usr/bin/env stack
-- stack --resolver lts-15.6 runghc --package HUnit --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Array.IArray              ( Array )
import qualified Data.Array.IArray             as A
import           Data.Ix                        ( range
                                                , inRange
                                                )
import           Linear.V2                      ( V2(..)
                                                , perp
                                                )
import           Control.Applicative            ( empty )
import           Data.Void                      ( Void )
import           Data.List                      ( foldl'
                                                , iterate
                                                , cycle
                                                , nub
                                                , intersect
                                                , transpose
                                                , maximumBy
                                                )
import           Data.Ord                       ( comparing )
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( first )

type Parser = P.Parsec Void Text
data Tile = Tile { tileID :: Int
                 , borders :: [[Bool]]
                 , rotation :: Int
                 , flippedH :: Bool
                 , flippedV :: Bool
                 }

type Coord = V2 Int
type Grid = A.Array Coord Bool

instance Show Tile where
    show (Tile id _ r h v) = show id ++ ":" ++ rotation
        where rotation = show r ++ (if h then "H" else "") ++ (if v then "V" else "")

instance Eq Tile where
    a == b = tileID a == tileID b && borders a == borders b

topBorder = head . borders
rightBorder = (!! 1) . borders
botBorder = (!! 2) . borders
leftBorder = (!! 3) . borders

solve :: Text -> Int
solve input = zip (tileID <$> ts) (matchingSideCount <$> ts)
              & filter ((== 2) . snd)
              & fmap fst
              & product
  where
    ts                  = fst <$> parse input
    allTilePermutations = concatMap allPermutations ts
    matchingSideCount   = length . filter (not . null) . matches allTilePermutations

directions :: [V2 Int]
directions = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

solve2 :: Text -> Int
solve2 input = roughCells - monsterCells
  where
    grid                 = buildGrid input
    bounds               = A.bounds grid
    roughCells           = length $ filter id $ A.elems grid
    allMonsterBeginnings = (\m -> filter (isMonster m) $ range bounds) <$> sequenceA mods monster
    monsterBeginnings    = maximumBy (comparing length) allMonsterBeginnings
    monsterCount         = length monsterBeginnings
    isMonster monster c = and (f . (+ c) <$> monster) where f c = inRange bounds c && grid A.! c
    monsterCells = monsterCount * length monster
    mods         = fmap <$> (m ++ ((. perp) <$> m)) where m = (*) <$> [ V2 y x | y <- [-1, 1], x <- [-1, 1] ]

buildGrid :: Text -> Grid
buildGrid input = grid
  where
    tsWithPixels = parse input
    tilePixelMap :: IntMap [[Bool]]
    tilePixelMap        = IM.fromList (first tileID <$> tsWithPixels)
    ts                  = fst <$> tsWithPixels
    allTilePermutations = concatMap allPermutations ts
    tilesWithMatches    = withMatches allTilePermutations <$> ts
    firstCorner         = toTopLeft $ head $ filter isCorner tilesWithMatches
    toTopLeft           = until (staysInBounds (V2 0 0)) (rotateWithMatches 1)
    rotateWithMatches n (t, borderMatches) = (rotateTile n t, rotate n (fmap (rotateTile n) <$> borderMatches))
    isCorner (t, borderMatches) = length (filter (not . null) borderMatches) == 2
    isSide (t, borderMatches) = length (filter (not . null) borderMatches) == 3
    staysInBounds coord (t, borderMatches) = and $ zipWith f directionCoords borderMatches
      where
        directionCoords = (+ coord) <$> directions
        f c matches | inRange tileBounds c = not $ null matches
                    | otherwise            = null matches
    isTopLeft = \case
        (_, [[], _, _, []]) -> True
        _                   -> False
    sideCount  = length $ filter isSide tilesWithMatches
    sideMax    = (sideCount `div` 4) + 1
    tileBounds = (V2 0 0, V2 sideMax sideMax)
    initialTileMap :: Map (V2 Int) [Tile]
    initialTileMap = M.singleton (V2 0 0) (return $ fst firstCorner)
    tileMap        = foldl' buildTileMap initialTileMap (range tileBounds)
    buildTileMap m c = foldl' f newM newAssocs
      where
        [(t, borderMatches)] = filter (staysInBounds c) $ withMatches allTilePermutations <$> (m M.! c)
        newAssocs            = filter (inRange tileBounds . fst) $ zip ((+ c) <$> directions) borderMatches
        newM                 = M.insert c [t] m
        f m (k, vs) = M.insertWith intersect k vs m
    withMatches ts t = (t, matches ts t)
    grid = A.array gridBounds $ concatMap assocs $ range tileBounds
    assocs tileCoord@(V2 y x) = zip (range bounds) (concat pixels)
      where
        bounds    = (minCoord, maxCoord)
        minCoord  = V2 (y * tileSideLength) (x * tileSideLength)
        maxCoord  = V2 ((y + 1) * tileSideLength - 1) ((x + 1) * tileSideLength - 1)
        [tile]    = tileMap M.! tileCoord
        rawPixels = tilePixelMap IM.! tileID tile
        pixels = rawPixels
                & applyWhen (flippedH tile) flipHTilePixels
                & applyWhen (flippedV tile) flipVTilePixels
                & rotateTilePixels (rotation tile)
    gridSideMax    = (sideMax + 1) * tileSideLength - 1
    tileSideLength = length (head $ snd $ head tsWithPixels)
    gridBounds     = (V2 0 0, V2 gridSideMax gridSideMax)
flipHTilePixels :: [[a]] -> [[a]]
flipHTilePixels = reverse
flipVTilePixels :: [[a]] -> [[a]]
flipVTilePixels = fmap reverse
rotateTilePixels :: Int -> [[a]] -> [[a]]
rotateTilePixels n xss = iterate f xss !! n where f = flipVTilePixels . transpose

monster :: [V2 Int]
monster =
    [ V2 0 18
    , V2 1 0
    , V2 1 5
    , V2 1 6
    , V2 1 11
    , V2 1 12
    , V2 1 17
    , V2 1 18
    , V2 1 19
    , V2 2 1
    , V2 2 4
    , V2 2 7
    , V2 2 10
    , V2 2 13
    , V2 2 16
    ]

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen b f x = if b then f x else x

matches :: [Tile] -> Tile -> [[Tile]]
matches ts t = [topMatches, rightMatches, botMatches, leftMatches]
  where
    leftMatches  = filter ((== reverse (leftBorder t)) . rightBorder) otherTiles
    botMatches   = filter ((== reverse (botBorder t)) . topBorder) otherTiles
    rightMatches = filter ((== reverse (rightBorder t)) . leftBorder) otherTiles
    topMatches   = filter ((== reverse (topBorder t)) . botBorder) otherTiles
    curID        = tileID t
    otherTiles   = filter ((/= curID) . tileID) ts

squareToClockwiseBorders :: [[a]] -> [[a]]
squareToClockwiseBorders xs = [top, right, bot, left]
  where
    top   = head xs
    right = last <$> xs
    bot   = reverse $ last xs
    left  = reverse (head <$> xs)

allPermutations :: Tile -> [Tile]
allPermutations = nub . concatMap allRotations . sequenceA [id, flipVerically, flipHorizontally]
flipHorizontally t = t { borders = reverse <$> [botBorder t, rightBorder t, topBorder t, leftBorder t]
                       , flippedH = not (flippedH t)
                       }
flipVerically t = t { borders = reverse <$> [topBorder t, leftBorder t, botBorder t, rightBorder t]
                    , flippedV = not (flippedV t)
                    }

allRotations :: Tile -> [Tile]
allRotations t = (`rotateTile` t) <$> [0 .. 3]

rotateTile :: Int -> Tile -> Tile
rotateTile n t = t { borders = rotate n (borders t), rotation = (rotation t + n) `mod` 4 }

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop (length xs - n) $ cycle xs

parse :: Text -> [(Tile, [[Bool]])]
parse input = case P.parse parser "" input of
    Left  bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
    Right result -> result
  where
    parser = tile `P.endBy1` P.space1
    tile   = do
        id     <- symbol "Tile" *> number <* symbol ":" <* P.space1
        pixels <- P.some pixel `P.sepBy` P.try singleEol
        return (Tile id (squareToClockwiseBorders pixels) 0 False False, middle <$> middle pixels)
    pixel  = P.try (False <$ P.char '.') P.<|> (True <$ P.char '#')
    middle = tail . init
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme        = PL.lexeme spaceConsumer
    symbol        = PL.symbol spaceConsumer
    number        = lexeme PL.decimal
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol

showGrid :: Grid -> String
showGrid grid = unlines rows
  where
    bounds            = A.bounds grid
    (_, V2 maxY maxX) = bounds
    rows              = showRow <$> [0 .. maxY]
    showRow y = showCell y <$> [0 .. maxX]
    showCell y x = toChar $ grid A.! V2 y x
    toChar = \case
        True  -> '#'
        False -> '.'

main = do
    input        <- readFile "inputs/day20.txt"
    exampleInput <- readFile "inputs/day20_example.txt"
    putStrLn $ showGrid $ buildGrid input
    runTestTT $ TestCase $ do
        rotate 1 "TRBL" @?= "LTRB"
        rotate 2 "TRBL" @?= "BLTR"
        let t = Tile
                2473
                [ [True, False, False, False, False, True, True, True, True, False]
                , [False, False, False, True, True, True, False, True, False, False]
                , [False, True, False, True, False, True, True, True, False, False]
                , [False, True, True, False, False, False, True, True, True, True]
                ]
                0
                False
                False
        t @?= t
        rotateTile 2 t @?= flipHorizontally (flipVerically t)

        solve exampleInput @?= 20899048083289
        solve input @?= 174206308298779

        let pixels = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        rotateTilePixels 0 pixels @?= pixels
        rotateTilePixels 1 pixels @?= [[7, 4, 1], [8, 5, 2], [9, 6, 3]]
        flipHTilePixels pixels @?= [[7, 8, 9], [4, 5, 6], [1, 2, 3]]
        rotateTilePixels 2 pixels @?= flipHTilePixels (flipVTilePixels pixels)

        solve2 exampleInput @?= 273
        solve2 input @?= 2409
