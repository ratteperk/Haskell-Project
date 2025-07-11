module Pathfinding where

import Types
import Config (tileSize)
import Input (posToTile, getTile)
import System.Random (randomR, mkStdGen, StdGen)


tileToPos :: (Int, Int) -> Position 
tileToPos (y, x) = (fromIntegral x * tileSize + tileSize/2, fromIntegral y * tileSize + tileSize/2)

-- Precompute path for enemies based on the map
getEnemyPath :: [[TileType]] -> StdGen -> [Position]
getEnemyPath tiles gen = calculatePath road 
  where
    start = findStart tiles
    road = findRoad tiles start gen 

findStart :: [[TileType]] -> Position
findStart tiles = 
  case [(y, x) | y <- [0..length tiles - 1],
        x <- [0..length (tiles !! y) - 1],
        tiles !! y !! x == Start] of 
    (a: _) -> tileToPos a
    [] -> error "No start tile found"

lastEl :: [a] -> a
lastEl [x] = x
lastEl (x:xs) = lastEl xs

generateRandom :: Int -> Int -> StdGen -> Int 
generateRandom l r gen = fst $ randomR (l, r) gen

findRoad :: [[TileType]] -> Position -> StdGen -> [Position]
findRoad tiles start gen = helper start []
  where 
    helper prev path 
      | let (a, b) = posToTile prev in getTile tiles a b == Just Finish = path ++ [prev]
      | otherwise = let current = findNear prev path in helper current (path ++ [prev])

    findNear prev path = let (x, y) = posToTile prev in
      case [tileToPos (y', x') | (x', y') <- [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)],
               case getTile tiles x' y' of 
                Just Finish -> True 
                Just Road -> True 
                _ -> False] of
        [] -> error "No road tile found"
        arr -> chooseCorrect (if length path /= 0 then filter (/= lastEl path) arr else arr) path

    chooseCorrect [] prev = error ("No road Tile Found" ++ show (map posToTile prev))
    chooseCorrect nearRoad prev
      | isFinish nearRoad = getFinish nearRoad  
      | otherwise = nearRoad !! (generateRandom 0 (length nearRoad - 1) gen)

    isFinish [] = False
    isFinish (x : xs)
      | let (x', y') = posToTile x in getTile tiles x' y' == Just Finish = True 
      | otherwise = isFinish xs

    getFinish [] = error "Something wrong"
    getFinish (x : xs) 
      | let (x', y') = posToTile x in getTile tiles x' y' == Just Finish = x 
      | otherwise = getFinish xs


calculatePath :: [Position] -> [Position]
calculatePath = id