module Pathfinding where

import Types
import Config (tileSize)

-- Precompute path for enemies based on the map
getEnemyPath :: [[TileType]] -> [Position]
getEnemyPath tiles = 
    let start = findStart tiles
        finish = findFinish tiles
    in calculatePath start finish tiles

findStart :: [[TileType]] -> Position
findStart tiles = 
    let y = 0
        row = tiles !! y
        x = head [i | (i, tile) <- zip [0..] row, tile == Road]
    in (fromIntegral x * tileSize + tileSize/2, fromIntegral y * tileSize + tileSize/2)

findFinish :: [[TileType]] -> Position
findFinish tiles = 
    case [(y, x) | y <- [0..length tiles - 1], 
                   x <- [0..length (tiles !! y) - 1],
                   tiles !! y !! x == Finish] of
        ((y,x):_) -> (fromIntegral x * tileSize + tileSize/2, fromIntegral y * tileSize + tileSize/2)
        [] -> error "No finish tile found"

calculatePath :: Position -> Position -> [[TileType]] -> [Position]
calculatePath start finish _tiles = [start, finish]  -- Simplified path