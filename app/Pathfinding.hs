module Pathfinding where

import Types
import Config (tileSize)
import Input (posToTile, getTile)

tileToPos :: (Int, Int) -> Position 
tileToPos (y, x) = (fromIntegral x * tileSize + tileSize/2, fromIntegral y * tileSize + tileSize/2)

-- Precompute path for enemies based on the map
getEnemyPath :: [[TileType]] -> [Position]
getEnemyPath tiles = 
    let start = findStart tiles
        finish = findFinish tiles
        road = [start]
    in calculatePath start road finish tiles

findStart :: [[TileType]] -> Position
findStart tiles = 
    case [(y, x) | y <- [0..length tiles - 1],
                   x <- [0..length (tiles !! y) - 1],
                   tiles !! y !! x == Start] of 
        (a: _) -> tileToPos a
        [] -> error "No start tile found"
    -- let y = 0
    --     row = tiles !! y
    --     x = head [i | (i, tile) <- zip [0..] row, tile == Road]
    -- in (fromIntegral x * tileSize + tileSize/2, fromIntegral y * tileSize + tileSize/2)

-- lastEl :: [a] -> a
-- lastEl [x] = x
-- lastEl (x:xs) = lastEl xs

-- findRoad :: [[TileType]] -> Position -> [Position]
-- findRoad tiles start = helper start []
--     where 
--         helper :: Position -> [Position] -> [Positions]
--         helper prev path = let (x, y) = posToTile prev in 
--             if length path == 0 then tryNear Nothing x y else tryNear (Just (lastEl path)) x y
        
--         tryNear :: Maybe Position -> Int -> Int -> Position
--         tryNear Nothing x y = case [(x' y') | x' <- [x, x + 1],
--                                               y' <- [y, y + 1],
--                                               tiles !! y !! x == Road]
--                                 (a:_) -> tileToPos a
--                                 [] -> error "No finish tile found"
--         tryNear (Just pos) x y = case



findFinish :: [[TileType]] -> Position
findFinish tiles = 
    case [(y, x) | y <- [0..length tiles - 1], 
                   x <- [0..length (tiles !! y) - 1],
                   tiles !! y !! x == Finish] of
        (a:_) -> tileToPos a
        [] -> error "No finish tile found"

calculatePath :: Position -> [Position] -> Position -> [[TileType]] -> [Position]
calculatePath start road finish _tiles = [start, finish]  -- Simplified path