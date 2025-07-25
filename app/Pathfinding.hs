module Pathfinding where

import Types
import Config (tileSize)
import Input (posToTile, getTile)
import System.Random (randomR, mkStdGen, StdGen)
import Data.List (find)


tileToPos :: TileCoord -> Position 
tileToPos (y, x) = (fromIntegral x * tileSize + tileSize/2, fromIntegral y * tileSize + tileSize/2)

-- Precompute path for enemies based on the map and current random generator state 
getEnemyPath :: [[TileType]] -> StdGen -> [Position]
getEnemyPath tiles gen = findRoad gen tiles (findStart tiles gen) []

findStart :: [[TileType]] -> StdGen -> Position
findStart tiles gen = 
  case [(y, x) | y <- [0..length tiles - 1],
        x <- [0..length (tiles !! y) - 1],
        tiles !! y !! x == Start] of
    [] -> error "No start tile found" 
    arr -> tileToPos (arr !! fst (generateRandom 0 (length arr - 1) gen))
    
-- Function analyze all possible ways to finish and select random one
findRoad :: StdGen -> [[TileType]] -> Position -> [Position] -> [Position]
findRoad gen tiles current path = findNear current path
  where 

    -- finds near Road tiles, if there is no ambiguity, select it, else checks for finish in every way and select random of such ways
    findNear prev path = let (x, y) = posToTile prev in
      case [tileToPos (y', x') | (x', y') <- [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)],
               case getTile tiles (x', y') of 
                Just Finish -> True 
                Just Road -> True  
                _ -> False] of
        [] -> error "No road tile found"
        arr -> let arr' = filter (`notElem` path) arr
          in chooseCorrect (if length arr' > 1 then (filter (goToFinish) arr') else arr')

    goToFinish next = isLeadToFinish next (path ++ [current])

    --  function checks is there a possibility to reach finish, if yes - True, else - False
    isLeadToFinish pos path' =
      let visited = path' ++ [pos]
          (x, y) = posToTile pos
          neighbors = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
          validMoves = [tileToPos (y', x') | (x', y') <- neighbors,
                        case getTile tiles (x', y') of
                          Just Road -> True
                          Just Finish -> True 
                          _ -> False ]
          unvisited = filter (`notElem` visited) validMoves
      in any (canReachFinish visited) unvisited

    -- helper function for "isLeadToFinish"
    canReachFinish :: [Position] -> Position -> Bool
    canReachFinish visited current' 
      | isFinish [current'] = True
      | otherwise = 
        let (cx, cy) = posToTile current'
            neighbors = [(cx+1, cy), (cx-1, cy), (cx, cy+1), (cx, cy-1)]
            next = [tileToPos (ny, nx) | (nx, ny) <- neighbors,
                                          case getTile tiles (nx, ny) of 
                                            Just Road -> True 
                                            Just Finish -> True 
                                            _ -> False]
            newVisited = visited ++ [current']
            next' = filter (`notElem` newVisited) next
        in case next' of 
          [] -> False
          el -> any (canReachFinish newVisited) el

    -- Function checks for Finish in near tiles, if yes - returns all path from start to finish,
    -- if not - calls "findRoad" to continue searching process
    chooseCorrect [] = error ("No correct road tile" ++ show (map posToTile path) ++ show (posToTile current))
    chooseCorrect nearRoad
      | isFinish nearRoad = path ++ [current, getFinish nearRoad]
      | otherwise = 
        let (rInt, newGen) = generateRandom 0 (length nearRoad - 1) gen
        in findRoad newGen tiles (nearRoad !! rInt) (path ++ [current])

    isFinish [] = False
    isFinish (x : xs)
      | let pos = posToTile x in getTile tiles pos == Just Finish = True 
      | otherwise = isFinish xs

    getFinish [] = error "Something wrong"
    getFinish (x : xs) 
      | let pos = posToTile x in getTile tiles pos == Just Finish = x 
      | otherwise = getFinish xs

generateRandom :: Int -> Int -> StdGen -> (Int, StdGen) 
generateRandom l r gen = (randomR (l, r) gen)
