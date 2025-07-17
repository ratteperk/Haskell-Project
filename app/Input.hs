module Input where

import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), KeyState(..), SpecialKey(..))
import Types
import Config
import Data.List (find)
import Graphics.Gloss.Data.Color (blue, orange, violet)

-- Returns a tile by its coords on 2D grid map 
getTile :: [[TileType]] -> TileCoord -> Maybe TileType
getTile grid (x, y)
  | y < 0 || y >= length grid = Nothing
  | x < 0 || x >= length row = Nothing
  | otherwise = Just (row !! x)
    where
      row = grid !! y

-- Shows whether the tower can be placed here (considering current game state)
canBuildTowerHere :: Position -> GameState -> Bool
canBuildTowerHere coords gs = isBuildable && enoughCoins
  where
    pos = posToTile coords
    tile = getTile (tiles gs) pos

    isBuildable = case tile of
      Nothing -> False
      Just t -> t == Buildable

    enoughCoins = case tile of
      Nothing -> False
      _ -> case buildMode gs of
          Building CannonTower -> coins gs >= cannonTowerCost
          Building SlowTower -> coins gs >= slowTowerCost
          Building SplashTower -> coins gs >= splashTowerCost
          _ -> False


-- Returns position of the center of the tile (in pixels) 
tileCenterPosition :: TileCoord -> Position
tileCenterPosition (tileX, tileY) =
  (fromIntegral tileX * tileSize + (tileSize / 2), fromIntegral tileY * tileSize + (tileSize / 2))

-- Places tower at the current pixel coordinates
buildTower :: Position -> TowerType -> GameState -> GameState
buildTower pos towerType gs = 
  let 
    cost = case towerType of
      CannonTower -> cannonTowerCost
      SlowTower -> slowTowerCost
      SplashTower -> splashTowerCost
    newTower = Tower
      { towerPosition = pos
      , towerType = towerType
      , towerDamage = case towerType of
        CannonTower -> cannonTowerDamage
        SlowTower -> slowTowerDamage
        SplashTower -> splashTowerDamage
      , towerRange = case towerType of
        CannonTower -> cannonTowerRange
        SlowTower -> slowTowerRange
        SplashTower -> splashTowerRange
      , towerCooldown = case towerType of
        CannonTower -> cannonTowerCooldown
        SlowTower -> slowTowerCooldown
        SplashTower -> splashTowerCooldown
      , towerTimeSinceLastShot = 0
      }
  in gs 
    { towers = newTower : towers gs
    , coins = coins gs - cost
    , buildMode = NotBuilding
    }

-- Converts pixel coordinates into coordinates on 2D grid map
posToTile :: Position -> TileCoord
posToTile (x, y) = 
  (floor (x / tileSize), floor (y / tileSize))


-- Handles user input
handleInput :: Event -> GameState -> GameState
handleInput event gs = case gameState gs of 

  Menu -> case event of  -- Menu state
    EventKey (MouseButton LeftButton) Down _ mousePos ->
      case getClickedButton mousePos gs of 
        Just button -> btnAction button gs
        Nothing -> gs
    _ -> gs

  GameOver -> case event of -- Gameover state
    EventKey (SpecialKey KeySpace) Down _ _ -> initialState {randomGen = randomGen gs, completedMaps = completedMaps gs}
    _ -> gs

  GameProcess -> case event of -- Gameplay
    EventKey (MouseButton LeftButton) Down _ mousePos ->
      case buildMode gs of -- Building modes:

        Building towerType ->
          if canBuildTowerHere mousePosOffset gs
          then tryBuildTower (tileCenterPosition (posToTile mousePosOffset)) towerType gs
          else checkButton

        NotBuilding -> checkButton

        Removing -> -- Removes tower that mouse was pointing to and returns half of removed tower cost
          let
            getPointingTower = find (\t -> towerPosition t == (tileCenterPosition (posToTile mousePosOffset)))
          in
            if not (canBuildTowerHere mousePosOffset gs)
            then gs {
                coins = case getPointingTower (towers gs) of
                  Nothing -> coins gs
                  Just tower -> div (getTowerCost (towerType tower)) 2 + coins gs,
                towers =
                  filter (\t -> towerPosition t /= (tileCenterPosition (posToTile mousePosOffset))) (towers gs),
                buildMode = NotBuilding
              }
            else checkButton

        GatesBuilding ->
          case isRoadTile (tileCenterPosition (posToTile mousePosOffset)) gs of
            True -> tryBuildGates (tileCenterPosition (posToTile mousePosOffset)) gs
            False -> checkButton
  
      where 
        -- Mouse position has to be moved with entire game field
        mousePosOffset = (xOffset + fst mousePos, yOffset + snd mousePos)

        -- Checks whether click was performed on some button:
        checkButton = case getClickedButton mousePos gs of
          Just button -> btnAction button gs
          Nothing -> gs {buildMode = NotBuilding}

    _ -> gs

-- In case of success places gates into corresponding road tile (otherwise does nothing)
tryBuildGates :: Position -> GameState -> GameState
tryBuildGates pos gs =
  let
    newGates = Gates 
      { gatesHealth = gatesDefaultHealth
      , gatesPosition = pos
      }

    isOccupied [] = False
    isOccupied (gates:rest)
      | gatesPosition gates == pos = True
      | otherwise = isOccupied rest

  in case not (isOccupied (gates gs)) && coins gs >= gatesCost of
    True -> gs
        { coins = coins gs - gatesCost
        , gates = newGates : gates gs
        , buildMode = NotBuilding
        }
    False -> gs
      
-- Checks whether the tile is free for new tower, and in the case of success calls buildTower function
tryBuildTower :: Position -> TowerType -> GameState -> GameState
tryBuildTower pos towerType gs =
  let 
    isOccupied [] = False
    isOccupied (tower:rest)
      | towerPosition tower == pos = True
      | otherwise = isOccupied rest
  in case isOccupied (towers gs) of
    True -> gs
    _ -> buildTower pos towerType gs

-- Checks whether the tile that is placed on these pixel coordinates is road tile or not
isRoadTile :: Position -> GameState -> Bool
isRoadTile coords gs = isRoadTile
  where
    pos = posToTile coords
    tile = getTile (tiles gs) pos

    isRoadTile = case tile of
      Nothing -> False
      Just t -> t == Road

-- Returns a button that mouse has clicked on
getClickedButton :: Position -> GameState -> Maybe UIElement
getClickedButton pos gs = case gameState gs of
  Menu -> find (isPointInButton pos) menuButtons
  _ -> find (isPointInButton pos) gameButtons

-- Checks whether this pixel coordinates belongs to argument button or not
isPointInButton :: Position -> UIElement -> Bool
isPointInButton (px, py) button =
  let 
    (bx, by) = btnPosition button
    (w, h) = btnSize button
    left = bx - w/2
    right = bx + w/2
    bottom = by - h/2
    top = by + h/2
  in
    px >= left && px <= right && py >= bottom && py <= top
