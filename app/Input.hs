module Input where

import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), KeyState(..), SpecialKey(..))
import Types
import Config
import Types (startBuilding)
import Data.List (find)
import Graphics.Gloss.Data.Color (blue, orange, violet)

getTile :: [[TileType]] -> Int -> Int -> Maybe TileType
getTile grid x y
  | y < 0 || y >= length grid = Nothing
  | x < 0 || x >= length row = Nothing
  | otherwise = Just (row !! x)
    where
      row = grid !! y

canBuildTowerHere :: Position -> GameState -> Bool
canBuildTowerHere coords gs = isBuildable && enoughCoins
  where
    (tileX, tileY) = posToTile coords
    tile = getTile (tiles gs) tileX tileY

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


tileCenterPosition :: (Int, Int) -> Position
tileCenterPosition (tileX, tileY) =
  (fromIntegral tileX * tileSize, fromIntegral tileY * tileSize)

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

posToTile :: Position -> (Int, Int)
posToTile (x, y) = 
  (floor (x / tileSize), floor (y / tileSize))

handleInput :: Event -> GameState -> GameState
handleInput event gs = case gameState gs of 
  Menu -> case event of 
    EventKey (MouseButton LeftButton) Down _ mousePos ->
      case getClickedButton mousePos gs of 
        Just button -> btnAction button gs
        Nothing -> gs
    _ -> gs

  GameOver -> case event of
    EventKey (SpecialKey KeySpace) Down _ _ -> (initialState (tiles gs)) {randomGen = randomGen gs}
    _ -> gs

  GameProcess -> case event of
    EventKey (MouseButton LeftButton) Down _ mousePos ->
      case buildMode gs of

        Building towerType ->
          if canBuildTowerHere mousePosOffset gs
          then tryBuildTower (tileCenterPosition (posToTile mousePosOffset)) towerType gs
          else checkButton

        NotBuilding -> checkButton

        Removing -> 
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
        mousePosOffset = (xOffset + fst mousePos, yOffset + snd mousePos)

        checkButton = case getClickedButton mousePos gs of
          Just button -> btnAction button gs
          Nothing -> gs

    _ -> gs

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
  in case isOccupied (gates gs) of
    True -> gs
    False -> case coins gs >= gatesCost of
      False -> gs
      True -> gs
        { coins = coins gs - gatesCost
        , gates = newGates : gates gs
        , buildMode = NotBuilding
        }

getTowerCost :: TowerType -> Int
getTowerCost CannonTower = cannonTowerCost
getTowerCost SlowTower = slowTowerCost
getTowerCost SplashTower = splashTowerCost

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

isRoadTile :: Position -> GameState -> Bool
isRoadTile coords gs = isRoadTile
  where
    (tileX, tileY) = posToTile coords
    tile = getTile (tiles gs) tileX tileY

    isRoadTile = case tile of
      Nothing -> False
      Just t -> t == Road

getClickedButton :: Position -> GameState -> Maybe UIElement
getClickedButton pos gs = case gameState gs of
  Menu -> find (isPointInButton pos) menuButtons
  _ -> find (isPointInButton pos) gameButtons

isPointInButton :: Position -> UIElement -> Bool
isPointInButton (px, py) button =
  let 
    (bx, by) = btnPosition button
    (w, h) = btnSize button
    left = bx - w/2
    right = bx + w/2
    bottom = by - h/2
    top = by + h/2
  in px >= left && px <= right && py >= bottom && py <= top
