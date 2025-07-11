module Input where

import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), KeyState(..), SpecialKey(..))
import Types
import Config
import GameState
import Types (startBuilding)
import Data.List (find)
import Graphics.Gloss.Data.Color (blue, orange, violet)

getTile :: [[TileType]] -> Int -> Int -> Maybe TileType
getTile grid x y
  | y < 0 || y >= length grid = Nothing
  | x < 0 || x >= length row = Nothing
  | otherwise = Just (row !! x)
  where row = grid !! y

canBuildHere :: Position -> GameState -> Bool
canBuildHere (x, y) gs = isBuildable && enoughCoins
  where
  tileX = floor (x / tileSize)
  tileY = floor (y / tileSize)
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
handleInput event gs = case event of
  EventKey (MouseButton LeftButton) Down _ mousePos ->
    case buildMode gs of
      Building towerType -> let mousePosOffset = (xOffset + fst mousePos, yOffset + snd mousePos) in
        if canBuildHere mousePosOffset gs
        then tryBuildTower (tileCenterPosition (posToTile mousePosOffset)) towerType gs
        else case getClickedButton mousePos gs of 
          Just button -> gs {buildMode = NotBuilding}
          Nothing -> gs
      NotBuilding -> 
        case getClickedButton mousePos gs of
          Just button -> btnAction button gs
          Nothing -> gs
  EventKey (SpecialKey KeySpace) Down _ _ ->
    if gameOver gs then (initialState sampleMap) {randomGen = randomGen gs} else gs
  _ -> gs

tryBuildTower :: Position -> TowerType -> GameState -> GameState
tryBuildTower pos towerType gs = if gameOver gs then gs else 
  let 
    isOccupied [] = False
    isOccupied (tower:rest)
      | towerPosition tower == pos = True
      | otherwise = isOccupied rest
  in case isOccupied (towers gs) of
    True -> gs
    _ -> buildTower pos towerType gs


getClickedButton :: Position -> GameState -> Maybe UIElement
getClickedButton pos gs = 
  let 
    buttons = 
      [ Button { btnPosition = (-300, -250)
          , btnSize = (100, 50)
          , btnAction = startBuilding CannonTower
          , btnLabel = "Cannon"
          , btnColor = blue
          }
      , Button { btnPosition = (-150, -250)
          , btnSize = (100, 50)
          , btnAction = startBuilding SlowTower
          , btnLabel = "Slow"
          , btnColor = orange
          }
      , Button { btnPosition = (0, -250)
          , btnSize = (100, 50)
          , btnAction = startBuilding SplashTower
          , btnLabel = "Slow"
          , btnColor = violet
          }
      ]
  in find (isPointInButton pos) buttons

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
