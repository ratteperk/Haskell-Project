module Input where

import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), KeyState(..))
import Types
import Config
import GameState
import Types (startBuilding)
import Data.List (find)
import Graphics.Gloss.Data.Color (blue, orange)


canBuildHere :: Position -> GameState -> Bool
canBuildHere (x, y) gs =
    let tileX = floor (x / tileSize)
        tileY = floor (y / tileSize)
        tile = (tiles gs !! tileY) !! tileX
        enoughCoins = case buildMode gs of
            Building CannonTower -> coins gs >= cannonTowerCost
            Building SlowTower -> coins gs >= slowTowerCost
            _ -> False
    in tile == Buildable && enoughCoins

buildTower :: Position -> TowerType -> GameState -> GameState
buildTower pos towerType gs = 
    let cost = case towerType of
                CannonTower -> cannonTowerCost
                SlowTower -> slowTowerCost
        newTower = Tower
            { towerPosition = pos
            , towerType = towerType
            , towerDamage = case towerType of
                CannonTower -> cannonTowerDamage
                SlowTower -> 0  -- Slow tower doesn't damage
            , towerRange = case towerType of
                CannonTower -> 150
                SlowTower -> 120
            , towerCooldown = case towerType of
                CannonTower -> 1.0
                SlowTower -> 2.0
            , towerTimeSinceLastShot = 0
            }
    in gs 
        { towers = newTower : towers gs
        , coins = coins gs - cost
        , buildMode = NotBuilding
        }

-- Update the button handling
handleInput :: Event -> GameState -> GameState
handleInput event gs = case event of
    EventKey (MouseButton LeftButton) Down _ mousePos ->
        case buildMode gs of
            Building towerType -> 
                if canBuildHere mousePos gs
                then buildTower mousePos towerType gs
                else gs
            NotBuilding -> 
                case getClickedButton mousePos gs of
                    Just button -> btnAction button gs
                    Nothing -> gs
    _ -> gs

-- Update button creation to use record syntax
getClickedButton :: Position -> GameState -> Maybe UIElement
getClickedButton pos gs = 
    let buttons = 
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
            ]
    in find (isPointInButton pos) buttons

isPointInButton :: Position -> UIElement -> Bool
isPointInButton (px, py) button =
    let (bx, by) = btnPosition button
        (w, h) = btnSize button
        left = bx - w/2
        right = bx + w/2
        bottom = by - h/2
        top = by + h/2
    in px >= left && px <= right && py >= bottom && py <= top
