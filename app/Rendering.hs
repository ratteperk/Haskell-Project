module Rendering where

import Graphics.Gloss
import Types
import Config
import Types (startBuilding)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), KeyState(..))

-- Add these helper functions
renderProjectiles :: [Projectile] -> Picture
renderProjectiles = pictures . map renderProjectile
    where
        renderProjectile proj = 
            let (x, y) = projPosition proj
                color = makeColor 1 1 0 1  -- Yellow projectiles
            in translate x y $ colorCircle color 5  -- Small circle for projectiles

renderButton :: UIElement -> Picture
renderButton (Button pos size action label color) =
    let (x, y) = pos
        (w, h) = size
    in pictures
        [ translate x y $ colorRectangle color w h
        , translate (x - w/4) (y - h/4) $ scale 0.1 0.1 $ text label
        ]

-- Add this to your imports from Types


renderGame :: GameState -> Picture
renderGame gs = pictures
    [ renderMap gs
    , renderTowers (towers gs)
    , renderEnemies (enemies gs)
    , renderProjectiles (projectiles gs)
    , renderUI gs
    , if gameOver gs then renderGameOver else blank
    ]

renderMap :: GameState -> Picture
renderMap gs = pictures $ concatMap renderRow (zip [0..] (tiles gs))
    where
        renderRow (y, row) = map (renderTile y) (zip [0..] row)
        renderTile y (x, tile) = 
            let pos = (fromIntegral x * tileSize, fromIntegral y * tileSize)
                color = case tile of
                    Road -> roadColor
                    Buildable -> buildableColor
                    Neutral -> neutralColor
                    Finish -> finishColor
            in translate (fst pos) (snd pos) $ colorRectangle color tileSize tileSize

renderTowers :: [Tower] -> Picture
renderTowers = pictures . map renderTower
    where
        renderTower tower = 
            let color = towerColors (towerType tower)
                radius = towerRange tower / 2  -- Visual representation of range
            in pictures
                [ translate (fst pos) (snd pos) $ colorRectangle color 30 30
                , translate (fst pos) (snd pos) $ colorCircle (withAlpha 0.2 color) radius
                ]
            where pos = towerPosition tower

renderEnemies :: [Enemy] -> Picture
renderEnemies = pictures . map renderEnemy
    where
        renderEnemy enemy = pictures
            [ translate x y $ color (enemyColor $ enemyType enemy) $ circleSolid 15  -- Enemy body
            , translate x (y - 20) $ healthBar (enemyHealth enemy) (enemyMaxHealth enemy)
            ]
            where
                (x, y) = enemyPosition enemy
                
                healthBar current max =
                    let width = 30
                        ratio = current / max
                    in pictures
                        [ color red $ rectangleWire width 5  -- Background
                        , color green $ rectangleSolid (width * ratio) 5  -- Health
                        ]

renderUI :: GameState -> Picture
renderUI gs = pictures
    [ translate (-350) 250 $ scale 0.2 0.2 $ text ("Coins: " ++ show (coins gs))
    , renderButton $ Button (-300, -250) (100, 50) (startBuilding CannonTower) "Cannon" blue
    , renderButton $ Button (-150, -250) (100, 50) (startBuilding SlowTower) "Slow" orange
    , case buildMode gs of
        Building _ -> translate 0 (-280) $ scale 0.15 0.15 $ text "Click on buildable tile to place tower"
        _ -> blank
    ]

renderGameOver :: Picture
renderGameOver = translate (-100) 0 $ scale 0.3 0.3 $ color red $ text "GAME OVER"

-- Helper rendering functions
-- Replace with:
colorRectangle :: Color -> Float -> Float -> Picture
colorRectangle c w h = Color c $ rectangleSolid w h

colorCircle :: Color -> Float -> Picture
colorCircle c r = Color c $ circleSolid r