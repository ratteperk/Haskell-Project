module Rendering where

import Graphics.Gloss
import Types
import Config
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), KeyState(..))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (randomR)

-- Most of the contants and functions names are self-explanatory

-- Function is used for translation entire game field
translateGameZone :: Picture -> Picture
translateGameZone pic = translate (- xOffset) (- yOffset) pic

renderProjectiles :: [Projectile] -> Picture
renderProjectiles = pictures . map renderProjectile
  where
    renderProjectile proj = 
      let 
        (x, y) = projPosition proj
      in translate x y (colorCircle yellow 5)

renderButton :: GameState -> UIElement -> Picture
renderButton gs but@(Button pos size action label color) =
  let 
    (x, y) = pos
    (w, h) = size
    complete = case gameState gs of 
      Menu -> if (notElem but (completedMaps gs)) 
        then translate (x - w/2 + 15) (y - 70) (scale 0.1 0.1 (text "Not completed")) 
        else translate (x - w/2 + 15) (y - 70) (scale 0.1 0.1 (text "completed"))
      _ -> blank
  in pictures
    [ translate x y (colorRectangle color w h)
    , translate (x - w / 2 + 15) y (scale 0.1 0.1 (text label))
    , complete
    ]


renderGame :: GameState -> Assets -> Picture
renderGame gs assets = case gameState gs of 
  Menu -> renderGameMenu gs
  _ -> pictures
    [ translateGameZone (renderMap gs assets)
    , translateGameZone (renderGates (gates gs) assets gs)
    , translateGameZone (renderTowers (towers gs))
    , translateGameZone (renderEnemies (enemies gs) assets)
    , translateGameZone (renderProjectiles (projectiles gs))
    , renderUI gs
    , if (gameState gs) == GameOver then renderGameOver else blank
    ]

renderMap :: GameState -> Assets -> Picture
renderMap gs assets = pictures (concatMap renderRow (zip [0..] (tiles gs)))
  where
    -- Tiles have to translated by half of its size since 
    -- we need to shift their center for proper conversion from 2D grid map coordinates
    renderRow (y, row) = map (translate (tileSize/2) (tileSize/2) . renderTile y) (zip [0..] row)

    renderTile y (x, tile) = 
      let pos = (fromIntegral x * tileSize, fromIntegral y * tileSize)
          border = rectangleWire tileSize tileSize 
          pic = case tile of 
            Road -> roadBlockImg assets 
            Buildable -> buildableBlockImg assets 
            Neutral -> neutralBlockImg assets
            Finish -> finishBlockImg assets
            Start -> spawnBlockImg assets 
      in translate (fst pos) (snd pos) (pictures [pic, border])


renderTowers :: [Tower] -> Picture
renderTowers = pictures . map renderTower
  where
    renderTower tower = 
      let 
        color = towerColors (towerType tower)
        radius = towerRange tower  -- Visual representation of range
      in pictures
        [ translate (fst pos) (snd pos) (colorRectangle color 30 30)
        , translate (fst pos) (snd pos) (colorCircle (withAlpha 0.1 color) radius)
        ]
      where pos = towerPosition tower

renderGates :: [Gates] -> Assets -> GameState -> Picture
renderGates g assets gs = pictures $ map renderGate g
  where
    renderGate gate = 
      let 
        pos = gatesPosition gate
        x = fst pos
        y = snd pos
      in
        pictures 
          [ translate x y (rotate (fst (randomR (0, 360) (randomGen gs))) (gatesImg assets))
          , translate x (y + 20) (healthBar (gatesHealth gate) (gatesDefaultHealth))
          ]
    
renderEnemies :: [Enemy] -> Assets -> Picture
renderEnemies ens assets = pictures (map renderEnemy ens)
  where
    renderEnemy enemy = pictures
      [ translate x y (color (enemyColor (enemyType enemy)) (enemyBody))  -- Enemy body
      , translate x (y - 20) (healthBar (enemyHealth enemy) (enemyMaxHealth enemy))
      ]
      where
        (x, y) = enemyPosition enemy
        
        enemyBody = case enemyType enemy of 
          BasicEnemy -> basicEnemyImg assets
          StrongEnemy -> case enemyEffect enemy of 
            None -> strongEnemyImg assets 
            Freeze -> pictures [coldyImg assets, colorCircle (withAlpha 0.1 blue) 100]
          Boss -> bossImg assets

healthBar :: Health -> Health -> Picture
healthBar current max =
  let 
    width = 30
    ratio = current / max
  in pictures
    [ color red (rectangleWire width 5)  -- Background
    , color green (rectangleSolid (width * ratio) 5)  -- Health
    ]

renderUI :: GameState -> Picture
renderUI gs = pictures 
  ([ translate (-350) 250 (scale 0.2 0.2 (text ("Coins: " ++ show (coins gs)))),
    case buildMode gs of
      Building _ -> translate 0 (-280) (scale 0.15 0.15 (text "Click on buildable tile to place tower"))
      Removing -> translate 0 (-280) (scale 0.15 0.15 (text "Click on tower that you want to remove"))
      GatesBuilding -> translate 0 (-280) (scale 0.15 0.15 (text "Click on road tile to place gates"))
      _ -> blank
  ] ++ map (renderButton gs) gameButtons)

renderGameOver :: Picture
renderGameOver = pictures 
  [ translate (-100) 0 (scale 0.3 0.3 (color red (text "GAME OVER")))
  , translate (-200) (-100) (scale 0.3 0.3 (color red (text "(press Space to restart)")))
  ]

renderGameMenu :: GameState -> Picture 
renderGameMenu gs = pictures ((color white (rectangleSolid 2000 2000)) : (map (renderButton gs) menuButtons))
  
colorRectangle :: Color -> Float -> Float -> Picture
colorRectangle c w h = pictures [Color c (rectangleSolid w h), Color black (rectangleWire w h)]

colorCircle :: Color -> Float -> Picture
colorCircle c r = Color c (circleSolid r)

loadPNG :: FilePath -> IO Picture
loadPNG path = do
    mImg <- loadJuicyPNG path 
    case mImg of
        Just img -> return img
        Nothing  -> error ("Failed to load PNG: " ++ path)

loadAssets :: IO Assets
loadAssets = do
    basicEnemy <- loadPNG "assets/basicEnemy.png"
    strongEnemy <- loadPNG "assets/strongEnemy.png"
    boss <- loadPNG "assets/boss.png"
    roadBlock <- loadPNG "assets/dirt-block.png"
    grassBlock <- loadPNG "assets/grass-block.png"
    gates <- loadPNG "assets/gates.png"
    cobblestone <- loadPNG "assets/cobblestone.png"
    coldy <- loadPNG "assets/coldy.png"
    return Assets 
      { basicEnemyImg = scale 0.03 0.03 basicEnemy
      , strongEnemyImg = scale 0.03 0.03 strongEnemy
      , bossImg = scale 0.05 0.05 boss
      , roadBlockImg = scale 0.043 0.043 roadBlock
      , buildableBlockImg = scale 0.26 0.26 grassBlock
      , spawnBlockImg = color startColor (rectangleSolid tileSize tileSize)
      , finishBlockImg = color finishColor (rectangleSolid tileSize tileSize)
      , neutralBlockImg = translate (-0.2) 0.2 (scale 0.05 0.05 cobblestone)
      , gatesImg = scale 0.22 0.22 gates 
      , coldyImg = scale 0.04 0.04 coldy
      }
