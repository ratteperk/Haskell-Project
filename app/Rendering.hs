module Rendering where

import Graphics.Gloss
import Types
import Config
import Types (startBuilding)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), KeyState(..))
import Graphics.Gloss.Juicy (loadJuicyPNG)

translateGameZone :: Picture -> Picture 
translateGameZone pic = translate (- xOffset) (- yOffset) pic

addTranslate :: Picture -> Picture 
addTranslate pic = translate (tileSize/2) (tileSize/2) pic

renderProjectiles :: [Projectile] -> Picture
renderProjectiles = pictures . map renderProjectile
  where
    renderProjectile proj = 
      let 
        (x, y) = projPosition proj
      in translate x y (colorCircle yellow 5)

renderButton :: UIElement -> Picture
renderButton (Button pos size action label color) =
  let 
    (x, y) = pos
    (w, h) = size
  in pictures
    [ translate x y (colorRectangle color w h)
    , translate (x - w / 2 + 15) y (scale 0.1 0.1 (text label))
    ]

renderGame :: GameState -> Assets -> Picture
renderGame gs assets = case gameState gs of 
  Menu -> renderGameMenu gs
  _ -> pictures
    [ addTranslate (translateGameZone (renderMap gs assets))
    , addTranslate (translateGameZone (renderGates (gates gs)))
    , addTranslate (translateGameZone (renderTowers (towers gs)))
    , translateGameZone (renderEnemies (enemies gs) assets)
    , translateGameZone (renderProjectiles (projectiles gs))
    , renderUI gs
    , if (gameState gs) == GameOver then renderGameOver else blank
    ]

renderMap :: GameState -> Assets -> Picture
renderMap gs assets = pictures (concatMap renderRow (zip [0..] (tiles gs)))
  where
    renderRow (y, row) = map (renderTile y) (zip [0..] row)
    renderTile y (x, tile) = case tile of 
      Road -> pictures (map (translate (fromIntegral x * tileSize) (fromIntegral y * tileSize)) 
        [(scale 0.043 0.043 (roadBlockImg assets)), (rectangleWire tileSize tileSize)])

      Buildable -> pictures (map (translate (fromIntegral x * tileSize) (fromIntegral y * tileSize)) 
        [(scale 0.26 0.26 (buildableBlockImg assets)), (rectangleWire tileSize tileSize)])
      _ ->
        let 
          pos = (fromIntegral x * tileSize, fromIntegral y * tileSize)
          color = case tile of
            Road -> roadColor
            Buildable -> buildableColor
            Neutral -> neutralColor
            Finish -> finishColor
            Start -> startColor
        in translate (fst pos) (snd pos) (colorRectangle color tileSize tileSize)

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

renderGates :: [Gates] -> Picture
renderGates = pictures . map renderGate
  where
    renderGate gate = let pos = gatesPosition gate in
      pictures [translate (fst pos) (snd pos) (colorRectangle magenta tileSize tileSize)
        , translate (fst pos) (snd pos) (healthBar (gatesHealth gate) (gatesDefaultHealth))
        ]

renderEnemies :: [Enemy] -> Assets -> Picture
renderEnemies ens assets = pictures (map renderEnemy ens)
  where
    renderEnemy enemy = pictures
      [ translate x y (color (enemyColor (enemyType enemy)) (scale 0.03 0.03 enemyBody))  -- Enemy body
      , translate x (y - 20) (healthBar (enemyHealth enemy) (enemyMaxHealth enemy))
      ]
      where
        (x, y) = enemyPosition enemy
        
        enemyBody = case enemyType enemy of 
          BasicEnemy -> basicEnemyImg assets
          StrongEnemy -> strongEnemyImg assets 
          Boss -> bossImg assets

healthBar :: Float -> Float -> Picture
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
  ] ++ map renderButton gameButtons)

renderGameOver :: Picture
renderGameOver = pictures 
  [ translate (-100) 0 (scale 0.3 0.3 (color red (text "GAME OVER")))
  , translate (-200) (-100) (scale 0.3 0.3 (color red (text "(press Space to restart)")))
  ]

renderGameMenu :: GameState -> Picture 
renderGameMenu gs = pictures ((color white (rectangleSolid 2000 2000)) : (map renderButton menuButtons))
  
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
    return Assets 
      { basicEnemyImg = basicEnemy
      , strongEnemyImg = strongEnemy
      , bossImg = boss
      , roadBlockImg = roadBlock
      , buildableBlockImg = grassBlock
      , spawnBlockImg = roadBlock
      , finishBlockImg = roadBlock
      , rockBlockImg = roadBlock
      }
