module Config where

import Graphics.Gloss.Data.Color
import Types

-- Display configuration
windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 600

tileSize :: Float
tileSize = 40.0

-- Game balance constants
initialCoins :: Int
initialCoins = 100

cannonTowerCost :: Int
cannonTowerCost = 50

slowTowerCost :: Int
slowTowerCost = 75

cannonTowerDamage :: Float
cannonTowerDamage = 10.0

slowTowerSlowFactor :: Float
slowTowerSlowFactor = 0.5  -- Reduces enemy speed by 50%

enemyReward :: Int
enemyReward = 20

-- Colors
roadColor :: Color
roadColor = makeColor 0.5 0.35 0.05 1.0  -- Brown

buildableColor :: Color
buildableColor = makeColor 0.1 0.8 0.1 1.0  -- Green

neutralColor :: Color
neutralColor = makeColor 0.7 0.7 0.7 1.0  -- Gray

finishColor :: Color
finishColor = makeColor 1.0 0.0 0.0 1.0  -- Red

towerColors :: TowerType -> Color
towerColors CannonTower = makeColor 0.0 0.0 1.0 1.0  -- Blue
towerColors SlowTower = makeColor 1.0 0.5 0.0 1.0   -- Orange

enemyColor :: EnemyType -> Color
enemyColor BasicEnemy = makeColor 0.8 0.2 0.2 1.0  -- Red