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

xOffset, yOffset :: Float
yOffset = 200.0
xOffset = 150.0

transferProjStart :: Position -> Position
transferProjStart (x, y) = (x + tileSize/2, y + tileSize/2)

-- Game balance constants

basicEnemyHealth :: Float
basicEnemyHealth = 100.0

basicEnemySpeed :: Float
basicEnemySpeed = 50.0

basicEnemyValue :: Int
basicEnemyValue = 25

projectileSpeed :: Float
projectileSpeed = 900.0

hitRadius :: Float
hitRadius = 30.0  -- Radius for projectile collision detection

initialCoins :: Int
initialCoins = 200

cannonTowerCost :: Int
cannonTowerCost = 50

slowTowerCost :: Int
slowTowerCost = 75

cannonTowerDamage :: Float
cannonTowerDamage = 10.0

slowTowerDamage :: Float
slowTowerDamage = 0


cannonTowerRange, slowTowerRange :: Float
cannonTowerRange = 150
slowTowerRange = 120

cannonTowerCooldown, slowTowerCooldown :: Float
cannonTowerCooldown = 1
slowTowerCooldown = 0

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

startColor :: Color
startColor = yellow

towerColors :: TowerType -> Color
towerColors CannonTower = makeColor 0.0 0.0 1.0 1.0  -- Blue
towerColors SlowTower = makeColor 1.0 0.5 0.0 1.0   -- Orange

enemyColor :: EnemyType -> Color
enemyColor BasicEnemy = makeColor 0.8 0.2 0.2 1.0  -- Red