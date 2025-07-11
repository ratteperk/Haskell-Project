module Config where

import System.Random (randomR, mkStdGen)
import Graphics.Gloss.Data.Color
import Types
import Data.List (find)

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

strongEnemyHealth :: Float 
strongEnemyHealth = 200.0

strongEnemySpeed :: Float
strongEnemySpeed = 30.0

strongEnemyValue :: Int 
strongEnemyValue = 40

bossHealth :: Float 
bossHealth = 1000.0

bossSpeed :: Float 
bossSpeed = 20.0

bossValue :: Int 
bossValue = 200

projectileSpeed :: Float
projectileSpeed = 900.0

hitRadius :: Float
hitRadius = 30.0  -- Radius for projectile collision detection

startPos :: Position
startPos = (0.0, 0.0)

startPath :: [Position]
startPath = []

basicEnemy :: Enemy 
basicEnemy =  Enemy
  { enemyPosition = startPos
  , enemyType = BasicEnemy
  , enemyHealth = basicEnemyHealth
  , enemyMaxHealth = basicEnemyHealth
  , enemyValue = basicEnemyValue
  , enemySpeed = basicEnemySpeed
  , enemyPath = startPath
  , enemyCurrentTarget = 0
  }

strongEnemy :: Enemy 
strongEnemy = Enemy
  {enemyPosition = startPos
  , enemyType = StrongEnemy
  , enemyHealth = strongEnemyHealth
  , enemyMaxHealth = strongEnemyHealth
  , enemyValue = strongEnemyValue
  , enemySpeed = strongEnemySpeed
  , enemyPath = startPath
  , enemyCurrentTarget = 0
  }

boss :: Enemy 
boss = Enemy 
  {enemyPosition = startPos
  , enemyType = Boss
  , enemyHealth = bossHealth
  , enemyMaxHealth = bossHealth
  , enemyValue = bossValue
  , enemySpeed = bossSpeed
  , enemyPath = startPath
  , enemyCurrentTarget = 0
  }

bwEnemies :: [Enemy]
bwEnemies = [basicEnemy, basicEnemy]

fwEnemies :: [Enemy]
fwEnemies = [basicEnemy, basicEnemy, basicEnemy]

swEnemies :: [Enemy]
swEnemies = [strongEnemy, strongEnemy, strongEnemy]

twEnemies :: [Enemy]
twEnemies = fwEnemies ++ [strongEnemy, basicEnemy, strongEnemy, basicEnemy] ++ swEnemies

lwEnemies :: [Enemy]
lwEnemies = swEnemies ++ swEnemies ++ [boss]


initialCoins :: Int
initialCoins = 200

cannonTowerCost :: Int
cannonTowerCost = 50

slowTowerCost :: Int
slowTowerCost = 75

splashTowerCost :: Int
splashTowerCost = 100

cannonTowerDamage :: Float
cannonTowerDamage = 20.0

slowTowerDamage :: Float
slowTowerDamage = 0

splashTowerDamage :: Float
splashTowerDamage = 15

slowTowerCoef :: Float
slowTowerCoef = 0.55

cannonTowerRange, slowTowerRange, splashTowerRange :: Float
cannonTowerRange = 150
slowTowerRange = 120
splashTowerRange = 135

cannonTowerCooldown, slowTowerCooldown, splashTowerCooldown :: Float
cannonTowerCooldown = 1
slowTowerCooldown = 2
splashTowerCooldown = 1.5

slowTowerSlowFactor :: Float
slowTowerSlowFactor = 0.5  -- Reduces enemy speed by 50%

splashTowerSplashRadius :: Float
splashTowerSplashRadius = 50

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
towerColors SplashTower = violet

enemyColor :: EnemyType -> Color
enemyColor BasicEnemy = makeColor 0.8 0.2 0.2 1.0  -- Red
enemyColor StrongEnemy = blue
enemyColor Boss = orange

-- Waves

waveConfigs :: [(WaveType, [Enemy], Float)]
waveConfigs = 
  [ (BasicWave, bwEnemies, 0.8)
  , (FirstWave, fwEnemies, 0.5)
  , (SecondWave, swEnemies, 1.0)
  , (ThirdWave, twEnemies, 1.0)
  , (LastWave, lwEnemies, 1.5)]

getWaveConfig :: WaveType -> (WaveType, [Enemy], Float)
getWaveConfig wt = 
  case find (\(t,_,_) -> t == wt) waveConfigs of
    Just cfg -> cfg
    Nothing -> (BasicWave, bwEnemies, 1.0)

initGen :: Int 
initGen = 2