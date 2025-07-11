module Config where

import System.Random (randomR, mkStdGen)
import Graphics.Gloss.Data.Color
import Types
import Data.List (find)

-- Display configuration
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

tileSize :: Float
tileSize = 40

xOffset, yOffset :: Float
yOffset = 200
xOffset = 150

transferProjStart :: Position -> Position
transferProjStart (x, y) = (x + tileSize/2, y + tileSize/2)

-- Enemy section

basicEnemyHealth, strongEnemyHealth, bossHealth :: Float 
basicEnemyHealth = 100
strongEnemyHealth = 200
bossHealth = 1700

basicEnemySpeed, strongEnemySpeed, bossSpeed :: Float
basicEnemySpeed = 50
strongEnemySpeed = 30
bossSpeed = 20

basicEnemyValue, strongEnemyValue,bossValue :: Int 
basicEnemyValue = 20
strongEnemyValue = 40
bossValue = 200

startPos :: Position
startPos = (0, 0)

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

-- Predefined enemy waves
bwEnemies, fwEnemies, swEnemies, lwEnemies :: [Enemy]
bwEnemies = [basicEnemy, basicEnemy]

fwEnemies = [basicEnemy, basicEnemy, basicEnemy]

swEnemies = [strongEnemy, strongEnemy, strongEnemy]

twEnemies = fwEnemies ++ [strongEnemy, basicEnemy, strongEnemy, basicEnemy] ++ swEnemies

lwEnemies = swEnemies ++ swEnemies ++ [boss]


-- Projectiles section

projectileSpeed :: Float
projectileSpeed = 900

hitRadius :: Float
hitRadius = 30  -- Radius for projectile collision detection

-- Tower section

cannonTowerCost, slowTowerCost, splashTowerCost :: Int
cannonTowerCost = 50
slowTowerCost = 75
splashTowerCost = 100

cannonTowerDamage, slowTowerDamage, splashTowerDamage :: Float
cannonTowerDamage = 15
slowTowerDamage = 5
splashTowerDamage = 25


cannonTowerRange, slowTowerRange, splashTowerRange :: Float
cannonTowerRange = 150
slowTowerRange = 120
splashTowerRange = 135

cannonTowerCooldown, slowTowerCooldown, splashTowerCooldown :: Float
cannonTowerCooldown = 1
slowTowerCooldown = 2
splashTowerCooldown = 1.5


-- Specific constants:

slowTowerCoef :: Float
slowTowerCoef = 0.75

splashTowerSplashRadius :: Float
splashTowerSplashRadius = 50


-- Colors
roadColor :: Color
roadColor = makeColor 0.5 0.35 0.05 10  -- Brown

buildableColor :: Color
buildableColor = makeColor 0.1 0.8 0.1 1.0 -- Green that less bright than "green" from gloss

neutralColor :: Color
neutralColor = makeColor 0.7 0.7 0.7 10  -- Gray

finishColor :: Color
finishColor = red

startColor :: Color
startColor = yellow

towerColors :: TowerType -> Color
towerColors CannonTower = blue
towerColors SlowTower = orange
towerColors SplashTower = violet

enemyColor :: EnemyType -> Color
enemyColor BasicEnemy = makeColor 0.8 0.2 0.2 1.0 -- Red that less bright than "red" from gloss
enemyColor StrongEnemy = blue
enemyColor Boss = orange

-- Waves section

waveConfigs :: [(WaveType, [Enemy], Float)]
waveConfigs = 
  [ (BasicWave, bwEnemies, 0.8)
  , (FirstWave, fwEnemies, 0.5)
  , (SecondWave, swEnemies, 1)
  , (ThirdWave, twEnemies, 1)
  , (LastWave, lwEnemies, 1.5)]

getWaveConfig :: WaveType -> (WaveType, [Enemy], Float)
getWaveConfig wt = 
  case find (\(t,_,_) -> t == wt) waveConfigs of
    Just cfg -> cfg
    Nothing -> (BasicWave, bwEnemies, 1)

-- Game section

initGen :: Int 
initGen = 2 -- Constant to initialize generator field in initialState (although in Main 
            -- generator initial value is generated in runtime)

initialCoins :: Int
initialCoins = 200