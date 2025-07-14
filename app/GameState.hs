module GameState where

import Config
import Types
import System.Random (mkStdGen)

initialState :: [[TileType]] -> GameState
initialState mapTiles = GameState
  { towers = []
  , enemies = []
  , projectiles = []
  , coins = initialCoins
  , buildMode = NotBuilding
  , tiles = mapTiles
  , gameState = Menu
  , timeSinceLastWave = 0
  , randomGen = mkStdGen initGen -- Constant is needed to just initialize the field 
  , currentWave = BasicWave      -- (in Main gen is generated in runtime)
  , waveEnemies = []
  , spawnTimer = 0
  }

addTower :: Tower -> GameState -> GameState
addTower tower gs = gs { towers = tower : towers gs }

removeEnemy :: Enemy -> GameState -> GameState
removeEnemy enemy gs = gs { enemies = filter (/= enemy) (enemies gs) }

addProjectile :: Projectile -> GameState -> GameState
addProjectile proj gs = gs { projectiles = proj : projectiles gs }

addCoins :: Coins -> GameState -> GameState
addCoins amount gs = gs { coins = coins gs + amount }
