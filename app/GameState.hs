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
  , gameOver = False
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

n, r, b, f :: TileType
n = Neutral 
r = Road 
b = Buildable 
f = Finish
s = Start

sampleMap :: [[TileType]]
sampleMap =
  [[n, n, n, n, n, n, n, n, s, n, n, n, n],
   [n, n, n, n, n, n, n, b, r, n, n, n, n],
   [n, n, n, n, n, n, b, b, r, n, n, n, n], 
   [n, n, n, n, n, b, b, b, r, n, n, n, n], 
   [n, n, n, n, n, b, b, b, r, n, n, n, n], 
   [n, b, b, r, r, r, r, r, r, b, n, n, n], 
   [n, b, b, r, b, b, b, b, r, b, b, n, n], 
   [n, r, r, r, b, b, b, b, r, b, b, b, n], 
   [n, r, b, b, n, n, n, n, r, r, r, r, f], 
   [n, r, b, b, n, n, n, n, n, n, n, n, n],
   [n, r, b, b, b, b, b, n, n, n, n, n, n],
   [n, r, r, r, r, r, r, n, n, n, n, n, n],
   [n, n, n, n, n, n, f, n, n, n, n, n, n]]
   