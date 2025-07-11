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
  , waveNumber = 0
  , timeSinceLastWave = 0
  , randomGen = mkStdGen initGen
  }

-- Helper functions for state manipulation
addTower :: Tower -> GameState -> GameState
addTower tower gs = gs { towers = tower : towers gs }

removeEnemy :: Enemy -> GameState -> GameState
removeEnemy enemy gs = gs { enemies = filter (/= enemy) (enemies gs) }

addProjectile :: Projectile -> GameState -> GameState
addProjectile proj gs = gs { projectiles = proj : projectiles gs }

addCoins :: Coins -> GameState -> GameState
addCoins amount gs = gs { coins = coins gs + amount }
