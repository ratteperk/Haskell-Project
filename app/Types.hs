module Types
    ( module Types
    , Point, Picture  -- Re-export from Gloss
    ) where

import Graphics.Gloss (Point, Picture)
import Graphics.Gloss.Data.Color


-- Basic types
type Health = Float
type Damage = Float
type Speed = Float
type Radius = Float
type Coins = Int
type Time = Float

-- Position and dimensions
type Position = (Float, Float)
type TileCoord = (Int, Int)

-- Game entities
data TileType = Road | Buildable | Neutral | Finish
    deriving (Eq, Show)

data TowerType = CannonTower | SlowTower
    deriving (Eq, Show)

data EnemyType = BasicEnemy
    deriving (Eq, Show)

data Projectile = Projectile
    { projPosition :: Position
    , projTarget :: Maybe Enemy  -- Target enemy
    , projDamage :: Damage
    , projSpeed :: Speed
    }

data Tower = Tower
    { towerPosition :: Position
    , towerType :: TowerType
    , towerDamage :: Damage
    , towerRange :: Radius
    , towerCooldown :: Time
    , towerTimeSinceLastShot :: Time
    } deriving (Eq)

data Enemy = Enemy
    { enemyPosition :: Position
    , enemyType :: EnemyType
    , enemyHealth :: Health
    , enemySpeed :: Speed
    , enemyPath :: [Position]  -- Precomputed path
    , enemyCurrentTarget :: Int  -- Index in path
    } deriving (Eq)

data BuildMode = NotBuilding | Building TowerType
    deriving (Eq, Show)

data GameState = GameState
    { towers :: [Tower]
    , enemies :: [Enemy]
    , projectiles :: [Projectile]
    , coins :: Coins
    , buildMode :: BuildMode
    , tiles :: [[TileType]]  -- 2D grid representing the map
    , gameOver :: Bool
    , waveNumber :: Int
    , timeSinceLastWave :: Time
    }

-- In Types.hs
data UIElement = Button
    { btnPosition :: Position
    , btnSize :: (Float, Float)
    , btnAction :: GameState -> GameState
    , btnLabel :: String
    , btnColor :: Color
    }

startBuilding :: TowerType -> GameState -> GameState
startBuilding towerType gs = gs { buildMode = Building towerType }