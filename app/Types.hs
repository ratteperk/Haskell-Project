module Types where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Data.Color
import System.Random (randomR, mkStdGen, StdGen)

-- Synonyms
type Health = Float
type Damage = Float
type Speed = Float
type Radius = Float
type Coins = Int
type Time = Float

type Position = (Float, Float) -- Coordinates in pixels
type TileCoord = (Int, Int) -- Coordinates on 2D grid map

data TileType = Road | Buildable | Neutral | Finish | Start
  deriving (Eq, Show)

data TowerType = CannonTower | SlowTower | SplashTower
  deriving (Eq, Show)

data EnemyType = BasicEnemy | StrongEnemy | Boss
  deriving (Eq, Show)

data States = Menu | GameProcess | GameOver
  deriving (Eq, Show)

data Projectile = Projectile
  { projPosition :: Position
  , projType :: TowerType
  , projTarget :: Position
  , projDamage :: Damage
  , projSpeed :: Speed
  }

data Gates = Gates 
  { gatesHealth :: Health
  , gatesPosition :: Position
  -- damage is fixed for all gates, so there is no need to store it in every instance
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
  , enemyMaxHealth :: Health
  , enemySpeed :: Speed -- Pixels per second
  , enemyPath :: [Position] -- List of points that enemy traverses (centers of road tiles)
  , enemyCurrentTarget :: Int -- Index of the point that enemy is walking to
  , enemyValue :: Coins -- Amount of coins that that enemy leaves after dying
  } deriving (Eq)

data BuildMode = NotBuilding | Building TowerType | Removing | GatesBuilding
  deriving (Eq, Show)

data GameState = GameState
  { towers :: [Tower]
  , gates :: [Gates]
  , enemies :: [Enemy]
  , projectiles :: [Projectile]
  , coins :: Coins
  , buildMode :: BuildMode
  , tiles :: [[TileType]] 
  , gameState :: States
  , timeSinceLastWave :: Time
  , randomGen :: StdGen
  , currentWave :: WaveType
  , waveEnemies :: [Enemy]
  , spawnTimer :: Time
  }

data UIElement = Button
  { btnPosition :: Position
  , btnSize :: (Float, Float)
  , btnAction :: GameState -> GameState
  , btnLabel :: String
  , btnColor :: Color
  }

data WaveType 
  = BasicWave 
  | FirstWave
  | SecondWave
  | ThirdWave
  | LastWave
  deriving (Eq, Show)

data Assets = Assets
    { basicEnemyImg :: Picture
    , strongEnemyImg :: Picture
    , bossImg :: Picture
    , roadBlockImg :: Picture
    , buildableBlockImg :: Picture
    , spawnBlockImg :: Picture 
    , finishBlockImg :: Picture
    , rockBlockImg :: Picture
    }
