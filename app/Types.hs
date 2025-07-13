module Types
  ( module Types
  , Point, Picture 
  ) where

import Graphics.Gloss (Point, Picture)
import Graphics.Gloss.Data.Color
import System.Random (randomR, mkStdGen, StdGen)


type Health = Float
type Damage = Float
type Speed = Float
type Radius = Float
type Coins = Int
type Time = Float

type Position = (Float, Float)
type TileCoord = (Int, Int)

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
  , projTarget :: Maybe Enemy
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
  , enemyHealth :: Float 
  , enemyMaxHealth :: Float 
  , enemySpeed :: Float
  , enemyPath :: [Position]
  , enemyCurrentTarget :: Int
  , enemyValue :: Int
  } deriving (Eq)

data BuildMode = NotBuilding | Building TowerType
  deriving (Eq, Show)

data GameState = GameState
  { towers :: [Tower]
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
  , spawnTimer :: Float
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
    }

startBuilding :: TowerType -> GameState -> GameState
startBuilding towerType gs = gs { buildMode = Building towerType }

startMap :: [[TileType]] -> GameState -> GameState 
startMap tilemap gs = gs { tiles = tilemap, gameState = GameProcess }