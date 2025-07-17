module Logic where

import Types
import Config
import Pathfinding
import Data.List (find, foldl')
import Data.Maybe (listToMaybe)
import System.Random (randomR)

-- Just euclidian distance in pixels
distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- Applies damage effect of gates: they damaging yourself while damaging enemies 
applyGatesDamage :: [Gates] -> [Enemy] -> ([Gates], [Enemy])
applyGatesDamage [] enemies = ([], enemies) -- Base case: no gates
applyGatesDamage gates enemies = foldl' processGate ([], enemies) gates
  where
    processGate (remainingGates, currentEnemies) gate = -- automatically removes "killed" gates there
      let (gate', enemies') = tics gate currentEnemies []
      in if gatesHealth gate' <= 0 then (remainingGates, enemies') else (gate':remainingGates, enemies')

    -- Damage function
    tics g [] accum = (g, accum)
    tics g (e:es) accum =
      if distance (enemyPosition e) (gatesPosition g) <= gatesHitRadius
      then tics (g {gatesHealth = gatesHealth g - gatesDefaultDamage}) es
        (e {enemyHealth = enemyHealth e - gatesDefaultDamage} : accum)
      else tics g es (e:accum)

-- Returns updated projectiles and enemies
updateProjectiles :: [Projectile] -> [Enemy] -> ([Projectile], [Enemy])
updateProjectiles [] enemies = ([], enemies)  -- Base case: no projectiles
updateProjectiles projectiles enemies =
  foldl' processProjectile ([], enemies) projectiles
  where
    processProjectile (remainingProjs, currentEnemies) proj =

      case find (isHit proj) currentEnemies of -- Detecting collisions:
        Nothing -> (proj : remainingProjs, currentEnemies) -- Miss
        Just enemy -> case projType proj of -- Hit
          SplashTower -> (remainingProjs, map (isUnderSplash proj) currentEnemies) -- Applying splash
          _ ->
            -- Applying additional effect of the tower:
            let damagedEnemy = applyEffect proj enemy
            -- and removing old enemy from the list
            in (remainingProjs, damagedEnemy : filter (/= enemy) currentEnemies)

    -- Collision detection function
    isHit proj enemy = distance (projPosition proj) (enemyPosition enemy) < hitRadius

    -- Applying additional effect of the tower:
    applyEffect proj enemy = case projType proj of
      CannonTower -> enemy { enemyHealth = enemyHealth enemy - projDamage proj}
      SlowTower ->
        enemy {enemyHealth = enemyHealth enemy - projDamage proj, 
          enemySpeed = if enemySpeed enemy > 25 
          then  enemySpeed enemy * slowTowerCoef else enemySpeed enemy}
      SplashTower -> enemy { enemyHealth = enemyHealth enemy - projDamage proj}
    
    -- Applying splash damage:
    isUnderSplash projectile enemy =
      if distance (projPosition projectile) (enemyPosition enemy) <= splashTowerSplashRadius
      then enemy { enemyHealth = enemyHealth enemy - projDamage projectile}
      else enemy

-- Main enemies spawn function
spawnEnemies :: Float -> GameState -> GameState
spawnEnemies dt gs
  | currentWave gs == StopWave = initialState {completedMaps = getByUIElem gs}
  -- Spawning the next wave
  | timeSinceLastWave gs > waveSeparateTime && null (enemies gs) && null (waveEnemies gs) = prepareNextWave gs
  -- Spawning the next enemy
  | spawnTimer gs <= 0 && not (null (waveEnemies gs)) = spawnNextEnemy gs
  -- Waiting for the next wave
  | null (enemies gs) && null (waveEnemies gs) = gs {timeSinceLastWave = timeSinceLastWave gs + dt}
  -- Waiting for the next enemy in wave
  | otherwise = gs {spawnTimer = spawnTimer gs - dt}

getByUIElem :: GameState -> [UIElement]
getByUIElem gs = uiOfMap : (completedMaps gs)
  where 
    uiOfMap 
      | (tiles gs) == sampleMap1 = menuButtons !! 0
      | (tiles gs) == sampleMap2 = menuButtons !! 1
      | otherwise = menuButtons !! 2

-- Spawns the next enemy in wave
spawnNextEnemy :: GameState -> GameState
spawnNextEnemy gs = case waveEnemies gs of 
  [] -> gs 
  (x:xs) -> let (_, _, spawnInt) = getWaveConfig (currentWave gs) in gs 
    { enemies = enemies gs ++ [x {enemyPosition = head (getEnemyPath (tiles gs) (randomGen gs))
    , enemyPath = getEnemyPath (tiles gs) (randomGen gs)}]
    , waveEnemies = xs 
    , spawnTimer = spawnInt
    }

-- Just sets the next wave to the GameState
prepareNextWave :: GameState -> GameState
prepareNextWave gs = 
  let 
    nextWaveType = getNextWaveType (currentWave gs)
    (_, newEnemies, interval) = getWaveConfig nextWaveType
  in gs 
    { currentWave = nextWaveType 
    , waveEnemies = newEnemies
    , spawnTimer = interval 
    , timeSinceLastWave = 0
    }

-- Updating function for entire game:
updateGame :: Float -> GameState -> GameState
updateGame delta gs = case gameState gs of 
  Menu -> gs -- just waiting while user click some button
  GameOver -> gs
  GameProcess -> foldl' (\state f -> f state) updatedGS updates
    where
      -- Update projectiles and enemies with collision
      (remainingProjectiles, shootedEnemies) = updateProjectiles (projectiles gs) (enemies gs)
      (gates', updatedEnemies) = applyGatesDamage (gates gs) (shootedEnemies)
      
      -- Add coins for killed enemies
      coinsEarned = sum [enemyValue e | e <- updatedEnemies, enemyHealth e <= 0]
      
      -- Creating a new generator:
      (_, newGen) = randomR (1 :: Int, 100 :: Int) (randomGen gs)

      -- Moving (and filtering) alive enemies
      updatedEnemies' = moveEnemies delta (gates gs) (filter (\e -> enemyHealth e > 0) updatedEnemies)

      -- Update state
      updatedGS = gs
        { enemies = updatedEnemies'
        , projectiles = filter hasNotReachedTarget (map (\x -> moveProjectile delta x) remainingProjectiles)
        , coins = coins gs + coinsEarned
        , gates = gates'
        , timeSinceLastWave = timeSinceLastWave gs + delta
        , randomGen = newGen
        }

      -- Independent checks
      updates = 
        [ spawnEnemies delta
        , \s -> towersAttack delta s
        , checkGameOver
        ]
      
      hasNotReachedTarget proj = projTarget proj /= projPosition proj

-- Moves projectile in the direction of the target:
moveProjectile :: Float -> Projectile -> Projectile
moveProjectile delta proj =
  let 
    (px, py) = projPosition proj
    (ex, ey) = projTarget proj
    dx = ex - px
    dy = ey - py
    dist = distance (px, py) (ex, ey)
    moveDist = projSpeed proj * delta -- distance that projectile passes during one frame
  in 
    if dist <= moveDist
    then proj {projPosition = (ex, ey)}
    else proj {projPosition = (px + dx/dist*moveDist, py + dy/dist*moveDist)}

-- Moving of enemies (considering gates stopping effect)
moveEnemies :: Float -> [Gates] -> [Enemy] -> [Enemy]
moveEnemies delta gates = map updateEnemy
  where
    updateEnemy e = e
      { enemyPosition = newPos e
      , enemyCurrentTarget = if reachedNextPoint e then enemyCurrentTarget e + 1 else enemyCurrentTarget e
      }

    newPos e = -- if an enemy is very close to some gates then it stops moving
      case find (\x -> distance (gatesPosition x) (enemyPosition e) <= gatesHitRadius) gates of
        Nothing -> moveAlongPath e delta
        Just _ -> enemyPosition e

-- Moves enemy to the next point of its path
moveAlongPath :: Enemy -> Float -> Position
moveAlongPath e delta = 
  let 
    (x, y) = enemyPosition e
    (nextX, nextY) = enemyPath e !! min (enemyCurrentTarget e + 1) (length (enemyPath e) - 1) -- next point
    r = enemySpeed e * delta / distance (nextX, nextY) (x, y) -- ratio of distance to walk during this frame and
    -- remaining distance 
  in
    (x + (nextX - x) * r, y + (nextY- y) * r)

-- Returns the distance to the next point on the path
getDistanceToNextPoint :: Enemy -> Float
getDistanceToNextPoint e = distance p1 p2
  where
    p1 = enemyPosition e
    p2 = enemyPath e !! min (enemyCurrentTarget e + 1) (length (enemyPath e) - 1)

-- Checks whether an enemy reached the next point of its path or not 
reachedNextPoint :: Enemy -> Bool
reachedNextPoint e = getDistanceToNextPoint e <= 2 -- 2-pixels precision is needed due to floating-point issues

-- Performs attack of towers
towersAttack :: Float -> GameState -> GameState
towersAttack delta gs = foldl' (attackWithTower delta) gs (towers gs)
  where
    attackWithTower delta game tower
      | towerTimeSinceLastShot tower >= towerCooldown tower =
        case findTarget tower (enemies game) of
          Just enemyPos ->
            let 
              newProj = createProjectile tower enemyPos
              newTower = tower {towerTimeSinceLastShot = 0}
            in game
              { projectiles = newProj : projectiles game
              , towers = newTower : filter (/= tower) (towers game)
              }
          Nothing -> game
      | otherwise = 
        game {towers = tower {towerTimeSinceLastShot = towerTimeSinceLastShot tower + delta} 
          : filter (/= tower) (towers game)}

-- Find the closest to finish enemy in the scope
findTarget :: Tower -> [Enemy] -> Maybe Position
findTarget tower enemiesInRange = findNearestToFinish inRange []
  where
    -- filtered enemies
    inRange = filter (\e -> distance (towerPosition tower) (enemyPosition e) <= towerRange tower) enemiesInRange

    findNearestToFinish [] accum = case listToMaybe accum of
      Nothing -> Nothing
      Just e -> Just (enemyPosition e)
    findNearestToFinish (e:es) accum = case listToMaybe accum of
      Nothing -> findNearestToFinish es [e]
      Just en -> if (isFirstCloserToFinish e en) then findNearestToFinish es [e] else findNearestToFinish es [en]
    
    -- returns amount of remaining tiles (that enemy should traverse to reach the finish)
    remainingTiles e = length (enemyPath e) - enemyCurrentTarget e

    isFirstCloserToFinish e1 e2 = case remainingTiles e1 == remainingTiles e2 of
      True -> getDistanceToNextPoint e1 < getDistanceToNextPoint e2
      False -> remainingTiles e1 < remainingTiles e2

-- Just creates projectile
createProjectile :: Tower -> Position -> Projectile
createProjectile tower pos = Projectile
  { projPosition = towerPosition tower
  , projType = towerType tower
  , projTarget = pos
  , projDamage = towerDamage tower
  , projSpeed = projectileSpeed -- Pixels per frame
  }

-- Checks whether any enemy is close enough to finish
checkGameOver :: GameState -> GameState
checkGameOver gs = if any reachedFinish (enemies gs) then gs { gameState = GameOver } else gs
  where
    reachedFinish e = 
      let 
        p1 = enemyPosition e
        p2 = last (enemyPath e)
      in distance p1 p2 < 5 
