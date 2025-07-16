module Logic where

import Types
import Config
import Pathfinding
import Graphics.Gloss.Data.Vector (mulSV)
import Data.List (find, foldl')
import System.Random (randomR)

-- Just euclidian distance
distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

applyGatesDamage :: [Gates] -> [Enemy] -> ([Gates], [Enemy])
applyGatesDamage [] enemies = ([], enemies)
applyGatesDamage gates enemies = foldl' processGate ([], enemies) gates
  where
    processGate (remainingGates, currentEnemies) gate = 
      let (gate', enemies') = tics gate currentEnemies []
      in if gatesHealth gate' <= 0 then (remainingGates, enemies') else (gate':remainingGates, enemies')

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
      case find (isHit proj) currentEnemies of
        Nothing -> (proj : remainingProjs, currentEnemies)  -- Miss
        Just enemy -> case projType proj of
          SplashTower -> (remainingProjs, map (isUnderSplash proj) currentEnemies)
          _ ->
            let damagedEnemy = applyEffect proj enemy
            in (remainingProjs, damagedEnemy : filter (/= enemy) currentEnemies)

    isHit proj enemy = distance (projPosition proj) (enemyPosition enemy) < hitRadius
    applyEffect proj enemy = case projType proj of
      CannonTower -> enemy { enemyHealth = enemyHealth enemy - projDamage proj}
      SlowTower ->
        enemy {enemySpeed = if enemySpeed enemy > 25 
        then  enemySpeed enemy * slowTowerCoef else enemySpeed enemy}
      SplashTower -> enemy { enemyHealth = enemyHealth enemy - projDamage proj}
    
    isUnderSplash projectile enemy =
      if distance (projPosition projectile) (enemyPosition enemy) <= splashTowerSplashRadius
      then enemy { enemyHealth = enemyHealth enemy - projDamage projectile}
      else enemy

getNextWaveType :: WaveType -> WaveType
getNextWaveType BasicWave = FirstWave
getNextWaveType FirstWave = SecondWave
getNextWaveType SecondWave = ThirdWave
getNextWaveType ThirdWave = LastWave 
getNextWaveType LastWave = BasicWave

spawnEnemies :: Float -> GameState -> GameState
spawnEnemies dt gs
  | timeSinceLastWave gs > 5 && null (enemies gs) && null (waveEnemies gs) = 
    prepareNextWave gs
  | spawnTimer gs <= 0 && not (null (waveEnemies gs)) = 
    spawnNextEnemy gs
  | null (enemies gs) && null (waveEnemies gs) = gs {timeSinceLastWave = timeSinceLastWave gs + dt}
  | otherwise = gs {spawnTimer = spawnTimer gs - dt}


spawnNextEnemy :: GameState -> GameState
spawnNextEnemy gs = case waveEnemies gs of 
  [] -> gs 
  (x:xs) -> let (_, _, spawnInt) = getWaveConfig (currentWave gs) in gs 
    { enemies = enemies gs ++ [x {enemyPosition = head (getEnemyPath (tiles gs) (randomGen gs))
                                  , enemyPath = getEnemyPath (tiles gs) (randomGen gs)}]
    , waveEnemies = xs 
    , spawnTimer = spawnInt}

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


updateGame :: Float -> GameState -> GameState
updateGame delta gs = case gameState gs of 
  Menu -> gs
  GameProcess -> foldl (\acc f -> f acc) updatedGS updates
  GameOver -> gs

  where
    -- Update projectiles and enemies with collision
    (remainingProjectiles, shootedEnemies) = updateProjectiles (projectiles gs) (enemies gs)
    (gates', updatedEnemies) = applyGatesDamage (gates gs) (shootedEnemies)
    
    -- Add coins for killed enemies
    coinsEarned = sum [enemyValue e | e <- updatedEnemies, enemyHealth e <= 0]
    
    (_, newGen) = randomR (1 :: Int, 100 :: Int) (randomGen gs)

    updatedEnemies' = moveEnemies delta (filter (\e -> enemyHealth e > 0) updatedEnemies)
    -- Update state
    updatedGS = gs
      { enemies = updatedEnemies'
      , projectiles = filter hasNotReachedTarget (map (\x -> moveProjectile delta x) remainingProjectiles)
      , coins = coins gs + coinsEarned
      , gates = gates'
      , timeSinceLastWave = timeSinceLastWave gs + delta
      , randomGen = newGen
      }
    updates = 
      [ spawnEnemies delta
      , \s -> towersAttack delta s
      , checkGameOver
      ]
    
    hasNotReachedTarget proj = case projTarget proj of
      Nothing -> False
      Just enemy -> enemyPosition enemy /= projPosition proj

moveProjectile :: Float -> Projectile -> Projectile
moveProjectile delta proj = case projTarget proj of
  Nothing -> proj  -- No target (shouldn't happen, just to complete the function)
  Just enemy ->
    let 
      (px, py) = projPosition proj
      (ex, ey) = enemyPosition enemy
      dx = ex - px
      dy = ey - py
      dist = sqrt (dx*dx + dy*dy)
      moveDist = projSpeed proj * delta
    in 
      if dist <= moveDist
      then proj { projPosition = (ex, ey) }
      else proj { projPosition = (px + dx/dist*moveDist, py + dy/dist*moveDist) }

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies delta = map updateEnemy
  where
    updateEnemy e = e 
      { enemyPosition = moveAlongPath e delta
      , enemyCurrentTarget = if reachedNextPoint e delta then enemyCurrentTarget e + 1 else enemyCurrentTarget e
      }

moveAlongPath :: Enemy -> Float -> Position
moveAlongPath e delta = 
  let 
    (x, y) = enemyPosition e
    path = enemyPath e
    currentIdx = enemyCurrentTarget e
    nextPos = path !! min (currentIdx + 1) (length path - 1)
    (tx, ty) = nextPos
    dx = tx - x
    dy = ty - y
    dist = sqrt (dx*dx + dy*dy)
    speed = enemySpeed e * delta
    ratio = if dist > 0 then speed / dist else 0
  in (x + dx * ratio, y + dy * ratio)

reachedNextPoint :: Enemy -> Float -> Bool
reachedNextPoint e delta =
  let 
    (x, y) = enemyPosition e
    path = enemyPath e
    currentIdx = enemyCurrentTarget e
    nextPos = path !! min (currentIdx + 1) (length path - 1)
    (tx, ty) = nextPos
    dx = tx - x
    dy = ty - y
    distSq = dx*dx + dy*dy
  in distSq <= (enemySpeed e * delta) ^ 2

towersAttack :: Float -> GameState -> GameState
towersAttack delta gs = foldl (attackWithTower delta) gs (towers gs)
  where
    attackWithTower delta acc tower
      | towerTimeSinceLastShot tower >= towerCooldown tower =
        case findTarget tower (enemies acc) of
          Just enemy -> 
            let 
              newProj = createProjectile tower enemy
              newTower = tower { towerTimeSinceLastShot = 0 }
            in acc
              { projectiles = newProj : projectiles acc
              , towers = newTower : filter (/= tower) (towers acc)
              }
          Nothing -> acc
      | otherwise = 
        acc { towers = tower { towerTimeSinceLastShot = towerTimeSinceLastShot tower + delta } 
          : filter (/= tower) (towers acc) }

findTarget :: Tower -> [Enemy] -> Maybe Enemy
findTarget tower enemiesInRange = 
  let inRange = filter (\e -> distance (towerPosition tower) (enemyPosition e) <= towerRange tower) enemiesInRange
  in case inRange of
    [] -> Nothing
    es -> Just (head es)  -- Simple targeting - first enemy in range

createProjectile :: Tower -> Enemy -> Projectile
createProjectile tower enemy = Projectile
  { projPosition = transferProjStart (towerPosition tower)
  , projType = towerType tower
  , projTarget = Just enemy
  , projDamage = towerDamage tower
  , projSpeed = projectileSpeed -- Pixels per frame
  }

checkGameOver :: GameState -> GameState
checkGameOver gs = if any reachedFinish (enemies gs) then gs { gameState = GameOver } else gs
  where
    reachedFinish e = 
      let 
        (x, y) = enemyPosition e
        (tx, ty) = last (enemyPath e)
        dx = tx - x
        dy = ty - y
      in sqrt (dx*dx + dy*dy) < 5  -- Close enough to finish
