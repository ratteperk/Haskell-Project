module Logic where

import Types
import Config
import Pathfinding
import Graphics.Gloss.Data.Vector (mulSV)
import Data.List (find, foldl')

-- Add these helper functions
distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

updateProjectiles :: Float -> [Projectile] -> [Projectile]
updateProjectiles delta = map updateProjectile
    where
        updateProjectile proj = 
            case projTarget proj of
                Just enemy -> 
                    let (ex, ey) = enemyPosition enemy
                        (px, py) = projPosition proj
                        dir@(dx, dy) = (ex - px, ey - py)
                        dist = distance (px, py) (ex, ey)
                        speed = projSpeed proj * delta
                        move = if dist > 0 then mulSV (speed/dist) dir else (0, 0)
                    in proj { projPosition = (px + fst move, py + snd move) }
                Nothing -> proj

-- updateProjectiles' :: Float -> [Projectile] -> [Enemy] -> ([Projectile], [Enemy])
-- updateProjectiles' delta projectiles enemies = foldl' processProjectile (projectiles, enemies) projectiles
--     where
--         processProjectile (projs, ens) proj =
--             case find (isHit proj) ens of
--                 Nothing -> (proj:projs, ens)  -- Projectile missed
--                 Just enemy -> 
--                     let damagedEnemy = applyDamage proj enemy
--                     in if enemyHealth damagedEnemy <= 0
--                         then (projs, filter (/= enemy) ens)  -- Enemy died
--                         else (projs, damagedEnemy : filter (/= enemy) ens)

--         isHit proj enemy = distance (projPosition proj) (enemyPosition enemy) < hitRadius

--         applyDamage proj enemy = enemy { enemyHealth = enemyHealth enemy - projDamage proj }

updateProjectiles' :: Float -> [Projectile] -> [Enemy] -> ([Projectile], [Enemy])
updateProjectiles' _ [] enemies = ([], enemies)  -- Base case: no projectiles
updateProjectiles' delta projectiles enemies =
    foldl' processProjectile ([], enemies) projectiles
    where
        processProjectile (remainingProjs, currentEnemies) proj =
            case find (isHit proj) currentEnemies of
                Nothing -> (proj : remainingProjs, currentEnemies)  -- Miss
                Just enemy ->
                    let damagedEnemy = applyDamage proj enemy
                    in if enemyHealth damagedEnemy <= 0
                        then (remainingProjs, filter (/= enemy) currentEnemies)  -- Killed
                        else (remainingProjs, damagedEnemy : filter (/= enemy) currentEnemies)  -- Damaged

        isHit proj enemy = distance (projPosition proj) (enemyPosition enemy) < hitRadius

        applyDamage proj enemy = enemy { enemyHealth = enemyHealth enemy - projDamage proj }

spawnEnemies :: GameState -> GameState
spawnEnemies gs
    | timeSinceLastWave gs > 5 && null (enemies gs) =
        let newEnemies = replicate (waveNumber gs + 3) (createEnemy (head (getEnemyPath (tiles gs))))
        in gs { enemies = newEnemies
             , waveNumber = waveNumber gs + 1
             , timeSinceLastWave = 0 }
    | otherwise = gs
    where
        createEnemy startPos = Enemy
            { enemyPosition = startPos
            , enemyType = BasicEnemy
            , enemyHealth = basicEnemyHealth
            , enemyMaxHealth = basicEnemyHealth
            , enemyValue = basicEnemyValue
            , enemySpeed = basicEnemySpeed
            , enemyPath = getEnemyPath (tiles gs)
            , enemyCurrentTarget = 0
            }

-- Update the foldl in updateGame to use function application
-- Remove one of these duplicate declarations
-- updateGame :: Float -> GameState -> GameState
-- updateGame delta gs 
--     | gameOver gs = gs
--     | otherwise = foldl (\acc f -> f acc) updatedGS updates
--     where
--         updatedGS = gs
--             { enemies = updateEnemies delta (enemies gs)
--             , projectiles = updateProjectiles delta (projectiles gs)
--             , timeSinceLastWave = timeSinceLastWave gs + delta
--             }
--         updates = 
--             [ spawnEnemies
--             , \s -> towersAttack delta s
--             , checkGameOver
--             ]

updateGame :: Float -> GameState -> GameState
updateGame delta gs 
    | gameOver gs = gs
    | otherwise = foldl (\acc f -> f acc) updatedGS updates
    where
        -- Update projectiles and enemies with collision
        (remainingProjectiles, updatedEnemies) = updateProjectiles' delta (projectiles gs) (enemies gs)
        
        -- Add coins for killed enemies
        coinsEarned = sum [enemyValue e | e <- enemies gs, e `notElem` updatedEnemies]
        
        -- Update state
        updatedGS = gs
            { enemies = updateEnemies delta updatedEnemies
            , projectiles = map (\x -> moveProjectile delta x) remainingProjectiles
            , coins = coins gs + coinsEarned
            , timeSinceLastWave = timeSinceLastWave gs + delta
            }
        updates = 
            [ spawnEnemies
            , \s -> towersAttack delta s
            , checkGameOver
            ]

moveProjectile :: Float -> Projectile -> Projectile
moveProjectile delta proj = case projTarget proj of
    Nothing -> proj  -- No target, shouldn't happen
    Just enemy ->
        let (px, py) = projPosition proj
            (ex, ey) = enemyPosition enemy
            dx = ex - px
            dy = ey - py
            dist = sqrt (dx*dx + dy*dy)
            moveDist = projSpeed proj * delta
        in proj { projPosition = (px + dx/dist*moveDist, py + dy/dist*moveDist) }
            -- if dist <= moveDist
            -- then proj { projPosition = (ex, ey) }  -- Reached target
            -- else 



updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies delta = map updateEnemy
    where
        updateEnemy e = e 
            { enemyPosition = moveAlongPath e delta
            , enemyCurrentTarget = if reachedNextPoint e delta 
                                    then enemyCurrentTarget e + 1 
                                    else enemyCurrentTarget e
            }

moveAlongPath :: Enemy -> Float -> Position
moveAlongPath e delta = 
    let (x, y) = enemyPosition e
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
    let (x, y) = enemyPosition e
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
                        let newProj = createProjectile tower enemy
                            newTower = tower { towerTimeSinceLastShot = 0 }
                        in acc
                            { projectiles = newProj : projectiles acc
                            , towers = newTower : filter (/= tower) (towers acc)
                            }
                    Nothing -> acc
            | otherwise = 
                acc { towers = tower { towerTimeSinceLastShot = towerTimeSinceLastShot tower + delta } : filter (/= tower) (towers acc) }

findTarget :: Tower -> [Enemy] -> Maybe Enemy
findTarget tower enemiesInRange = 
    let inRange = filter (\e -> distance (towerPosition tower) (enemyPosition e) <= towerRange tower) enemiesInRange
    in case inRange of
        [] -> Nothing
        es -> Just (head es)  -- Simple targeting - first enemy in range

createProjectile :: Tower -> Enemy -> Projectile
createProjectile tower enemy = Projectile
    { projPosition = towerPosition tower
    , projTarget = Just enemy
    , projDamage = towerDamage tower
    , projSpeed = projectileSpeed -- pixels per second
    }

checkGameOver :: GameState -> GameState
checkGameOver gs = if any reachedFinish (enemies gs)
                    then gs { gameOver = True }
                    else gs
    where
        reachedFinish e = 
            let (x, y) = enemyPosition e
                (tx, ty) = last (enemyPath e)
                dx = tx - x
                dy = ty - y
            in sqrt (dx*dx + dy*dy) < 5  -- Close enough to finish