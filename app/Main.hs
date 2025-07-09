module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Types
import GameState
import Rendering
import Input
import Logic
import Config

n, r, b, f :: TileType
n = Neutral 
r = Road 
b = Buildable 
f = Finish
s = Start

-- Sample map (0 = Neutral, 1 = Road, 2 = Buildable, 3 = Finish)
sampleMap :: [[TileType]]
sampleMap =
    [[n, n, n, n, n, n, n, n, s, n],
     [n, n, n, n, n, n, n, b, r, n],
     [n, n, n, n, n, n, b, b, r, n], 
     [n, n, n, n, n, b, b, b, r, n], 
     [n, n, n, n, n, b, b, b, r, n], 
     [n, b, b, r, r, r, r, r, r, n], 
     [n, b, b, r, b, b, b, b, n, n], 
     [n, r, r, r, b, b, b, b, n, n], 
     [n, r, n, n, n, n, n, n, n, n], 
     [n, f, n, n, n, n, n, n, n, n]]

main :: IO ()
main = do
    let initialGameState = initialState sampleMap 
    playIO
        (InWindow 
            "Tower Defense" 
            (windowWidth, windowHeight) 
            (100, 100))
        white
        60
        initialGameState
        (\gs -> return $ renderGame gs)
        (\event gs -> return $ handleInput event gs)
        (\delta gs -> return $ updateGame delta gs) 