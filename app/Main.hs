module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Types
import GameState
import Rendering
import Input
import Logic
import Config

-- Sample map (0 = Neutral, 1 = Road, 2 = Buildable, 3 = Finish)
sampleMap :: [[TileType]]
sampleMap =
    [ [Buildable, Road, Road,   Neutral, Neutral, Neutral]
    , [Buildable, Road,   Neutral, Neutral, Neutral]
    , [Neutral, Road,   Road,    Road,    Road]
    , [Neutral, Neutral,Neutral, Road,    Neutral]
    , [Neutral, Neutral,Neutral, Road,    Neutral, Finish]
    ]

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