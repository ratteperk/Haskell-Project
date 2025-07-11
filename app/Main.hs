module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Types
import GameState
import Rendering
import Input
import Logic
import Config
import System.Random (randomR, mkStdGen, newStdGen)


main :: IO ()
main = do
  rndGen <- newStdGen
  let initialGameState = (initialState sampleMap) {randomGen = rndGen}
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
    