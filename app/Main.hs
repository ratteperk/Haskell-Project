module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Types
import Rendering
import Input
import Logic
import Config
import System.Random (randomR, mkStdGen, newStdGen)


main :: IO ()
main = do
  rndGen <- newStdGen
  assets <- loadAssets

  let initialGameState = (initialState sampleMap1) {randomGen = rndGen}
  playIO
    (InWindow 
      "Tower Defense" 
      (windowWidth, windowHeight) 
      (100, 100))
    white
    fps
    initialGameState
    (\gs -> return (renderGame gs assets))
    (\event gs -> return (handleInput event gs))
    (\delta gs -> return (updateGame delta gs)) 
    