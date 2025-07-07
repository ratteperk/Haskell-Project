module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Определим простой тип для состояния игры
type World = (Float, Bool)  -- (время, пауза)

main :: IO ()
main = play
  (InWindow "Tower Defense" (800, 600) (100, 100))  -- Окно 800x600
  white                                              -- Фон
  30                                                 -- FPS
  initialWorld                                       -- Начальное состояние
  renderWorld                                        -- Функция отрисовки
  handleInput                                        -- Обработка ввода
  updateWorld                                        -- Логика обновления

-- Начальное состояние: время 0, не на паузе
initialWorld :: World
initialWorld = (0, False)

-- Отрисовка: просто выводим текст с текущим временем
renderWorld :: World -> Picture
renderWorld (time, paused) = 
  pictures [
    color black (text $ "Time: " ++ show time),
    if paused 
      then color red (translate (-100) 50 (text "PAUSED")) 
      else blank
  ]

-- Обработка ввода: пробел ставит на паузу
handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (t, paused) = (t, not paused)
handleInput _ world = world

-- Обновление: увеличиваем время, если не на паузе
updateWorld :: Float -> World -> World
updateWorld dt (time, False) = (time + dt, False)
updateWorld _ world = world