module Main where

import Rendering
import Shape

main :: IO ()
main = do
  let prims = [shape $ Triangle (0.0, 1.0) (-1.0, -1.0) (1.0, -1.0)]
  win <- createWindow "Window" (512, 512)
  drawInWindow win prims
  closeWindow win
