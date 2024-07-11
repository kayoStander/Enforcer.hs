module Main where

import Rendering
import Shape

main :: IO ()
main = do
  let drawables =
        [ toDrawable Red $ Square (-0.5, -0.5) 1.0,
          toDrawable Green $ Circle (0.5, 0.5) 0.5 100,
          toDrawable Blue $ Rect (-1.0, 0.33) (0.0, 0.66),
          toDrawable White $
            Polyline
              [ (0.0, -0.66),
                (0.33, -0.33),
                (0.66, -0.66),
                (1.0, -0.33)
              ]
              0.01
        ]

  let objs =
        [ toDrawable White $ Circle (0.0, 0.0) 0.5 15,
          toDrawable White $ Polyline [(-1.0, 0.0), (1.0, 0.0)] 0.01
        ]

  window <- createWindow "Enforcer basis" (512, 512)
  drawIn Default window objs
  closeWindow window
