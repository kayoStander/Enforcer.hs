module Shape where

import Graphics.Rendering.OpenGL (Vertex2 (..))
import Utils

data Shape = Circle Point Radius Divisions | Square Point Side | Rect Point Point | Line Point Point Float | Triangle Point Point Point | Quad [Point] | Polygon [Point] | Polyline [Point] Float | Curve [Point] deriving (Show)

data Transform = Rotate2D Float Point | Translate2D Point Point deriving (Show)

type Picture = [Vertex2 Float]

type Point = (Float, Float)

type Radius = Float

type Side = Float

type Divisions = Int

toVertex :: [[Point]] -> Picture
toVertex xs = map vertex $ concat xs

vertex :: Point -> Vertex2 Float
vertex p = uncurry Vertex2 p

shape :: Shape -> [Point]
shape (Square pos side) = square pos side
shape (Circle pos rad divs) = circle pos rad divs
shape (Rect bl tr) = rect bl tr
shape (Line p1 p2 w) = line p1 p2 w
shape (Polyline ps w) = polyline ps w
shape (Triangle p1 p2 p3) = triangle p1 p2 p3
shape _ = error "WIP (curve,quad,polygon) or not acceptable data"

polyline :: [Point] -> Float -> [Point]
polyline ps w = concatMap (\(x, y) -> line x y w) $ pairs $ abbcca ps

triangle :: Point -> Point -> Point -> [Point]
triangle p1 p2 p3 = [p1, p2, p3]

square :: Point -> Float -> [Point]
square pos side = [p1, p2, p3, p1, p3, p4]
  where
    x = fst pos
    y = snd pos
    r = side / 2
    p1 = (x + r, y + r)
    p2 = (x - r, y + r)
    p3 = (x - r, y - r)
    p4 = (x + r, y - r)

abbcca :: [a] -> [a]
abbcca (x : xs) = [x] ++ (concat $ map (\(x1, y) -> [x1, y]) $ map (\x1 -> (x1, x1)) (init xs)) ++ [last xs]
abbcca _ = error "ERROR"

circle :: Point -> Float -> Int -> [Point]
circle pos r divs =
  let x = fst pos
      y = snd pos
      divs' = fromIntegral divs
      sines = map ((x +) . (r *) . sin) [0.0, 2 * pi / divs' .. 2 * pi]
      cosines = map ((x +) . (r *) . cos) [0.0, 2 * pi / divs' .. 2 * pi]
   in concat $ insertpos $ abbcca $ zip sines cosines
  where
    insertpos (x : y : []) = [[pos, x, y]]
    insertpos (x : y : xs) = [pos, x, y] : insertpos xs

rect :: Point -> Point -> [Point]
rect (x1, y1) (x2, y2) =
  [ (x2, y2),
    (x1, y2),
    (x1, y1),
    (x2, y2),
    (x1, y1),
    (x2, y1)
  ]

line :: Point -> Point -> Float -> [Point]
line (x1, y1) (x2, y2) w = map (addVectors (x1, y1)) $ rotate2D' theta $ rect (0.0, -w / 2) (len, w / 2)
  where
    (x, y) = normalize $ ((x2 - x1), (y2 - y1))
    theta = signum y * acos x -- angle in rad
    len = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
