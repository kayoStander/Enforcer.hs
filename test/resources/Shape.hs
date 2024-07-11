module Shape where

import Graphics.Rendering.OpenGL (Color4 (..), GLclampf, Vertex4 (..))
import Utils

data Shape
  = Circle Point Radius Divisions
  | Square Point Side
  | Rect Point Point
  | Line Point Point Float
  | Triangle Point Point Point
  | Quad [Point]
  | Polygon [Point]
  | Polyline [Point] Float
  | Curve [Point]
  deriving (Show)

data Color
  = Red
  | Green
  | Blue
  | White
  | Black
  | RGB GLclampf GLclampf GLclampf
  | RGBA GLclampf GLclampf GLclampf GLclampf
  | Default
  deriving (Show)

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  White == White = True
  Black == Black = True
  RGB {} == RGB {} = True
  RGBA {} == RGBA {} = True
  Default == Default = True
  _ == _ = False

data Transform = Rotate2D Float Point | Translate2D Point Point deriving (Show)

type Picture = [Vertex4 Float]

type Points = [Point]

type Point = (Float, Float)

type Radius = Float

type Side = Float

type Divisions = Int

type Drawable = ([Color4 Float], [Vertex4 Float])

toDrawable :: Color -> Shape -> Drawable
toDrawable clr x = (cs, vs)
  where
    vs = map vertex $ shape x
    color = getColor clr
    cs = map (\u -> color) $ vs

toVertex :: [[Point]] -> Picture
toVertex xs = map vertex $ concat xs

vertex :: Point -> Vertex4 Float
vertex p = (\(k, l) -> Vertex4 k l 0 1) p

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
abbcca _ = error "wrong input or no input"

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
    insertpos _ = error "wrong input or no input"

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
line (x1, y1) (x2, y2) w = map (addVectors (x1, y1)) $ rotate2D' theta $ rect (0.0, -(w / 2)) (lineLength, w / 2)
  where
    (x, y) = normalize $ ((x2 - x1), (y2 - y1))
    theta = signum y * acos x -- angle in rad
    lineLength = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

getColor :: (Real a) => Color -> Color4 a
getColor x
  | x == Red = Color4 1 0 0 1
  | x == Green = Color4 0 1 0 1
  | x == Blue = Color4 0 0 1 1
  | x == White = Color4 1 1 1 1
  | otherwise = Color4 0 0 0 1
