module Utils where

type Matrix2D = (Float, Float, Float, Float)

toRadians :: Float -> Float
toRadians x = x * pi / 180

fromRadians :: Float -> Float
fromRadians x = x / pi * 180

rotate2D' :: Float -> [(Float, Float)] -> [(Float, Float)]
rotate2D' a = map (rotate2D a)

rotate2D :: Float -> (Float, Float) -> (Float, Float)
rotate2D theta (x, y) = (x', y')
  where
    x' = x * cos theta - y * sin theta
    y' = x * sin theta + y * cos theta

normalize :: (Float, Float) -> (Float, Float)
normalize v@(x, y) = (x * len', y * len')
  where
    len' = 1.0 / len v

len :: (Float, Float) -> Float
len (x, y) = sqrt (x * x + y * y)

mulMatrVect :: Matrix2D -> (Float, Float) -> (Float, Float)
mulMatrVect (x1, x2, y1, y2) (x, y) = ((x1 + x2) * x, (y1 + y2) * y)

addVectors :: (Float, Float) -> (Float, Float) -> (Float, Float)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pairs :: [t] -> [(t, t)]
pairs [] = []
pairs [_] = error "non-even list for pair func"
pairs (x : y : xs) = (x, y) : pairs xs

fromPairs :: [(a, a)] -> [a]
fromPairs [] = []
fromPairs ((x, y) : xs) = x : y : fromPairs xs
