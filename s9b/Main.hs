module Main (main) where

import Data.List (sortBy)

type Point = (Int, Int)

type Line = (Point, Point)

main :: IO ()
main = do
  cont <- getContents

  let coords = (\x -> (read (head x), read (x !! 1))) . splitOn ',' <$> lines cont

  print $ solve coords

solve :: [Point] -> Int
solve points = getFirstContained sortedAreas points
  where
    getRectangleArea (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
    sortedAreas =
      sortBy
        (\(_, a1) (_, a2) -> compare a2 a1)
        $ (\(p, skip) -> (\x -> ((x, p), getRectangleArea p x)) <$> drop skip points) =<< numbered points

getFirstContained :: [(Line, Int)] -> [Point] -> Int
getFirstContained [] _ = error "Unable to find fitting rectangle"
getFirstContained ((line, area) : xs) points
  | polygonIsContained line points = area
  | otherwise = getFirstContained xs points

polygonIsContained :: Line -> [Point] -> Bool
polygonIsContained ((x1, y1), (x2, y2)) points = not $ linesCross rectangle (points ++ [head points])
  where
    topLeft = (min x1 x2 + 1, min y1 y2 + 1)
    topRight = (max x1 x2 - 1, min y1 y2 + 1)
    bottomRight = (max x1 x2 - 1, max y1 y2 - 1)
    bottomLeft = (min x1 x2 + 1, max y1 y2 - 1)
    rectangle = [(topLeft, topRight), (topRight, bottomRight), (bottomRight, bottomLeft), (bottomLeft, topLeft)]

linesCross :: [Line] -> [Point] -> Bool
linesCross _ [] = False
linesCross _ [_] = False
linesCross l (a : b : xs)
  | any (linesIntersect (a, b)) l = True
  | otherwise = linesCross l (b : xs)

linesIntersect :: Line -> Line -> Bool
linesIntersect (a, b) (c, d) = f a c d /= f b c d && f a b c /= f a b d
  where
    f o p q = ((snd q - snd o) * (fst p - fst o)) > ((snd p - snd o) * (fst q - fst o))

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []

numbered :: [a] -> [(a, Int)]
numbered = f 1
  where
    f _ [] = []
    f pos (x : xs) = (x, pos) : f (pos + 1) xs