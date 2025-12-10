module Main (main) where

type Point = (Int, Int)

main :: IO ()
main = do
  cont <- getContents

  let coords = (\x -> (read (head x), read (x !! 1))) . splitOn ',' <$> lines cont

  print $ solve coords

solve :: [Point] -> Int
-- this computes areas for diagonals for length 1 (same point)
-- but they get filtered out by maximum and it fast enough
-- proper solution would require manual folding so i cba
solve points = maximum $ (\t -> getRectangleArea t <$> points) =<< points
  where
    getRectangleArea (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []
