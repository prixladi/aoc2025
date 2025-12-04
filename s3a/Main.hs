module Main (main) where

main :: IO ()
main = do
  cont <- getContents
  print $ foldr ((+) . findHighestJoltage (0, 0) . fmap (read . pure)) 0 (lines cont)

findHighestJoltage :: (Int, Int) -> [Int] -> Int
findHighestJoltage (f, s) [] = 10 * f + s
findHighestJoltage (f, s) [x] = findHighestJoltage (f, max x s) []
findHighestJoltage (f, _) (x : xs) | x > f = findHighestJoltage (x, 0) xs
findHighestJoltage (f, s) (x : xs) | x > s = findHighestJoltage (f, x) xs
findHighestJoltage acc (_ : xs) = findHighestJoltage acc xs
