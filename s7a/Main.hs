module Main (main) where

main :: IO ()
main = do
  cont <- getContents

  let allLines = lines cont

  print $ traverseMatrix allLines [(findStart (head allLines) 0, 0)]

traverseMatrix :: [String] -> [(Int, Int)] -> Int
traverseMatrix _ [] = 0
traverseMatrix matrix ((x, _) : xs) | x < 0 || x >= length (head matrix) = traverseMatrix matrix xs
traverseMatrix matrix ((_, y) : xs) | y + 1 >= length matrix = traverseMatrix matrix xs
traverseMatrix matrix ((x, y) : xs) | ((matrix !! y) !! x) /= '^' = traverseMatrix matrix queue
  where
    downPos = (x, y + 1)
    queue = if isLastInQueue xs downPos then xs else xs ++ [downPos]
-- This logic assumes that there are no splitters right next to each other
-- If there were we would need to pass information whether we should skip splitting left/right
-- Also rightPos can be enqueued as duplicate right now but it does not cause any problems
traverseMatrix matrix ((x, y) : xs) = 1 + traverseMatrix matrix ((x - 1, y) : (x + 1, y) : xs)

findStart :: String -> Int -> Int
findStart [] _ = error "Unable to find start"
findStart ('S' : _) curr = curr
findStart (_ : xs) curr = findStart xs (curr + 1)

isLastInQueue :: [(Int, Int)] -> (Int, Int) -> Bool
isLastInQueue [] _ = False
isLastInQueue queue pos = last queue == pos