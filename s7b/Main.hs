module Main (main) where

main :: IO ()
main = do
  cont <- getContents

  let allLines = lines cont

  print $ traverseMatrix allLines [((findStart (head allLines) 0, 0), 1)]

traverseMatrix :: [String] -> [((Int, Int), Int)] -> Int
traverseMatrix _ [] = 0
traverseMatrix matrix (((x, _), _) : xs) | x < 0 || x >= length (head matrix) = traverseMatrix matrix xs
traverseMatrix matrix (((_, y), cnt) : xs) | y + 1 >= length matrix = cnt + traverseMatrix matrix xs
traverseMatrix matrix (((x, y), cnt) : xs) | ((matrix !! y) !! x) /= '^' = traverseMatrix matrix queue
  where
    downPos = (x, y + 1)
    queue = appendOrExtend xs (downPos, cnt)
-- This logic assumes that there are no splitters right next to each other
-- If there were we would need to pass information whether we should skip splitting left/right
-- Also rightPos can be enqueued as duplicate right now but it does not cause any problems
traverseMatrix matrix (((x, y), cnt) : xs) = traverseMatrix matrix (((x - 1, y), cnt) : ((x + 1, y), cnt) : xs)

findStart :: String -> Int -> Int
findStart [] _ = error "Unable to find start"
findStart ('S' : _) curr = curr
findStart (_ : xs) curr = findStart xs (curr + 1)

appendOrExtend :: [((Int, Int), Int)] -> ((Int, Int), Int) -> [((Int, Int), Int)]
appendOrExtend [] e = pure e
appendOrExtend queue e | fst (last queue) == fst e = init queue ++ [(fst e, snd e + snd (last queue))]
appendOrExtend queue e = queue ++ [e]
