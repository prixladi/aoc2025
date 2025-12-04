module Main (main) where

main :: IO ()
main = do
  cont <- getContents
  print $ countAccessibleTiles (lines cont) (0, 0)

countAccessibleTiles :: [[Char]] -> (Int, Int) -> Int
countAccessibleTiles board (x, y) | x < 0 = countAccessibleTiles board (0, y)
countAccessibleTiles board (x, y) | y < 0 = countAccessibleTiles board (x, 0)
countAccessibleTiles board (_, y) | y >= length board = 0
countAccessibleTiles board (x, y) | x >= length (head board) = countAccessibleTiles board (0, y + 1)
countAccessibleTiles board pos@(x, y) | getTileValue board pos == 0 = countAccessibleTiles board (x + 1, y)
-- TODO: This is just a semi-naive solution it moves the cursor to x - 1 y - 1 position ech time
-- we replace a tile, because it is its further most neighbor,
-- but is is not optimal because we will need to recheck tiles that should not be rechecked
countAccessibleTiles board pos@(x, y)
  | hit = 1 + countAccessibleTiles (removeOnPosition pos board) (x - 1, y - 1)
  | otherwise = 0 + countAccessibleTiles board (x + 1, y)
  where
    hit = foldr ((+) . getTileValue board) 0 (getNeighbors pos) < 4

getTileValue :: [[Char]] -> (Int, Int) -> Int
getTileValue board (x, y) | x < 0 || x >= length (head board) || y < 0 || y >= length board = 0
getTileValue board (x, y) = tileToValue $ (board !! y) !! x

tileToValue :: Char -> Int
tileToValue '.' = 0
tileToValue '@' = 1
tileToValue tile = error $ "Invalid tile: " ++ pure tile

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) =
  concatMap
    ( filter (\(xN, yN) -> xN /= x || yN /= y)
        . (\c -> fmap c [-1, 0, 1])
        . (\xD yD -> (x + xD, y + yD))
    )
    [-1, 0, 1]

removeOnPosition :: (Int, Int) -> [[Char]] -> [[Char]]
removeOnPosition (x, y) board = replace y (replace x '.' (board !! y)) board

replace :: Int -> a -> [a] -> [a]
replace x v a = take x a ++ v : drop (x + 1) a