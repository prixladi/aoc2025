module Main (main) where

main :: IO ()
main = do
  cont <- getContents
  print $ countAccessibleTiles (lines cont) (0, 0)

countAccessibleTiles :: [[Char]] -> (Int, Int) -> Int
countAccessibleTiles board (_, y) | y >= length board = 0
countAccessibleTiles board (x, y) | x >= length (head board) = countAccessibleTiles board (0, y + 1)
countAccessibleTiles board pos@(x, y) | getTileValue board pos == 0 = countAccessibleTiles board (x + 1, y)
countAccessibleTiles board pos@(x, y) = (if count < 4 then 1 else 0) + countAccessibleTiles board (x + 1, y)
  where
    count = foldr ((+) . getTileValue board) 0 (getNeighbors pos)

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