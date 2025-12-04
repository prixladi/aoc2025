module Main (main) where

joltageNum :: Int
joltageNum = 12

main :: IO ()
main = do
  cont <- getContents
  print $ foldr ((+) . findHighestJoltage [] . fmap (read . pure)) 0 (lines cont)

findHighestJoltage :: [Int] -> [Int] -> Int
findHighestJoltage acc _ | length acc == joltageNum = read (concatMap show acc)
findHighestJoltage acc str = findHighestJoltage (acc ++ [highest]) (drop (pos + 1) str)
  where
    availableStr = take (length str - (joltageNum - length acc - 1)) str
    (highest, pos) = findHighestWithPos (0, 0) 0 availableStr

findHighestWithPos :: (Int, Int) -> Int -> [Int] -> (Int, Int)
findHighestWithPos state _ [] = state
findHighestWithPos (h, _) currPos (x : xs) | x > h = findHighestWithPos (x, currPos) (currPos + 1) xs
findHighestWithPos state currPos (_ : xs) = findHighestWithPos state (currPos + 1) xs