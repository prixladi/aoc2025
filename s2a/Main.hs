module Main (main) where

main :: IO ()
main = do
  cont <- getContents
  print $ foldr ((+) . sumInvalidCodes) 0 (splitOn ',' cont)

sumInvalidCodes :: String -> Int
sumInvalidCodes range = foldr ((+) . sumInvalidCode) 0 [start .. end]
  where
    rangeTuple = splitOn '-' range
    start = read $ head rangeTuple
    end = read $ rangeTuple !! 1

sumInvalidCode :: Int -> Int
sumInvalidCode code | isInvalidCode (show code) = code
sumInvalidCode _ = 0

isInvalidCode :: String -> Bool
isInvalidCode code | odd ((length . show) code) = False
isInvalidCode code = take len code == drop len code
  where
    len = div (length code) 2

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ [] = []