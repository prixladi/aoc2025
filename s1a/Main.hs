module Main (main) where

main :: IO ()
main = do
  cont <- getContents
  print $ processInstructions $ lines cont

processInstructions :: [String] -> Int
processInstructions = fst . foldr (processInstruction . parseInstruction) (0, 50)

processInstruction :: Int -> (Int, Int) -> (Int, Int)
processInstruction 0 acc = acc
processInstruction ins (count, pos) = (count + overflow, newPos)
  where
    newPos = mod (pos + ins) 100
    overflow = if newPos == 0 then 1 else 0

parseInstruction :: String -> Int
parseInstruction ('L' : rest) = -(1 * read rest)
parseInstruction ('R' : rest) = read rest
parseInstruction ins = error ("Invalid instruction " ++ ins)
