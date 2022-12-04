module Main where

splitOnce :: String -> Char -> (String, String)
splitOnce [] _ = ([], [])
splitOnce (x:xs) delim
  | x == delim = ([], xs)
  | otherwise  = ret
    where
      (str, rest) = splitOnce xs delim
      ret = (x : str, rest)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (one, two) = (f one, f two)

solve :: [String] -> Int
solve [] = 0
solve (curr:rest) = ret
  where
    (pleft, pright) = splitOnce curr ','
    (l1, l2) = mapPair (\x -> (read :: String -> Int) x) $ splitOnce pleft '-'
    (r1, r2) = mapPair (\x -> (read :: String -> Int) x) $ splitOnce pright '-'
    contains = if (l1 <= r1 && l2 >= r2) || (r1 <= l1 && r2 >= l2) then 1 else 0
    ret = contains + solve rest

solveTwo :: [String] -> Int
solveTwo [] = 0
solveTwo (curr:rest) = ret
  where
    (pleft, pright) = splitOnce curr ','
    (l1, l2) = mapPair (\x -> (read :: String -> Int) x) $ splitOnce pleft '-'
    (r1, r2) = mapPair (\x -> (read :: String -> Int) x) $ splitOnce pright '-'
    contains = if (l1 <= r2 && l2 >= r1) || (r1 <= l2 && r2 >= l1) then 1 else 0
    ret = contains + solveTwo rest

main :: IO ()
main = do
  input <- readFile "inputs/day4.txt"
  
  putStrLn $ "[Part 1] " ++ (show . solve $ lines input)
  putStrLn $ "[Part 2] " ++ (show . solveTwo $ lines input)
