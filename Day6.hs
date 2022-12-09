module Main where

import System.IO

solve :: String -> Int -> Int
solve input n
  | length input < n = error "invalid input"
  | noDup = n
  | otherwise = 1 + solve(tail input) n
  where
    cs = take n input
    noDup = not $ any (/= (n-1)) $ map (\x -> length (filter (\y -> x /= y) cs)) cs

main :: IO ()
main = do
  input <- readFile "inputs/day6.txt"

  putStrLn $ "[Part 1] " ++ (show $ solve input 4)
  putStrLn $ "[Part 2] " ++ (show $ solve input 14)
