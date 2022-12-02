module Main where

import System.IO
import Data.Char (ord)

calcPts :: (Int, Int) -> Int
calcPts (left, right) = outcome + right + 1
  where
    outcome
      | left == right = 3
      | (left + 1) `mod` 3 == right = 6
      | otherwise = 0

solve :: [String] -> Int
solve [] = 0
solve lines = ret
  where
    curr_line = head lines
    left  = curr_line !! 0 
    right = curr_line !! 2 
    ret = calcPts(ord(left) - ord('A'), ord(right) - ord('X')) + solve(tail lines)


solveTwo :: [String] -> Int
solveTwo [] = 0
solveTwo lines = ret
  where
    curr_line = head lines
    right    = curr_line !! 2
    ordleft  = ord(curr_line !! 0) - ord('A')
    pts = case right of
      'X' -> 0 + (ordleft + 2) `mod` 3 + 1
      'Y' -> 3 + ordleft + 1
      'Z' -> 6 + (ordleft + 1) `mod` 3 + 1

    ret = pts + (solveTwo $ tail lines)

main :: IO ()
main = do
  input <- readFile "inputs/day2.txt"

  putStrLn $ "[Part 1] " ++ (show $ solve $ lines input)
  putStrLn $ "[Part 2] " ++ (show $ solveTwo $ lines input)
