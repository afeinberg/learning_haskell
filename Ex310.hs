module Main where

import IO
import Random
import Control.Monad

askForNums :: IO [Integer]
askForNums = do
  putStrLn "Enter a number: "
  numStr <- getLine
  let num = read numStr :: Integer
  if num == 0
    then return []
    else do 
      rest <- askForNums
      return (num : rest)
                    
factorial :: (Ord a, Num a) => a -> a         
factorial n = loop 1 n 
  where loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
                    | otherwise = acc                                  

showFactorial :: (Ord a, Num a) => a -> [Char]
showFactorial n = (show n) ++ "! is " ++ show (factorial n)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  nums <- askForNums 
  forM nums (putStrLn . showFactorial)
  putStrLn $ "Sum is " ++ show (sum nums)
  putStrLn $ "Product is " ++ show (foldl (*) 1 nums)
 