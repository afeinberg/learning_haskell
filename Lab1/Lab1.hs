module Main
       where

import IO
import System.Environment

import qualified Char
import qualified Histogram
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

contentsToList :: T.Text -> [T.Text]
contentsToList contents = 
  let discardFirst str | T.null str = str
                       | Char.isAlpha (T.head str) = str
                       | otherwise = T.tail str
      discardLast str | Char.isAlpha (T.last str) = str
                      | otherwise = T.init str          
      process = T.toLower . discardFirst . discardLast
      lst = process `map` T.words contents
  in filter (not . T.null) lst

main :: IO ()
main = do
  (filename:_) <- getArgs
  contents <- TIO.readFile filename
  let hst = Histogram.compute (contentsToList contents)
  putStrLn (Histogram.display hst 80)
  