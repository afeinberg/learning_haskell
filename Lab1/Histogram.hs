module Histogram (Histogram,
                  compute,
                  display)
       where

import qualified Data.List as List
import qualified Data.Map as Map

type Histogram a = [(a, Int)]

compute :: (Ord a) => [a] -> Histogram a
compute lst =
  let incOrAdd e m = Map.insertWith (\_ o -> o + 1) e 1 m
      counts = foldr incOrAdd Map.empty lst
      compareCounts (_, a) (_, b) = compare b a
  in List.sortBy compareCounts (Map.toList counts)
   
display :: (Show a) => Histogram a -> Int -> String
display hst maxCols =   
  let maxCount = snd (List.head hst)
      showNames = map (show . fst) hst
      maxLen = maximum (map length showNames)
      byRow acc (i, c) = 
        case (showBars (maxCols - maxLen - 1) maxCount c) of
          Nothing -> acc
          Just bars -> acc ++ (pad (show i) maxLen) ++ bars ++ "\n"          
  in foldl byRow "" hst
  
pad :: String -> Int -> String     
pad str len = 
  let spaces = len - (length str) + 1
  in str ++ (replicate spaces ' ')
    
showBars :: Int -> Int -> Int -> Maybe String
showBars cols maxCount count = 
  let wordsPerBar = (fromIntegral cols) / (fromIntegral maxCount)
      numBars = round $ wordsPerBar * (fromIntegral count)
  in if (numBars > 0) then Just (replicate numBars '*')
     else Nothing
          
  
