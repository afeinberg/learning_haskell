module Main
       where

import IO

main = do
  hSetBuffering stdin LineBuffering
  doLoop
  
doLoop = do
  putStrLn "Do you want to [read] a file, [write] a file or [quit]?"
  command <- getLine
  case command of
    'q':_ -> return ()
    'r':_ -> do putStrLn "Enter file name to read:"
                filename <- getLine
                doRead filename
                doLoop
    'w':_ -> do putStrLn "Enter a filename to write:"
                filename <- getLine
                doWrite filename
                doLoop

doRead filename =
  bracket (openFile filename ReadMode) hClose
  (\h -> do contents <- hGetContents h
            putStrLn contents)

doWrite filename = do
  let loop h = do
        text <- getLine
        case text of 
          "." -> return ()
          _ -> do hPutStrLn h text
                  loop h
  bracket (openFile filename WriteMode) hClose loop

    