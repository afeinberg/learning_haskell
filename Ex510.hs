haskellIsFun = "Haskell is fun"
debuggingIsFun = "Debugging Haskell is fun"
whoAreYou = "I don't know who you are"

commentOnName0 :: String -> String
commentOnName0 inp =
  if (inp == "Simon" || inp == "John" || inp == "Phil") then
    haskellIsFun
  else 
    if (inp == "Koen") then
      debuggingIsFun
    else
      whoAreYou
      
commentOnName1 :: String -> String
commentOnName1 inp =
  case inp of
    "Simon" -> haskellIsFun
    "John" -> haskellIsFun
    "Phil" -> haskellIsFun
    "Koen" -> debuggingIsFun
    _ -> whoAreYou

doAsk :: IO ()
doAsk = do
  putStrLn "Enter your name"
  name <- getLine
  putStrLn $ commentOnName1 name
  