{-
askName1 :: IO String
askName1 = do putStrLn "What is your name?"
              name <- getLine
-}
  
askName2 :: IO String
askName2 = do putStrLn "What is your name?"
              name <- getLine
              return name

askName3 :: IO String
askName3 = do putStrLn "What is your name?"
              getLine

{-
askName4 :: IO String
askName4 = do putStrLn "What is your name?"
               getLine
-}


askName5 :: IO String
askName5 = do putStrLn "What is your name?"
             getLine
