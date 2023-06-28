main :: IO ()
main = putStrLn "What is your name?"     >>  
       getLine                           >>= \name -> 
       putStrLn ("Hello, " ++ name ++ "!")
