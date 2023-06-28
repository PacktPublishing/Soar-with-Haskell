main :: IO ()
main = putStrLn "What is your name?"     >>= (\_    -> 
       getLine                           >>= (\name -> 
       putStrLn ("Hello, " ++ name ++ "!")))
