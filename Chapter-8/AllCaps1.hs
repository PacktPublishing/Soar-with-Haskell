import Data.Char 
main :: IO ()
main = do l <- getLine
          (let ul = map toUpper l
           in do putStrLn ul
                 putStrLn ul)

