import Data.Char

main :: IO ()
main = do l <- getLine
          let ul = map toUpper l
          putStrLn ul
          putStrLn ul

