main :: IO ()
main = do contents <- readFile "input.txt"
          let (l:ls) = lines contents
          let n  = read l
          let xs = map read (take n ls)
          writeFile "output.txt" (show (sum xs :: Int))

