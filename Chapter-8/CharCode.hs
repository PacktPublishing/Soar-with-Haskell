import Data.Char

getCharCode :: IO Int
getCharCode = 
  putStrLn "Please enter a single character." >>
  getLine                                     >>= \l -> 
  case l of
     [c] -> return (ord c)
     _   -> putStrLn "That is not a single character." >>
            getCharCode
