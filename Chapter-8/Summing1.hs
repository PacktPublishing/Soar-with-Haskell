import Control.Monad 

readInteger :: IO Int
readInteger = do l <- getLine
                 return (read l)

readSum :: Int -> IO Int
readSum 0 = return 0
readSum n = do x <- readLn
               s <- readSum (n-1)
               return (x + s)

readSum2 :: Int -> IO Int
readSum2 0 = return 0
readSum2 n = do x <- readLn
                s <- readSum2 (n-1)
                return (x + s)

readSum3 :: Int -> IO Int
readSum3 n = do xs <- replicateM n readLn
                return (sum xs)

putInt :: Int -> IO ()
putInt n = putStrLn (show n)

main :: IO ()
main = do n <- readLn
          xs <- replicateM n readLn
          print (sum xs :: Int)


