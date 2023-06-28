import System.Random
import Data.Time

main :: IO ()
main = 
  do n <- randomRIO (1,100)
     putStr "Guess my number: "
     start <- getCurrentTime
     guess n
     end   <- getCurrentTime 
     putStrLn "Congratulations, you have guessed my number."
     putStrLn ("It took you " ++ show (diffUTCTime end start))

guess :: Int -> IO ()
guess n = do m <- readLn
             case compare n m of
               LT -> do putStr "Guess again, but lower: "
                        guess n 
               GT -> do putStr "Guess again, but higher: "
                        guess n 
               EQ -> return ()
     
