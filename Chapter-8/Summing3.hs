import Control.Monad
import System.IO

main :: IO ()
main = do xs <- withFile "input.txt" ReadMode (\hIn ->
              do n <- hReadLn hIn
                 replicateM n (hReadLn hIn)
            )
          withFile "output.txt" WriteMode (\hOut ->
            hPrint hOut (sum xs :: Int)
            )

hReadLn :: Read a => Handle -> IO a
hReadLn h = do l <- hGetLine h
               return (read l)
