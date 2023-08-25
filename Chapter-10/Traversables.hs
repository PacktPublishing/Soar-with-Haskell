import Data.Char (isSpace)
import Data.Monoid (Sum (..))
import Control.Monad.State
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Product

haiku :: String
haiku = unlines ["I write, erase, rewrite"
                ,"Erase again, and then"
                ,"A poppy blooms."]


-- * Character count

type Count = Const (Sum Integer)

getCount :: Count a -> Integer
getCount = getSum . getConst

count :: Integer -> a -> Count b
count n _ = Const (Sum n)

charCount :: String -> Integer
charCount = getCount . traverse (count 1)

-- * Line count

test :: Bool -> Count b
test b = if b then count 1 () else count 0 ()

lineCount :: String -> Integer
lineCount = getCount . traverse (\c -> test (c == '\n'))

-- * Word count

type WordCount = Compose (State Bool) Count

getWordCount :: WordCount a -> Integer
getWordCount p =  getCount (fst (runState (getCompose p) False))

newWord :: Char -> WordCount b
newWord c = let s' = not (isSpace c)
            in Compose (state (\s -> (test (not s && s'), s')))

wordCount :: String -> Integer
wordCount = getWordCount . traverse newWord

-- * All together now

stats :: String -> (Integer, Integer, Integer)
stats = unpack . traverse f where
  f :: Char -> Product Count (Product Count WordCount) ()
  f c = Pair (count 1 ()) (Pair (test (c == '\n')) (newWord c))

  unpack :: Product Count (Product Count WordCount) a -> (Integer, Integer, Integer)
  unpack (Pair cc (Pair lc wc)) = (getCount cc, getCount lc, getWordCount wc)
  
