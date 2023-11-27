import Text.Parsec
import Data.Char (ord, isDigit)

type Parser = Parsec String ()

data Value = Number Int | Bool Bool
  deriving Show

boolP :: Parser Value
boolP = fmap Bool (trueP <|> falseP) where
  trueP = pure True <* word "true"
  falseP = pure False <* word "false"
  word = traverse char 

numberP :: Parser Value
numberP = (fmap (Number . digitsToNumber) (many1 digitValue)) where
  digitsToNumber :: [Int] -> Int
  digitsToNumber = foldl (\x y -> 10 * x + y ) 0

digitValue :: Parser Int
digitValue = fmap digitToInt digit where
  digitToInt c = ord c - ord '0'
  digit = satisfy isDigit
