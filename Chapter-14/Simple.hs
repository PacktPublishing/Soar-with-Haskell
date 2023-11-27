import Data.Char

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (liftM, ap)

data Expr = Lit Int | Plus Expr Expr
  deriving Show

digit :: Parser Char
digit = satisfy isDigit

digitValue :: Parser Int
digitValue = fmap digitToInt digit where
  digitToInt c = ord c - ord '0'

char :: Char -> Parser Char
char c = satisfy (== c)

plus :: Parser Char
plus = char '+'

simpleSumP :: Parser Expr
simpleSumP  =
  do x <- digitValue
     plus
     y <- digitValue
     pure (Plus (Lit x) (Lit y))


space :: Parser Char
space = satisfy isSpace

simpleSumP' :: Parser Expr
simpleSumP'  =
  do x <- digitValue
     space
     plus
     space
     y <- digitValue
     pure (Plus (Lit x) (Lit y))


optional :: Alternative f => f a -> f (Maybe a)
optional p = fmap Just p <|> pure Nothing

simpleSumP3 :: Parser Expr
simpleSumP3  =
  do x <- digitValue
     optional space
     plus
     optional space
     y <- digitValue
     pure (Plus (Lit x) (Lit y))

spaces :: Parser ()
spaces = (space >> spaces) <|> pure ()

many, many1 :: Parser a -> Parser [a]
many  p = many1 p <|> pure []
many1 p = do x <- p
             xs <- many p
             pure (x:xs)

number :: Parser Int
number = fmap digitsToNumber (many1 digitValue) where
  digitsToNumber :: [Int] -> Int
  digitsToNumber = foldl (\x y -> 10 * x + y ) 0

exprP :: Parser Expr 
exprP = do spaces
           x <- literalP
           spaces
           more x <|> pure x
  where literalP :: Parser Expr
        literalP = fmap Lit number
        more :: Expr -> Parser Expr
        more x = do plus
                    y <- exprP
                    pure (Plus x y)

newtype Parser a = P { runP :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure x = P (\s -> Just (x, s))
  (<*>) = ap

instance Monad Parser where
  p >>= f = P (\s -> case runP p s of
                       Nothing -> Nothing
                       Just (x, s') -> runP (f x) s')

instance Alternative Parser where
  empty = P (\s -> Nothing)
  p <|> q = P (\s -> case runP p s of
                       Nothing -> runP q s
                       r       -> r) 

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = 
  P (\s -> case s of
             (c:s') -> if p c then Just (c,s')
                              else Nothing
             []     -> Nothing)

parser :: Parser a -> String -> Maybe a
parser p s = fmap fst (runP p s)

eof :: Parser ()
eof = P (\s -> if null s then Just ((),"") else Nothing)

parse :: Parser a -> String -> Maybe a
parse p s = fmap fst (runP p s)
