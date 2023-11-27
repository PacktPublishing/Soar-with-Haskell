import Text.Parsec
import Data.Char (ord)

data Expr = Lit Int | Plus Expr Expr | Times Expr Expr
  deriving Show

type Parser = Parsec String ()

plus :: Parser Char
plus = char '+'

times :: Parser Char
times = char '*'

digitValue :: Parser Int
digitValue = fmap digitToInt digit where
  digitToInt c = ord c - ord '0'

number :: Parser Int
number = fmap digitsToNumber (many1 digitValue) where
  digitsToNumber :: [Int] -> Int
  digitsToNumber = foldl (\x y -> 10 * x + y ) 0

exprP0 = literalP <|> sumP where
  literalP = Lit <$> number
  sumP = do x <- exprP0 
            plus
            y <- exprP0
            pure (Plus x y)

exprP1 = try sumP <|> literalP where
  literalP = Lit <$> number
  sumP = do x <- exprP1 
            plus
            y <- exprP1
            pure (Plus x y)

exprP2 = 
  do x <- literalP
     f <- rest
     pure (f x) where
  literalP = Lit <$> number
  rest = (do plus
             y <- exprP2
             pure (`Plus` y)
         ) <|> (pure id)

parens = between (char '(') (char ')')

exprP3 = 
  do x <- termP
     f <- exprR
     pure (f x) where
  termP = do x <- factorP
             f <- termR
             pure (f x)
  factorP = literalP <|> parens exprP3
  literalP = Lit <$> number
  termR = (do times
              y <- termP
              pure (`Times` y)
          ) <|> (pure id)
  exprR = (do plus
              y <- exprP3
              pure (`Plus` y)
         ) <|> (pure id)

exprP4  = termP `chainl1` plusP where
  plusP  = pure Plus  <* plus
  timesP = pure Times <* times
  termP    = factorP `chainl1` timesP
  factorP  = literalP <|> parens exprP3
  literalP = Lit <$> number

literalP = Lit <$> number <?> "literal" 

exprP5 = exprP5' id

exprP5' f  = 
  do x <- literalP
     rest (f x) where
  literalP = Lit <$> number
  rest x = (do plus
               exprP5' (Plus x)
           ) <|> (pure x)

exprP6  = literalP `chainl1` opP where
  opP = plusP <|> timesP
  plusP  = pure Plus  <* plus
  timesP = pure Times <* times
