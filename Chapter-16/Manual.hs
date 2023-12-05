horner1 :: [Int] -> Int -> Int
horner1 coeffs x = foldl (\r c -> c + x * r) 0 coeffs

horner2 :: [Int] -> Int -> Int
horner2 coeffs x = foldr (\r c -> c + x * r) 0 coeffs

horner3 :: [Int] -> Int -> Int
horner3 coeffs x = foldr (\c r -> c + x * r) 0 coeffs

