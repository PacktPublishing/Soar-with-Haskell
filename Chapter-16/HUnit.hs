import Test.HUnit

horner :: [Int] -> Int -> Int
horner coeffs x = foldl (\r c -> c + x * r) 0 coeffs

test1 :: Test
test1 = TestCase (assertEqual "for horner [1,0,3] 1" 4 (horner [1,0,3] 1))

test2 :: Test
test2 = TestCase (assertEqual "for horner [1,0,3] 0" 1 (horner [1,0,3] 0))

hornerTests :: Test
hornerTests = TestLabel "horner" (TestList [TestLabel "x=1" test1, TestLabel "x=0" test2])

main :: IO ()
main = runTestTT test1 >>= print

