import Lib
import Test.QuickCheck

main :: IO ()
main = do
 putStrLn "GCD tests"
 quickCheckWith stdArgs { maxSuccess = 1000 }  gcd_selfcheck 
 putStrLn "1!"
 quickCheckWith stdArgs { maxSuccess = 1000 }  gcd_one_left_check
 putStrLn "2!"
 quickCheckWith stdArgs { maxSuccess = 1000 }  gcd_one_right_check 
 putStrLn "3!"
 quickCheckWith stdArgs { maxSuccess = 1000 }  gcd_comm_check 
 putStrLn "4!"
 quickCheckWith stdArgs { maxSuccess = 1000 }  gcd_associative_check 
 putStrLn "Готово!"
 putStrLn "mul tests"
 quickCheckWith (stdArgs {maxSuccess = 1000}) $
    forAll (choose (-100, 100)) (\x ->
     forAll (choose (-100, 100)) (\y ->
      forAll nonZero (\m -> mul_mod_check x y m)))
 quickCheckWith (stdArgs {maxSuccess = 1000}) $
   forAll (choose (-100, 100)) (\x ->
    forAll nonZero (\m -> mul_neutral_r_check x m))
 quickCheckWith (stdArgs {maxSuccess = 1000}) $
   forAll (choose (-100, 100)) (\x ->
    forAll (choose (-100, 100)) (\y ->
     forAll nonZero (\m -> mul_comm_check x y m)))
 putStrLn "Готово!"



nonZero :: Gen Int
nonZero = choose (-100, 100) `suchThat` (/= 0)

gcd_selfcheck :: Int -> Bool
gcd_selfcheck a = mGcd a a == a

gcd_one_left_check :: Int -> Bool
gcd_one_left_check a = mGcd 1 a == 1

gcd_one_right_check :: Int -> Bool
gcd_one_right_check a = mGcd a 1 == 1

gcd_comm_check :: Int -> Int -> Bool
gcd_comm_check a b = (mGcd a b) == (mGcd b a)

gcd_associative_check :: Int -> Int -> Int -> Bool
gcd_associative_check a b c = mGcd (mGcd a b) c == mGcd a (mGcd b c)

mul_mod_check :: Int -> Int -> Int -> Bool
mul_mod_check x y m = mod (modMult x y m) m == mod (x*y) m

mul_neutral_r_check :: Int -> Int -> Bool
mul_neutral_r_check x m = mod x m == modMult x 1 m

mul_comm_check :: Int -> Int ->Int-> Bool
mul_comm_check x y m = modMult y x m == modMult x y m
