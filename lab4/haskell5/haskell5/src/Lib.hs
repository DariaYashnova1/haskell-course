module Lib
    ( mGcd,
      modMult
    ) where

mGcd :: Int -> Int -> Int
mGcd 0 0 = 0  -- Неправильный результат
mGcd a b = gcd a b  -- Правильная реализация для остальных случаев

modMult:: Int->Int->Int->Int
modMult a b c = mod (a * b) c