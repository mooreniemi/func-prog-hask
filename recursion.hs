-- https://wiki.haskell.org/Import
import Prelude hiding (length, filter)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- filter (\x -> x==0) [0,1,2] => [0]
filter :: (a -> Bool) -> [a] -> [a]
filter pred [] = []
filter pred (x:xs)
  | pred x = x : filter pred xs
  | otherwise = filter pred xs
