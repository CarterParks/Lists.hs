module Lists where
  import Data.List

  countingNumbers :: [Integer] {- get every natural number -}
  countingNumbers = [1..]

  multiplesOfNumbers :: Integer -> [Integer] {- get every multiple of n -}
  multiplesOfNumbers n = filter (\x -> mod x n == 0) [1..]

  woodallNumbers :: [Integer] {- get all woodall numbers -}
  woodallNumbers = map (\x -> (x * (2 ^ x)) - 1) [1..]

  padovanNumbers :: [Integer]
  padovanNumbers = map pN [0..]
    where
    pN :: Integer -> Integer {- get the nth padovan number -}
    pN n | n <= 2 = 1
         | otherwise = (pN (n - 2)) + (pN (n - 3))

  order :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [a] {- merge lists in order based on function -}
  order _ [] y = y
  order _ x [] = x
  order f (x:xs) (y:ys) | (f x y) = x : order f xs (y:ys)
                        | otherwise = y : order f (x:xs) ys

  pairUp :: [Integer] -> [[Integer]] {- build list joining every other number -}
  pairUp xs | odd (length xs) = (pairUp (init xs)) ++ [[last xs]]
            | otherwise = map (\(x,y) -> [x,y]) (zip (sep odd) (sep even))
    where
    sep :: (Integer -> Bool) -> [Integer] {- separate into odd/even -}
    sep f = map (\(x,_) -> x) (filter (\(_,n) -> f n) (zip xs [1..]))

  runLengthEncoding :: Eq a => [a] -> [(a,Int)] {- compose length counter and Data.List group funcs -}
  runLengthEncoding = map (\x -> (head x, length x)) . group

  listPairApply :: Num t => [t -> t -> t] -> [[t]] -> [t] {- go through number pairs applying corresponding func -}
  listPairApply _ [] = []
  listPairApply fs xs | length (last xs) == 2 = map (\(f,[x,y]) -> f x y) (zip (cycle fs) xs)
                      | otherwise = (listPairApply fs (init xs)) ++ last xs

  composeList :: [(Int -> Int)] -> (Int -> Int) {- compose list of funcs together -}
  composeList xs | (length xs) > 1 = (head xs) . composeList (tail xs)
                 | otherwise = head xs

{-
Resources used:
https://stackoverflow.com/questions/15211629/merge-finite-sorted-list-in-haskell#15211764 
https://stackoverflow.com/questions/43772891/haskell-run-length-encoding-function
-}
