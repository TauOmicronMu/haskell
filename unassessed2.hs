-- Chapter 2 - Exercises
-- -----------
-- 1. Done
-- 2. (2^3)*4
--    (2*3)+(4*5)
--    2+(3*(4^5))
-- 3. 
n = a `div` length xs 
    where
        a = 10
        xs = [1,2,3,4,5]  
-- 4.
last xs = head (reverse xs)

-- 5.
init xs = reverse (tail (reverse xs))
init xs = take (length xs - 1) xs

-- Chapter 3 - Exercises
--------------
-- 1. [Char]
--    (Char, Char, Char)
--    [(Bool, Char)]
--    ([Bool],[Char])
--    [[a] -> [a]]
-- 2. bools = [True]
--    nums = [[1]]
--    add a b c = a + b + c :: Int
--    copy a = (a,a)
--    apply f a = f a
-- 3. [a] -> a
--    (a,b) -> (b,a)
--    a -> b -> (a,b)
--    [a] -> Bool
--    (a -> a) -> a -> a
-- 4. Done
-- 5. It isn't feasible as it would require *all* possible combinations of inputs to be tested to ensure that
--    the mapping is identical. It would only be feasible if there was only a small domain of inputs.

-- Chapter 4 - Exercises
--------------
-- 1.
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = (length xs) `div` 2

-- 2.
third :: [a] -> [a]
third xs = head (tail (tail (tail xs)))
third xs = xs !! 3
third (a:b:c:x:xs) = x

-- 3.
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs
safetail xs | null xs = []
            | otherwise = tail xs

-- safetail [] = []
-- safetail (x:xs) = xs

-- 4. 

-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = False
-- False || True = False
-- False || False = False

-- 5.

-- True && True = True
-- (True && False) && (False && True) = False

-- 6. ??

-- 7. 

-- 8.

-- Chapter 5 - Exercises
------------

-- 1.
-- sum [x*x | x <- [1..100]]

-- 2. 
grid x y = [(x,y) | x <- [0..x], y <- [0..x]]

-- 3. 
