f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
  where ys = [a | a <- xs, a <= x]
        zs = [a | a <- xs, a >  x]

deleteone a []   = []
deleteone a (x:xs)
    | x == a    = xs
    | otherwise = [x] ++ (deleteone a xs)   

permutations []  = [[]]
permutations xs  = [x:ys | x <- xs, ys <- permutations (deleteone x xs) ]

--  ----------------------------
-- | Permutations time analysis |
--  ----------------------------
--
--        n    |    s
--    ----------------------
--        5    |     0.02
--        8    |     1.85
--        9    |    18.89
--        10   |   216.27
--
--   Working out constants:
--   
--     0.02/(5! 5 log 5)    = 0.000476892
--     1.85/(8! 8 log 8)    = 0.000006351
--    18.89/(9! 9 log 9)    = 0.000006061
--   216.27/(10! 10 log 10) = 0.000005960 
--
--   If we exclude the first result (i.e. n = 5) we get a somewhat constant value of around 
--   0.000006. The small differences are more likely to do with memory allocation of the computer
--   etc. than they are to do with the complexity itself, so I believe that the value may well be
--   a constant (for each individual machine).
--
