module Main where

{-
 -Using the pseudocode from Wikipedia:
 -http://en.wikipedia.org/wiki/Backtracking
 -}

import Table
import Data.List

main :: IO ()
main = print $ (head . nub . concatMap (bt t)) [Pos x y | x <- [0..d-1], y <- [0..d-1]]
    where d = 8
          t = emptyTable d

bt :: Table -> Pos -> [Table]
bt t@(Table _ d bs) p
       | reject t p = []
       | accept t p = [add p t]
       | otherwise  = concatMap (bt (add p t)) [Pos x y | x <- [0..d-1], y <- [0..d-1], bs !! x !! y == Empty]

add :: Pos -> Table -> Table
add (Pos x y) (Table q d bs) = Table (q+1) d [[ if bs !! x' !! y' == Empty then set x' y' else bs !! x' !! y'  | y' <- [0..d-1] ] | x' <- [0..d-1]]
    where
        set x' y' | x == x' && y == y'              = Queen
                  | row x' || col y' || slant x' y' = Occupied
                  | otherwise                       = Empty
        row = (==)x
        col = (==y)
        slant a b = abs x - a == abs y - b

reject :: Table -> Pos -> Bool
reject (Table _ _ bs) (Pos x y) = bs !! x !! y /= Empty

accept :: Table -> Pos -> Bool
accept t p = d == q
    where (Table q d _) = add p t
