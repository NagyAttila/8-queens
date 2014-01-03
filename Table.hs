module Table where

type Dim = Int
type Queens = Int
data State = Empty | Occupied | Queen
          deriving Eq
data Table = Table Queens Dim [[State]]
           deriving Eq
data Pos = Pos Int Int
         deriving Show

instance Show Table where
    show (Table q d bs) = queens ++ dim ++ concatMap (\x -> map p x ++ "\n") bs
        where p x | x == Empty    = '.'
                  | x == Occupied = 'X'
                  | otherwise     = 'Q'
              queens = "Queens: " ++ show q ++ "\n"
              dim    = "Dim: " ++ show d ++ "\n"

emptyTable :: Dim -> Table
emptyTable d = Table 0 d [[Empty | _ <- [1..d]] | _ <- [1..d] ]
