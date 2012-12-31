module Main (main) where

{-
data Prop = ...
data Polar = Pos Prop | Neg Prop
data Conj = T | Polar :&: Conj
data Disj = F | Conj :|: Disj
data Quant = Forall Var | Exists Var
data Prenex = Prenex (List Quant) Disj
-}


main :: IO ()
main = putStrLn "Hello, github!"
