module Main (main) where

import Data.Text


data Proposition = Predicate Text Variable
data Variable = Variable Text
data Polar = Positive Proposition | Negative Proposition
data Conjunction = T | Polar :&: Conjunction
data Disjunction = F | Conjunction :|: Disjunction
data Quantification = Forall Variable | Exists Variable
data Prenex = Prenex [Quantification] Disjunction


main :: IO ()
main = putStrLn "Hello, github!"
