module ackermann where

open import IO
open import Agda.Builtin.Nat
open import Data.Nat.Show

ack : Nat -> Nat -> Nat
ack zero n = suc n
ack (suc m) zero = ack m (suc zero)
ack (suc m) (suc n) = ack m (ack (suc m) n)

main = run (putStrLn (show (ack 4 0)))