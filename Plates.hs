module Plates where

import Prelude 

--SYNTAX for Plates

type Prog = [Cmd]

-- Do we need a pop operation ?? or can we just use (x:xs)

data Cmd = 
        |Expr
        | Test
        | Stmt

--thing that return ints
data Expr = 
        |  Add
        |  Sub
        |  Mul
        |  Equ

--things that return bools
data Test =
        |  Greater
        |  Equ

--thing that don't return anything
data Stmt = PushN Int
        |   PushB Bool
        |   PushS String
        |   While Prog
        |   IfElse Prog Prog    
    
--saving functions and static type systems for later

-- SEMANTICS
data StackItem = 
        | B Bool
        | I Int
        | S String

type Stack = [StackItem]

type Domain = Stack -> Maybe Stack

evalExpr :: Expr -> Domain
evalExpr Add (x:y:s) = PushN (x + y)
evalExpr Sub =
evalExpr Mul =
evalExpr Equ =

cmd :: Cmd -> Domain
cmd makeExpr e s = evalExpr e s 


--Syntactic Sugar
true :: Prog
true = [PushN 1, PushN 1, Equ]

false :: Prog
false = [PushN 0, PushN 1, Equ]

not :: Test -> Prog
not e = [PushB e, IfElse false true] 

less :: Test
less = not Greater

and :: Test -> Test
