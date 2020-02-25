module Plates where

import Prelude 

--SYNTAX for Plates

type Prog = [Cmd]

-- Do we need a pop operation ?? or can we just use (x:xs)

data Cmd  
--thing that return ints
        =  Add
        |  Sub
        |  Mul
--things that return bools
        |  Greater
        |  Equ
--thing that don't return anything
        |   PushN Int
        |   PushB Bool
        |   PushS String
        |   Loop Prog
        |   IfElse Prog Prog    
    deriving (Eq, Show)    
--saving functions and static type systems for later

-- SEMANTICS
data StackItem 
        = B Bool
        | N Int
        | S String
    deriving (Eq, Show)    

type Stack = [StackItem]

type Domain = Stack -> Maybe Stack


cmd :: Cmd -> Domain
cmd Add             (x:y:s) = case (x, y) of
                                (N i, N j) -> Just (N (i + j) : s)
                                (S i, S j) -> Just (S (i++j) : s)
                                (_, _)     -> Nothing  
cmd Sub             stack   =undefined
cmd Mul             stack   =undefined
cmd Greater         stack   =   undefined
cmd Equ             stack   =undefined
cmd (PushN   n)       stack   =  undefined
cmd (PushB   b)       stack   =undefined
cmd (PushS   str)     stack   =  undefined
cmd (Loop    p)       stack   =undefined
cmd (IfElse  pt pf)   stack   = undefined

--Syntactic Sugar
--true :: Prog
--true = [PushN 1, PushN 1, Equ]

--false :: Prog
--false = [PushN 0, PushN 1, Equ]

--not :: Test -> Prog
--not e = [PushB e, IfElse false true] 

--less :: Test
--less = not Greater

--and :: Test -> Test
