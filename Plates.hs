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
cmd Sub             (x:y:s) = case (x,y) of
                                (N i, N j) -> Just (N (i - j) : s)
                                (_, _)     -> Nothing  
cmd Mul             (x:y:s) = case (x,y) of
                                (N i, N j) -> Just (N (i * j) : s)
                                (_, _)     -> Nothing
cmd Greater         (x:y:s) = case (x,y) of
                                (N i, N j) -> if i > j then Just (B (True) : s) else Just (B (False): s)
                                (_, _)     -> Nothing
cmd Equ             (x:y:s) = case (x,y) of
                                (N i, N j) -> if i == j then Just (B (True) : s) else Just (B (False): s)
                                (S i, S j) -> if i == j then Just (B (True) : s) else Just (B (False): s)
                                (_, _)     -> Nothing
cmd (PushN   n)       stack   = Just(N n: stack)
cmd (PushB   b)       stack   = Just(B b: stack)
cmd (PushS   str)     stack   = Just(S str: stack)
cmd (Loop    p)       (x:s)   = case x of 
                                    (N int) -> if (int > 0) then for p int s else Just s
                                    _ -> Nothing
cmd (IfElse  pt pf)   (x:s)  = case x of
                                (B True) ->  prog pt s
                                (B False)->  prog pf s
                                _         -> Nothing

--loop helper function as a for loop
for :: Prog -> Int -> Stack -> Maybe Stack
for p num stack = if (num > 0) then case (prog p stack) of
                                        Just stack -> for p (num-1) stack 
                                        _ -> Nothing      
                                                            else Just stack

--evaluates a list of commands
prog :: Prog -> Domain
prog []         stack = Just stack
prog (x:s)      stack = case cmd x stack of
                        Just new_stack -> prog s new_stack
                        _ -> Nothing

-- starts a program with an empty stack 
exec :: Prog -> Maybe StackItem
exec p = case prog p [] of
            Just (x:xs) -> Just x
            _ -> Nothing
                    
goodEx :: Prog
goodEx = [PushN 3, PushN 2, Add]

goodEx2 :: Prog
goodEx2 = [PushN 3, Loop [PushN 5, PushN 4]]

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
