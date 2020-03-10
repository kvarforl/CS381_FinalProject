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
--thing that don't return anything/stack operations
        |   PushN Int
        |   PushB Bool
        |   PushS String
        |   PushF Prog
        |   Pop
 --       |   Loop Prog
        |   IfElse Prog Prog    
        |   Call
        |   Offset Int
    deriving (Eq, Show)    
--saving functions and static type systems for later

-- SEMANTICS
data StackItem 
        = B Bool
        | N Int
        | S String
        | F Prog
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
cmd (PushF   prog)    stack   = Just(F prog: stack)
cmd (Pop)             (x:s)   = Just(s)
--cmd (Loop    p)       (x:s)   = case x of 
--                                    (N int) -> if (int > 0) then for p int s else Just s
                                    --_ -> Nothing
cmd (IfElse  pt pf)   (x:s)  = case x of
                                (B True) ->  prog pt s
                                (B False)->  prog pf s
                                _         -> Nothing
cmd (Call)          (p:s) = case p of
                                (F cs) -> prog cs s
                                _ -> Nothing
cmd (Offset i)        stack  = undefined

--loop helper function as a for loop
--for :: Prog -> Int -> Stack -> Maybe Stack
--for p num stack = if (num > 0) then case (prog p stack) of
--                                        Just stack -> for p (num-1) stack 
  --                                      _ -> Nothing      
    --                                                        else Just stack

--STATICALLY TYPED VARIANT

-- Define the syntax of types
data Type = TInt | TBool | TFunc | TString | TError
	deriving(Eq,Show)

type StackType = [Type]

-- Define the typing relation
typeHandle :: Prog -> StackType -> Maybe StackType
typeHandle [] stackType = Just stackType
typeHandle (x:prog) stackType = case typeOf x stackType of 
					(TError:stackType')   -> Nothing
					stackType' -> typeHandle prog (stackType')

typeOf :: Cmd -> StackType -> StackType
typeOf Add          (x:y:s) = case (x, y) of
                                (TInt, TInt) -> (TInt:s)
                                (TString, TString) -> (TString:s)
                                (_, _)     -> (TError:s)  
typeOf Sub          (x:y:s) = case (x,y) of
                                (TInt, TInt) -> (TInt:s)
                                (_, _)     -> (TError:s) 
typeOf Mul          (x:y:s) = case (x,y) of
                                (TInt, TInt) -> (TInt:s)
                                (_, _)     -> (TError:s)
typeOf Greater      (x:y:s) = case (x,y) of
                                (TInt, TInt) -> (TBool:s)
                                (_, _)     -> (TError:s)
typeOf Equ          (x:y:s) = case (x,y) of
                                (TInt, TInt) -> (TBool:s)
                                (TString, TString) -> (TBool:s)
                                (_, _)     -> (TError:s)
typeOf (PushN   n)    stack   = (TInt:stack)
typeOf (PushB   b)       stack   = (TBool:stack)
typeOf (PushS   str)     stack   = (TString:stack)
typeOf (PushF   prog)    stack   = case typeHandle prog [] of
					Nothing -> (TError:stack)
					Just progStackType -> ((TFunc : progStackType) ++ stack)
typeOf (Pop)             (x:s)   = s
--cmd (Loop    p)       (x:s)   = case x of 
--                                    (N int) -> if (int > 0) then for p int s else Just s
--                                    _ -> Nothing

typeOf (IfElse  pt pf)   (x:s)  = case x of
                                (TBool) ->  case typeHandle pt [] of
						Nothing -> (TError:s)
						Just ptStackType -> case typeHandle pf [] of
								    	Nothing -> (TError:s)
									Just pfStackType -> if (ptStackType == pfStackType) then (ptStackType ++ s) else (TError:s)
                                _-> (TError:s)
typeOf (Call)          (p:s) = case p of
				TFunc -> s
				_ -> (TError:s)
typeOf (Offset i)        stack  = undefined

-- Define the semantics of type-correct programs


-- Define our interpreter
eval :: Prog -> Maybe StackType
eval prog = typeHandle prog []


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
                    
goodEx :: Int -> Prog
goodEx x = [PushN 6, PushN x, Equ, IfElse [PushS "YouWin"] [PushS "YouLose"]]

testEx :: Prog 
testEx = [PushB True, IfElse [PushN 6] [PushN 7]]

--type error
--badEx1 :: Prog
--badEx1 = [PushB False, Loop [PushS "this tries to use a boolean as a counter"]]

--stack underflow error
badEx2 :: Prog
badEx2 = [PushN 5, PushN 6, Add, Add]

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
