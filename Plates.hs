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
        |   Swap
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

--type Domain = Stack -> Maybe Stack
data Value
	= NV Int
	| BV Bool
	| SV String
	| Error
    deriving(Eq, Show)
type Domain = Stack -> Value
  

-- cmd :: Cmd -> Stack -> sMaybe Stack
-- cmd Add             (x:y:s) = case (x, y) of
--                                 (N i, N j) -> Just (N (i + j) : s)
--                                 (S i, S j) -> Just (S (i++j) : s)
--                                 (_, _)     -> Nothing  
-- cmd Sub             (x:y:s) = case (x,y) of
--                                 (N i, N j) -> Just (N (i - j) : s)
--                                 (_, _)     -> Nothing  
-- cmd Mul             (x:y:s) = case (x,y) of
--                                 (N i, N j) -> Just (N (i * j) : s)
--                                 (_, _)     -> Nothing
-- cmd Greater         (x:y:s) = case (x,y) of
--                                 (N i, N j) -> if i > j then Just (B (True) : s) else Just (B (False): s)
--                                 (_, _)     -> Nothing
-- cmd Equ             (x:y:s) = case (x,y) of
--                                 (N i, N j) -> if i == j then Just (B (True) : s) else Just (B (False): s)
--                                 (S i, S j) -> if i == j then Just (B (True) : s) else Just (B (False): s)
--                                 (_, _)     -> Nothing
-- cmd (PushN   n)       stack   = Just(N n: stack)
-- cmd (PushB   b)       stack   = Just(B b: stack)
-- cmd (PushS   str)     stack   = Just(S str: stack)
-- cmd (PushF   prog)    stack   = Just(F prog: stack)
-- cmd (Pop)             (x:s)   = Just(s)
-- --cmd (Loop    p)       (x:s)   = case x of 
-- --                                    (N int) -> if (int > 0) then for p int s else Just s
--                                     --_ -> Nothing
-- cmd (IfElse  pt pf)   (x:s)  = case x of
--                                 (B True) ->  prog pt s
--                                 (B False)->  prog pf s
--                                 _         -> Nothing
-- cmd (Call)          (p:s) = case p of
--                                 (F cs) -> prog cs s
--                                 _ -> Nothing
-- cmd (Offset i)        stack  =  offsets i stack []
-- cmd (Swap)            (x:y:stack) = Just(y:x:stack)

-- offsets :: Int->Stack->Stack-> Maybe Stack
-- offsets 0 (x:stack1) stack2 = Just(x:stack2++x:stack1)
-- offsets n (x:stack1) stack2 = offsets (n-1) (stack1) (stack2++[x])

 

--loop helper function as a for loop
--for :: Prog -> Int -> Stack -> Maybe Stack
--for p num stack = if (num > 0) then case (prog p stack) of
--                                        Just stack -> for p (num-1) stack 
  --                                      _ -> Nothing      
    --                                                        else Just stack

--STATICALLY TYPED VARIANT

-- Define the syntax of types
data Type = TInt | TBool | TFunc Prog | TString | TError
	deriving(Eq,Show)

type StackType = [Type]

-- Define the typing relation
typeHandle :: Prog -> StackType -> Maybe StackType
typeHandle [] stackType = Just stackType
typeHandle (x:prog) stackType = case typeOf x stackType of 
					(TError:stackType')   -> Nothing
					stackType' -> typeHandle prog (stackType')

typeOf :: Cmd -> StackType -> StackType
typeOf Add          stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TInt:s)
                                                (TString, TString) -> (TString:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf Sub          stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TInt:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                   
typeOf Mul          stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TInt:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf Greater      stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TBool:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf Equ          stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TBool:s)
                                                (TString, TString) -> (TBool:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf (PushN   n)    stack   = (TInt:stack)
typeOf (PushB   b)       stack   = (TBool:stack)
typeOf (PushS   str)     stack   = (TString:stack)
typeOf (PushF   prog)    stack   = ((TFunc prog):stack)
typeOf (Pop)             stack   = case stack of
                                        (x:s) -> s
                                        (x) -> [] 
                                        _ -> (TError:stack)
--cmd (Loop    p)       (x:s)   = case x of 
--                                    (N int) -> if (int > 0) then for p int s else Just s
--                                    _ -> Nothing

typeOf (IfElse  pt pf)   (x:s)  = case x of
                                (TBool) ->  case typeHandle pt s of
                                                Nothing -> (TError:s)
                                                Just (x:ptStackType) -> case typeHandle pf s of
                                                                        Nothing -> (TError:s)
                                                                        Just (y:pfStackType) -> if (x == y) then (x:s) else (TError:s)
                                _-> (TError:s)
typeOf (Call)          stack = case stack of
                                    (p:s) -> case p of
                                                TFunc prog -> case typeHandle prog s of 
                                                                    Nothing -> (TError:s)
                                                                    Just progStackType -> (progStackType++s)
                                                _ -> (TError:s)
                                    _ -> (TError:stack)
typeOf (Offset i)       stack  =  ((offsetsType i stack []):stack)
typeOf (Swap)           (x:y:stack) = (y:x:stack)

offsetsType :: Int->StackType->StackType-> Type
offsetsType _ [] stack2 = TError
offsetsType 0 (x:stack1) stack2 = x
offsetsType n (x:stack1) stack2 = offsetsType (n-1) (stack1) (stack2++[x])


-- Define the semantics of type-correct programs
cmd' :: Cmd -> Stack -> Stack
cmd' Add             (x:y:s) = case (x, y) of
                                (N i, N j) -> (N (i + j) : s)
                                (S i, S j) -> (S (i++j) : s) 
cmd' Sub             (x:y:s) = case (x, y) of
                                (N i, N j) -> (N (i - j) : s)
cmd' Mul             (x:y:s) = case (x, y) of
                                (N i, N j) -> (N (i * j) : s)
cmd' Greater         (x:y:s) = case (x, y) of
                                (N i, N j) -> if i > j then (B (True) : s) else (B (False): s)
cmd' Equ             (x:y:s) = case (x,y) of
                                (N i, N j) -> if i == j then (B (True) : s) else (B (False): s)
                                (S i, S j) -> if i == j then (B (True) : s) else (B (False): s)
cmd' (PushN   n)       stack   = (N n: stack)
cmd' (PushB   b)       stack   = (B b: stack)
cmd' (PushS   str)     stack   = (S str: stack)
cmd' (PushF   prog)    stack   = (F prog: stack)
cmd' (Pop)             (x:s)   = (s)
--cmd (Loop    p)       (x:s)   = case x of 
--                                    (N int) -> if (int > 0) then for p int s else Just s
                                    --_ -> Nothing
cmd' (IfElse  pt pf)   (x:s)  = case x of
                                (B True) ->  progStack pt s
                                (B False)->  progStack pf s
cmd' (Call)          (p:s) = case p of
                                (F cs) -> progStack cs s
cmd' (Offset i)        stack  =  offsets' i stack []
cmd' (Swap)            (x:y:stack) = (y:x:stack)

offsets' :: Int->Stack->Stack-> Stack
offsets' 0 (x:stack1) stack2 = (x:stack2++x:stack1)
offsets' n (x:stack1) stack2 = offsets' (n-1) (stack1) (stack2++[x])

-- Define our interpreter
eval :: Prog -> Maybe StackType
eval prog = typeHandle prog []


--evaluates a list of commands
prog :: Prog -> Domain
prog []         (x:s) = case x of 
				N i -> NV i
				B b -> BV b
				S s -> SV s			
prog (x:p)      stack = prog p (cmd' x stack)

progStack :: Prog -> Stack -> Stack
progStack []         stack = stack		
progStack (x:p)      stack = progStack p (cmd' x stack)


-- starts a program with an empty stack and statically type checks it
exec :: Prog -> Value
exec p = case eval p of 
		Nothing -> Error
		Just (TFunc prog:s) -> Error
		_ 	-> prog p []
                  
goodEx1 :: Int -> Prog
goodEx1 x = [PushN 6, PushN x, Equ, IfElse [PushS "YouWin"] [PushS "YouLose"]]

goodEx2 :: Int -> Prog
goodEx2 x = [PushN x, PushF [Offset 0, PushN 0, Equ, IfElse [Pop, Pop, PushN 1] [Offset 1, PushN 1, Offset 2, Sub, Offset 1, Call, Mul]], PushN x, Offset 1, Call]

testEx :: Prog 
testEx = [PushF [Offset 0, PushN 0, Equ, IfElse [Pop, Pop, PushN 1] [Offset 1, PushN 1, Offset 2, Sub, Offset 1, Call]], PushN 3, Offset 1, Call]

testEx2 :: Prog
testEx2 = [PushN 3, PushF [PushB False, IfElse [PushN 1] [PushB True, PushN 2] ], Call]

testEx3 :: Prog 
testEx3 = [PushF [Add]]

--type error
--badEx1 :: Prog
badEx1 x = [PushN 6, PushN x, Equ, IfElse [PushS "YouWin", PushN 1] [PushN 0, PushS "YouLose"]]

--stack underflow error
badEx2 :: Prog
badEx2 = [PushN 1, PushN 2, Add, Add]

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
