module Plates where

import Prelude hiding (lookup)
import Data.Map

--SYNTAX for Plates

type Prog = [Cmd]

data Cmd  
        =   Add
        |   Sub
        |   Mul
        |   Greater
        |   Equ
        |   PushN Int
        |   PushB Bool
        |   PushS String
        |   PushF String [Type] Type Prog
        |   PushFRec String [Type] Type Prog
        |   Pop
        |   IfElse Prog Prog    
        |   Call
        |   Offset Int
        |   Swap
    deriving (Eq, Show)    

data StackItem 
        = B Bool
        | N Int
        | S String
        | F String [Type] Type Prog
        | RF String [Type] Type Prog
    deriving (Eq, Show)    

type Stack = [StackItem]

data Value
    = NV Int
    | BV Bool
    | SV String
    | Error
    deriving(Eq, Show)
type Domain = Stack -> Value

--STATICALLY TYPED VARIANT

-- Define the syntax of types
data Type = TInt | TBool | TFunc String [Type] Type Prog | TString | TError | TFuncRec String [Type] Type Prog
    deriving(Show)

type StackType = [Type]

--allows equality checking of Types (including input types and output types) but excluding funtion names
instance Eq Type where 
    TInt == TInt = True
    TBool == TBool = True
    TString == TString = True
    TFunc _ inputTypes outputType _ == TFunc _ inputTypes' outputType' _ =
        inputTypes == inputTypes' && outputType == outputType'
    TFuncRec _ inputTypes outputType _ == TFuncRec _ inputTypes' outputType' _ =
        inputTypes == inputTypes' && outputType == outputType'
    _ == _ = False

type Env = Map String ([Type], Type)

-- Define the typing relation
typeHandle :: Prog -> Env -> StackType -> Maybe StackType
typeHandle [] _ stackType = Just stackType
typeHandle (x:prog) env stackType = case typeOf x env stackType of 
                    (TError:stackType')   -> Nothing
                    stackType' -> typeHandle prog env (stackType')

typeOf :: Cmd -> Env -> StackType -> StackType
typeOf Add     _     stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TInt:s)
                                                (TString, TString) -> (TString:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf Sub     _     stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TInt:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                   
typeOf Mul     _     stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TInt:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf Greater _     stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TBool:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf Equ     _     stack =  case stack of
                                    (x:y:s) -> case (x, y) of
                                                (TInt, TInt) -> (TBool:s)
                                                (TString, TString) -> (TBool:s)
                                                (_, _)     -> (TError:s)
                                    _       -> (TError:stack)
                                  
typeOf (PushN   n)   _      stack   = (TInt:stack)
typeOf (PushB   b)   _      stack   = (TBool:stack)
typeOf (PushS   str) _      stack   = (TString:stack)
typeOf (PushF name inputTypes outputType prog)      _   stack   = ((TFunc name inputTypes outputType prog):stack)
typeOf (PushFRec name inputTypes outputType prog)   _   stack   = ((TFuncRec name inputTypes outputType prog):stack)
typeOf (Pop)         _      stack   = case stack of
                                        (x:s) -> s
                                        [] -> (TError:stack)
typeOf (IfElse  pt pf) env  stack   = case stack of 
                                    (x:s)  -> case x of
                                        (TBool) ->  case typeHandle pt env s of
                                                        Nothing -> (TError:s)
                                                        Just (x:ptStackType) -> case typeHandle pf env s of
                                                                                Nothing -> (TError:s)
                                                                                Just (y:pfStackType) -> if (x == y) then (x:s) else (TError:s)
                                        _-> (TError:s)
                                    _ -> (TError:stack)
typeOf (Call)    env        stack   = case stack of
                                    (p:s) -> case p of
                                                TFunc name inputType outputType prog -> case lookup name env of
                                                                                            Just (inputType', outputType') -> case checkFuncType inputType' s of
                                                                                                                                    Just stack' -> (outputType':stack')
                                                                                                                                    Nothing -> (TError:s)
                                                                                            Nothing -> case typeHandle prog (insert name (inputType, outputType) env) s of 
                                                                                                Nothing -> (TError:s)
                                                                                                Just progStackType -> progStackType
                                                TFuncRec name inputType outputType prog -> case lookup name env of
                                                                                            Just (inputType', outputType') -> case checkFuncType (inputType'++[TFuncRec name inputType outputType prog]) s of
                                                                                                                                    Just stack' -> (outputType':stack')
                                                                                                                                    Nothing -> (TError:s)
                                                                                            Nothing -> case typeHandle prog (insert name (inputType, outputType) env) s of 
                                                                                                Nothing -> (TError:s)
                                                                                                Just progStackType -> progStackType
                                                _ -> (TError:s)
                                    _ -> (TError:stack)
typeOf (Offset i)   _    stack  =  ((offsetsType i stack []):stack)
typeOf (Swap)       _    stack = case stack of
                                    (x:y:stack) -> (y:x:stack)
                                    _ -> (TError:stack)

checkFuncType :: [Type] -> StackType -> Maybe StackType
checkFuncType [] stack = Just stack
checkFuncType (TInt:types) (TInt:stack) = checkFuncType types stack
checkFuncType (TBool:types) (TBool:stack) = checkFuncType types stack
checkFuncType (TString:types) (TString:stack) = checkFuncType types stack
checkFuncType ((TFunc _ inputTypes outputTypes _):types) ((TFunc _ inputTypes' outputTypes' _):stack) = 
    if (inputTypes == inputTypes' && outputTypes == outputTypes') then checkFuncType types stack else Nothing
checkFuncType ((TFuncRec _ inputTypes outputTypes _):types) ((TFuncRec _ inputTypes' outputTypes' _):stack) = 
    if (inputTypes == inputTypes' && outputTypes == outputTypes') then checkFuncType types stack else Nothing
checkFuncType _ _ = Nothing

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
cmd' (PushF name inputTypes outputType prog)    stack   = (F name inputTypes outputType prog: stack)
cmd' (PushFRec name inputTypes outputType prog)    stack   = (RF name inputTypes outputType prog: stack)
cmd' (Pop)             (x:s)   = (s)
cmd' (IfElse  pt pf)   (x:s)  = case x of
                                (B True) ->  progStack pt s
                                (B False)->  progStack pf s
cmd' (Call)          (p:s) = case p of
                                (F name inputTypes outputType cs) -> progStack cs s
                                (RF name inputTypes outputType cs) -> progStack cs s
cmd' (Offset i)        stack  =  offsets' i stack []
cmd' (Swap)            (x:y:stack) = (y:x:stack)

offsets' :: Int->Stack->Stack-> Stack
offsets' 0 (x:stack1) stack2 = (x:stack2++x:stack1)
offsets' n (x:stack1) stack2 = offsets' (n-1) (stack1) (stack2++[x])


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
exec p = case typeHandle p (fromList []) [] of 
        Nothing -> Error
        Just (TFunc _ _ _ prog:s) -> Error
        _     -> prog p []

--EXAMPLES:

--guessing game
goodEx1 :: Int -> Prog
goodEx1 x = [PushN 6, PushN x, Equ, IfElse [PushS "YouWin"] [PushS "YouLose"]]

--factorial
goodEx2 :: Int -> Prog
goodEx2 x = [PushFRec "factorial" [TInt] TInt body, PushN x, Offset 1, Call]
    where body = [Offset 0, PushN 1, Equ, IfElse [Swap, Pop] [Offset 1, PushN 1, Offset 2, Sub, Offset 1, Call, Mul, Swap, Pop]]

--type error
badEx1 :: Int -> Prog
badEx1 x = [PushN 6, PushN x, Equ, IfElse [PushS "YouWin", PushN 1] [PushN 0, PushS "YouLose"]]

--type error
badEx2 :: Prog
badEx2 = [PushN 6, PushB False, Add]

--stack underflow error
badEx3 :: Prog
badEx3 = [PushN 1, PushN 2, Add, Add]
