# CS381_FinalProject
Zach Bishop - bishopz

Lindsey Kvarfordt - kvarforl

ThuyVy Nguyen - nguythu2

This is Plates, a stack based language. It features static typing, recursive functions, strings, and not as many puns as we thought it was going to.


To run the examples: 
```haskell
exec (<exName> <optional arguments>)
```

guessing game:   exec (goodEx1 9)    -- Output: SV (S "YouLose")

factorial:       exec (goodEx2 5)    -- Output: NV 120

type error:      exec (badEx1 5)     -- Output: Error

type error:      exec badEx2         -- Output: Error

stack underflow: exec badEx3         -- Output: Error 
