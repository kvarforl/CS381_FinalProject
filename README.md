# CS381_FinalProject
Zach Bishop - bishopz

Lindsey Kvarfordt - kvarforl

ThuyVy Nguyen - nguythu2

This is Plates, a stack based language. It features static typing and strings. 


To run the examples: exec (<exName> <optional arguments>)
NOTE: factorial has to be run using prog (<exname> <optional arguments>) []

Ex guessing game: exec (goodEx1 9) -- Output: SV (S "YouLose")
Ex factorial (needs to be run differently because static type checking does not work with recursion): prog (goodEx2 5) [] -- Output: NV 120
Ex type error: exec (badEx1 5) -- Error
Ex type error: exec badEx2 -- Error
Ex stack underflow: exec badEx3 -- Error 
