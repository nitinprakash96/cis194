module Week5.Calc where


{-
 PREFACE:

 On day one of your new job as a software engineer, you’ve been asked to
 program the brains of the company’s new blockbuster product: a calculator.
 But this isn’t just any calculator! Extensive focus group analysis has
 revealed that what people really want out of their calculator is something
 that can add and multiply integers. Anything more just clutters the interface.

 Your boss has already started by modeling the domain with the following
 data type of arithmetic expressions:

    data ExprT = Lit Integer
               | Add ExprT ExprT
               | Mul ExprT ExprT
      deriving (Show, Eq)

 This type is capable of representing expressions involving integer
 constants, addition, and multiplication.
 For example, the expression
    (2 + 3) × 4 would be represented by the value
    Mul (Add (Lit 2) (Lit 3)) (Lit 4).

 Your boss has already provided the definition of ExprT in ExprT.hs,
 so as usual you just need to add import ExprT to the top of your file.

 However, this is where your boss got stuck.
-}

{-
 Exercise 1:

 Write Version 1 of the calculator: an evaluator for ExprT, with the signature
    eval :: ExprT -> Integer

 For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
-}
