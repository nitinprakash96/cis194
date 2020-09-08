module Week5.Calc where

import Week5.ExprT
import Week5.Parser
import Data.Maybe

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
eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


{-
 Exercise 2:

 The UI department has internalized the focus group data and is ready to
 synergize with you. They have developed the front-facing user-interface:
 a parser that handles the textual representation of the selected language.
 They have sent you the module Parser.hs, which exports parseExp, a parser
 for arithmetic expressions. If you pass the constructors of ExprT to it as
 arguments, it will convert Strings representing arithmetic expressions into
 values of type ExprT.

 For example:
    *Calc> parseExp Lit Add Mul "(2+3)*4"
    Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

    *Calc> parseExp Lit Add Mul "2+3*4"
    Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))

    *Calc> parseExp Lit Add Mul "2+3*"
    Nothing

 Leverage the assets of the UI team to implement the value-added function
    evalStr :: String -> Maybe Integer

 which evaluates arithmetic expressions given as a String, producing Nothing for
 inputs which are not well-formed expressions, and Just n for well-formed inputs
 that evaluate to n
-}
evalStr :: String -> Maybe Integer
evalStr ex = case parseExp Lit Add Mul ex of
               Just x -> Just $ eval x
               Nothing -> Nothing

-- Perhaps the above can be rewritten using guards
evalStr' :: String -> Maybe Integer
evalStr' ex
  | isNothing x = Nothing
  | otherwise   = Just $ (eval . fromJust) x
  where x = parseExp Lit Add Mul ex


-- I think there can be another implementation that uses function composition
evalStr'' :: String -> Maybe Integer
evalStr'' = fmap eval . parseExp Lit Add Mul
