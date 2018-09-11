import Arithmetic
import Definitions

test'all :: Bool
test'all = all (==True) (test'showExp ++ test'evalSimple ++ test'extendEnv ++ test'evalFull)

testEquals ::(Eq a) => [(a, a)] -> [Bool]
testEquals [] = []
testEquals ((e,s):xs) = (e == s) : testEquals xs

showTests ::(Show a) => [(a, a)] -> [String]
showTests [] = []
showTests ((e,_):xs) = show e : showTests xs

test'showExp :: [Bool]
test'showExp = testEquals [
    (showExp (Cst 2), "2"),
    (showExp (Cst (-1)), "(-1)"),
    (showExp (Add (Cst (-1)) (Cst 20)), "((-1)+20)"),
    (showExp (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)), "(((-1)+20)-3)"),
    (showExp (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)), "((((-1)+20)-3)*4)"),
    (showExp (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)), "(((((-1)+20)-3)*4)/5)"),
    (showExp (Pow (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)) (Cst 6)), "((((((-1)+20)-3)*4)/5)^6)"),
    (showExp (Pow (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)) (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5))), "((((((-1)+20)-3)*4)/5)^(((((-1)+20)-3)*4)/5))")]

test'evalSimple :: [Bool]
test'evalSimple = testEquals [
    (evalSimple (Cst 2), 2),
    (evalSimple (Cst (-1)), (-1)),
    (evalSimple (Add (Cst (-1)) (Cst 20)), 19),
    (evalSimple (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)), 16),
    (evalSimple (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)), 64),
    (evalSimple (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)), 12),
    (evalSimple (Pow (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)) (Cst 6)), 2985984),
    (evalSimple (Pow (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)) (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5))), 12 ^ 12)]

test'extendEnv :: [Bool]
test'extendEnv = testEquals [
    (env "x1", Nothing),-- Testing unbound
    ((extendEnv "x1" 1 env) "x1", Just 1), -- Testing bound x1
    ((extendEnv "x2" 2 (extendEnv "x1" 1 env)) "x2", Just 2), --Test access most recently bound
    ((extendEnv "x2" 2 (extendEnv "x1" 1 env)) "x1", Just 1), --Test access oldest bound
    ((extendEnv "x1" 2 (extendEnv "x1" 1 env)) "x1", Just 2)] --Test access rebound var
    where env = initEnv

test'evalFull :: [Bool]
test'evalFull = testEquals [
    (evalFull (Cst 2) env, 2),
    (evalFull (Cst (-1)) env, (-1)),
    (evalFull (Add (Cst (-1)) (Cst 20)) env, 19),
    (evalFull (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) env, 16),
    (evalFull (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) env, 64),
    (evalFull (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)) env, 12),
    (evalFull (Pow (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)) (Cst 6)) env, 2985984),
    (evalFull (Pow (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5)) (Div (Mul (Sub (Add (Cst (-1)) (Cst 20)) (Cst 3)) (Cst 4)) (Cst 5))) env, 12 ^ 12),
    (evalFull (If (Sub (Cst 2) (Cst 2)) (Div (Cst 3) (Cst 0)) (Cst 5)) env, 5),
    (evalFull (Let {var = "x", aux = Cst 5,
               body = Add (Let {var = "x", aux = Add (Cst 3) (Cst 4),
               body = Mul (Var "x") (Var "x")})
               (Var "x")}) env, 54),
    (evalFull (Let "x" (Div (Cst 4) (Cst 0)) (Cst 5)) env, 5),
    (evalFull (Var "x") env, 10),
    (evalFull (Sum "y" (Cst 1) (Cst 100) (Mul (Var "y") (Var "x"))) env, 50500)]
    where env = extendEnv "x" 10 initEnv

-- test'evalErr
-- test'sumEither
-- test'evalEither
-- test'fromRight