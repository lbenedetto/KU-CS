-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions
import Data.Maybe
import Data.Either

showExp :: Exp -> String
showExp (Cst i) = if i < 0 then "(" ++ show i ++ ")" else show i
showExp (Add l r) = "("++showExp l++"+"++showExp r++")"
showExp (Sub l r) = "("++showExp l++"-"++showExp r++")"
showExp (Mul l r) = "("++showExp l++"*"++showExp r++")"
showExp (Div l r) = "("++showExp l++"/"++showExp r++")"
showExp (Pow l r) = "("++showExp l++"^"++showExp r++")"
showExp _ = error $ show (EOther "No matching operator")

evalSimple :: Exp -> Integer
evalSimple (Cst i) = i
evalSimple (Add l r) = (evalSimple l) + (evalSimple r)
evalSimple (Sub l r) = (evalSimple l) - (evalSimple r)
evalSimple (Mul l r) = (evalSimple l) * (evalSimple r)
evalSimple (Div l r) = (evalSimple l) `div` (evalSimple r)
evalSimple (Pow l r) = if exponent >= 0
                         then seq num (num ^ exponent)
                         else error $ show ENegPower
                       where exponent = evalSimple r
                             num = evalSimple l
evalSimple _ = error $ show (EOther "No matching operator")

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r search = if search == v then Just n else r search

evalFull :: Exp -> Env -> Integer
evalFull (Cst i) _ = i
evalFull (Add l r) e = (evalFull l e) + (evalFull r e)
evalFull (Sub l r) e = (evalFull l e) - (evalFull r e)
evalFull (Mul l r) e = (evalFull l e) * (evalFull r e)
evalFull (Div l r) e = (evalFull l e) `div` (evalFull r e)
-- base == base forces base to be evaluated, causing errors to surface
evalFull (Pow l r) e = if exponent >= 0
                         then seq base (base ^ exponent)
                         else error $ show ENegPower
                       where exponent = evalFull r e
                             base = evalFull l e
evalFull (If test yes no) e = if (evalFull test e) /= 0
                                then evalFull yes e
                                else evalFull no e
evalFull (Var name) e = fromMaybe (error $ show (EBadVar name)) result
                        where result = e name
evalFull (Let var aux body) e = evalFull body (extendEnv var (evalFull aux e) e)
evalFull (Sum v e1 e2 e3) e = sum $ map
                                  (\n -> evalFull (Let v (Cst n) e3) e) [n1..n2]
                              where n1 = (evalFull e1 e)
                                    n2 = (evalFull e2 e)

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst i) _ = Right i
evalErr (Add l r) e = evalEither (evalErr l e) (+) (evalErr r e)
evalErr (Sub l r) e = evalEither (evalErr l e) (-) (evalErr r e)
evalErr (Mul l r) e = evalEither (evalErr l e) (*) (evalErr r e)
evalErr (Div l r) e = if isRight d
                        then if fromRight' d /= 0
                          then evalEither (evalErr l e) div d
                          else Left EDivZero
                        else d
                      where d = (evalErr r e)
evalErr (Pow l r) e = if isRight exponent
                        then if fromRight' exponent >= 0
                          then evalEither (evalErr l e) (^) exponent
                          else Left ENegPower
                        else exponent
                      where exponent = evalErr r e
evalErr (If test yes no) e = if isRight testResult
                               then if (fromRight' testResult) /= 0
                                  then evalErr yes e
                                  else evalErr no e
                               else testResult
                             where testResult = (evalErr test e)
evalErr (Var name) e = maybe (Left $ EBadVar name) Right result
                           where result = e name
evalErr (Let v a b) e = if isRight r
                          then evalErr b (extendEnv v (fromRight' r) e)
                          else r
                        where r = (evalErr a e)
evalErr (Sum v e1 e2 e3) e = if isRight n1
                               then if isRight n2
                                 then sumEither (Right 0) (map
                                      (\n -> evalErr (Let v (Cst n) e3) e)
                                      [fromRight' n1 .. fromRight' n2])
                                 else n2
                               else n1
                             where n1 = (evalErr e1 e)
                                   n2 = (evalErr e2 e)

-- Like sum, but if any elements in the [] are ArithError,
-- returns the error instead of the sum
sumEither :: (Num i) => Either a i -> [Either a i] -> Either a i
sumEither s [] = s
sumEither s (x:xs) = if isRight s
                     then if isRight x
                        then if isRight r
                            then Right $ (fromRight' x) + (fromRight' r)
                            else r
                        else x
                     else s
                     where r = sumEither s xs

-- Performs the passed in function on the other args if they are ints,
-- else return the error
evalEither :: Either a i -> (i -> i -> i) -> Either a i -> Either a i
evalEither n1 f n2 = if isRight n1
                       then if isRight n2
                         then Right $ f (fromRight' n1) (fromRight' n2)
                         else n2
                       else n1

-- similar to (fromRight :: b -> Either a b -> b) from Data.Either,
-- but instead of returning a default value, throws an error
fromRight' :: Either a b -> b
fromRight' (Right c) = c
fromRight' _ = error "Do not use this method without checking isRight first"

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
