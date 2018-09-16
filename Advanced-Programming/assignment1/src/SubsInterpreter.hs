module SubsInterpreter
       (
         Value(..)
       , runExpr
       -- You may include additional exports here, if you want to
       -- write unit tests for them.
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", (===:))
                       , ("<", (<:))
                       , ("+", (+:))
                       , ("*", (*:))
                       , ("-", (-:))
                       , ("%", (%:))
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM (\(e, p) -> Right (x, e))
  SubsM m >>= f = SubsM $ (\c -> case m c of
                             Right (a,e) -> runSubsM (f a) (e, snd c)
                             Left l -> Left l )
  fail s = SubsM (\_ -> Left s)

-- You may modify these if you want, but it shouldn't be necessary
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap

--Bool To Value
btv :: Bool -> Value
btv v = if v then TrueVal else FalseVal

-- Get the standard error message
stderr :: String -> Either Error Value
stderr s = Left $ s ++ "called with wrong number of type of arguments"

(===:) :: Primitive
(===:) [IntVal a, IntVal b] = Right $ btv $ a == b
(===:) [UndefinedVal, UndefinedVal] = Right TrueVal
(===:) [TrueVal, TrueVal] = Right TrueVal
(===:) [FalseVal, FalseVal] = Right TrueVal
(===:) [ArrayVal (x:xs), ArrayVal (y:ys)] =
        do curr <- ((===:) [x,y])
           rest <- ((===:) [ArrayVal xs, ArrayVal ys])
           return $ btv $ (curr == TrueVal) && (rest == TrueVal)
(===:) [ArrayVal [], ArrayVal []] = Right TrueVal
(===:) [a,b] = Right FalseVal
(===:) _ = stderr "==="

(<:) :: Primitive
(<:) [IntVal a, IntVal b] = Right $ btv (a < b)
(<:) [StringVal a, StringVal b] = Right $ btv (a < b)
(<:) _ = stderr "<"

(+:) :: Primitive
(+:) [IntVal a, IntVal b] = Right $ IntVal (a+b)
(+:) [StringVal a, StringVal b] = Right $ StringVal (a++b)
(+:) [StringVal a, IntVal b] = Right $ StringVal (a ++ show b)
(+:) [IntVal a, StringVal b] = Right $ StringVal (show a ++ b)
(+:) _ = stderr "+"

(*:) :: Primitive
(*:) [IntVal a, IntVal b] = Right $ IntVal (a*b)
(*:) _ = stderr "*"

(-:) :: Primitive
(-:) [IntVal a, IntVal b] = Right $ IntVal (a-b)
(-:) _ = stderr "-"

(%:) :: Primitive
(%:) [IntVal a, IntVal 0] = Left "Divide by zero error in %"
(%:) [IntVal a, IntVal b] = Right $ IntVal (a `mod` b)
(%:) _ = stderr "%"

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\(e, _) -> Right ((), f e))

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv (\e -> Map.insert name val e )

getVar :: Ident -> SubsM Value
getVar name =
  SubsM (\(e, _) ->
    case Map.lookup name e of
      Just r -> Right (r, e)
      Nothing -> Left ("No ident \"" ++ name ++ "\" is defined"))

getFunction :: FunName -> SubsM Primitive
getFunction name =
  SubsM (\(e, p) ->
    case Map.lookup name p of
      Just r -> Right (r, e)
      Nothing -> Left ("No fun named \"" ++ name ++ "\" is defined"))

evalExpr :: Expr -> SubsM Value
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Number n) = return $ IntVal n
evalExpr (String s) = return $ StringVal s
evalExpr (Array e) = undefined
evalExpr (Var i) = getVar i
evalExpr (Call n e) = undefined
evalExpr (Assign i e) = undefined
evalExpr (Comma e1 e2) = undefined
evalExpr (Compr a) = undefined

runExpr :: Expr -> Either Error Value
runExpr expr = case (runSubsM (evalExpr expr)) initialContext of
                 Right r -> Right $ fst r
                 Left l -> Left l
