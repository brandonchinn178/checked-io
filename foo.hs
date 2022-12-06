import Data.Typeable
import Control.Monad.Exception
data DivideByZero = DivideByZero deriving (Show, Typeable)
data SumOverflow  = SumOverflow  deriving (Show, Typeable)
instance Exception DivideByZero
instance Exception SumOverflow
data Expr = Add Expr Expr | Div Expr Expr | Val Double

eval (Val x)     = return x
eval (Add a1 a2) = do
   v1 <- eval a1
   v2 <- eval a2
   let sum = v1 + v2
   if sum < v1 || sum < v2 then throw SumOverflow else return sum
eval (Div a1 a2) = do
   v1 <- eval a1
   v2 <- eval a2
   if v2 == 0 then throw DivideByZero else return (v1 / v2)
