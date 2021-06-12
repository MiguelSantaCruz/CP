import Nat
import Cp

data ExpAr a = X
           | N a
           | Bin BinOp (ExpAr a) (ExpAr a)
           | Un UnOp (ExpAr a)
           deriving (Eq, Show)

data BinOp = Sum
           | Product
           deriving (Eq, Show)

data UnOp = Negate
          | E
          deriving (Eq, Show)

inExpAr = either (const X) num_ops where
  num_ops = either N ops
  ops     = either bin (uncurry Un)
  bin(op, (a, b)) = Bin op a b

baseExpAr f g h j k l z = f -|- (g -|- (h >< (j >< k) -|- l >< z))
baseExpAr' g f = baseExpAr id g id f f id f

recExpAr f =  id -|- (id -|- (id >< (f >< f) -|- id >< f))

clean (Bin Product _ (N 0)) = outExpAr $ N 0
clean (Bin Product (N 0) _) = outExpAr $ N 0 
clean x = outExpAr x

sd_gen = either g1 (either g3 (either g5_sd_g g6_sd_g)) where
    g1 _ = (X, N 1)
    g3 a = (N a, N 0)

g5_sd_g (op,((exp1,exp2),(exp3,exp4))) = if op == Sum then (Bin Sum exp1 exp3, Bin Sum exp2 exp4)
                                              else (Bin Product exp1 exp3, Bin Sum (Bin Product exp1 exp4) (Bin Product exp2 exp3))
g6_sd_g (op,(exp1,exp2)) = if op == Negate then (Un op exp1, Un op exp2)
                                else (Un op exp1, Bin Product (Un op exp1) exp2)