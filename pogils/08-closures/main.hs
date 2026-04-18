data Val = IntVal Integer
   deriving (Show,Eq)
data Exp = IntExp Integer
         | IntOpExp String Exp Exp
         | VarExp String
         | LetExp String Val Exp
   deriving (Show,Eq)
type Env = [(String,Val)]
intOps = [ ("+",(+))
         , ("-",(-))
         , ("*",*))
         , ("/",div)]
liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = IntVal 0
eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
  in liftIntOp f v1 v2
eval (VarExp v) env =
  case lookup v env of
    Just vv -> v
    Nothing -> IntVal 0
eval (LetExp var e1 e2) env =
  let v1 = eval e1 env
   in eval e2 (var,v1):env
