

data Exp0
    = Num0 Int
    | Add0 Exp0 Exp0
    | App0 Exp0 Exp0
    | Fun0 String Exp0
    | Id0 String

data Typ
    = TNum
    | TFun Typ Typ
    deriving (Eq, Show)

typeof0 :: Exp0 -> Typ
-- First let's do arithmetic expressions
typeof0 (Num0 _) = TNum
typeof0 (Add0 e1 e2) = TNum
    where TNum = typeof0 e1
          TNum = typeof0 e2
-- Now to functions.
typeof0 (App0 e1 e2) = if t1 == targ then t2 else undefined
-- Q: what to do when types don't match?
-- A: yield type error
    where TFun t1 t2 = typeof0 e1
          targ = typeof0 e2
typeof0 (Fun0 x e) = TFun undefined t
-- Q: what should we use as argument type?
-- A: require type annotation by the user
    where t = typeof0 e
typeof0 (Id0 x) = undefined
-- Q: what is the type of a variable reference? How to detect unbound variables?
-- A: need to keep track of bindings by simulating environment statically.



type Result = Either String Typ
yield t = Right t
throw s = Left s

-- To keep track of variable bindings, we need to simulate the environment
-- of our interpreter in the type checker. The environment maps variable
-- names to their value at run time:
type Val = Int -- Dummy definition
type Env = [(String, Val)]
-- At compile time, we don't know the actual values. But we can assert what
-- there types are. That is, for each variable, we keep track of the type of
-- values that the variable can refer to:
type Ctx = [(String, Typ)]
bind x t ctx = (x,t):ctx
-- This structure is called a typing context. Since we want to prevent errors
-- that might occur at run time during interpretation of the program, our type
-- checker needs to simulate the actions performed by the interpreter. That is,
-- whenever the interpreter changes the environment, our type checker must
-- perform a similar change to the typing context.

-- We require users to annotate the type of function parameters.
data Exp
    = Num Int
    | Add Exp Exp
    | App Exp Exp
    | Fun String Typ Exp
    | Id String


typeof :: Ctx -> Exp -> Result
typeof ctx (Num _) = yield TNum
typeof ctx (Add e1 e2) = do 
    TNum <- typeof ctx e1
    TNum <- typeof ctx e2
    yield TNum
typeof ctx (App e1 e2) = do
    TFun t1 t2 <- typeof ctx e1
    targ <- typeof ctx e2
    if t1 == targ
      then yield t2
      else throw $ "Mismatching argument type. Expected " ++ show t1 ++ " but was " ++ show targ
typeof ctx (Fun x t e) = do
    tbody <- typeof (bind x t ctx) e
    yield $ TFun t tbody
typeof ctx (Id x) =
    case lookup x ctx of
        Just t -> yield t
        Nothing -> throw $ "Unbound variable " ++ x
