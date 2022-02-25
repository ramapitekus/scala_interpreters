import Control.Monad.State

data Exp
     = Num Int
     | Add Exp Exp
     | Stupid
     | Id String
     | Abs String Exp
     | App Exp Exp

data Typ
     = TNum
     | TStupid
     | TFun Typ Typ
     | TUniv String
     deriving (Show, Eq)

occurs x TNum = False
occurs x TStupid = False
occurs x (TFun t1 t2) = occurs x t1 || occurs x t2
occurs x (TUniv y) = x == y

subst :: Map String Typ -> Typ -> Typ
subst s TNum = TNum
subst s TStupid = TStupid
subst s (TFun t1 t2) = TFun (subst s t1) (subst s t2)
subst s (TUniv x) = case lookup x s of
      Nothing -> TUniv x
      Just t -> t

gensym :: State Int Typ
gensym = do
    i <- get
    put $ i + 1
    return $ TUniv $ "U_" ++ show i

data Con = TEq Typ Typ
         deriving (Show, Eq)

solve :: [Con] -> Map String Typ -> Map String Typ
solve [] sol = sol
solve (TEq (TUniv x) t : cs) sol
    | not (occurs x t)
    = case lookup x sol of
        Nothing -> solve cs (bind x t sol)
        Just t2 -> solve (TEq t2 t : cs) sol
solve (TEq t (TUniv x) : cs) sol
    | not (occurs x t)
    = solve (TEq (TUniv x) t : cs) sol
solve (TEq TNum TNum : cs) sol = solve cs sol
solve (TEq TStupid TStupid : cs) sol = solve cs sol
solve (TEq (TFun t1 t2) (TFun u1 u2) : cs) sol
    = solve (TEq t1 u1 : TEq t2 u2 : cs) sol
solve _ _ = error "Constraint system not solveable."

type Result = Either String Typ
yield t cs = Right (t, cs)
throw s = Left s

type Map a b = [(a,b)]
type Val = Int -- dummy definition

type Env = Map String Val -- run time
type Ctx = Map String Typ -- compile time

bind x t ctx = ((x,t):ctx)

typeof :: Ctx -> Exp -> State Int (Either String (Typ, [Con]))
typeof ctx (Num n) = return $ yield TNum []
typeof ctx (Add e1 e2) = do
    Right (t1, cs1) <- typeof ctx e1
    Right (t2, cs2) <- typeof ctx e2
    return $ yield TNum (TEq t1 TNum : TEq t2 TNum : cs1 ++ cs2)
typeof ctx Stupid = return $ yield TStupid []

typeof ctx (App e1 e2) = do
    Right (tfun, cs1) <- typeof ctx e1
    Right (targ, cs2) <- typeof ctx e2
    t2 <- gensym
    return $ yield t2 (TEq (TFun targ t2) tfun : cs1 ++ cs2)
typeof ctx (Abs x e) = do
    tuniv <- gensym
    Right (tres, cs) <- typeof (bind x tuniv ctx) e
    return $ yield (TFun tuniv tres) cs
typeof ctx (Id x) = case lookup x ctx of
    Nothing -> return $ throw $ "Undefined variable " ++ x
    Just t -> return $ yield t []

tcheck e = case runState (typeof [] e) 0 of
  (Right (t, cs), _) -> Right $ subst (solve cs []) t
  (Left err,_) -> Left err
