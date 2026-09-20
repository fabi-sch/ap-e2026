module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String

type Refs = [VName]

newtype CheckM a = CheckM (Refs -> Either Error a)

instance Functor CheckM where
    -- fmap :: (a -> b) -> CheckM a -> CheckM b
    fmap f (CheckM x) = CheckM $ \refs -> 
        case x refs of
            (Right x') -> Right (f x')
            (Left err) -> Left err

instance Applicative CheckM where
    -- pure :: a -> CheckM a
    pure x = CheckM $ \refs ->
        (Right x)

    -- (<*>) :: CheckM (a -> b) -> CheckM a -> CheckM b
    CheckM f <*> CheckM x = CheckM $ \refs ->
        case f refs of
            (Left err) -> Left err
            (Right g) -> case x refs of
                (Left err) -> Left err
                (Right y) -> Right (g y)   

instance Monad CheckM where
    -- >>= :: CheckM a -> (a -> CheckM b) -> CheckM b
    CheckM x >>= f = CheckM $ \refs ->
        case x refs of
            (Left err) -> Left err
            (Right y) -> case f y of
                (CheckM g) -> g refs

askRefs :: CheckM Refs
askRefs = CheckM $ \refs -> Right refs

localRefs :: (Refs -> Refs) -> CheckM a -> CheckM a
localRefs f (CheckM x) = CheckM $ \refs -> x (f refs)

failure :: String -> CheckM a
failure s = CheckM $ \_refs -> (Left s)

refsExtend :: VName -> Refs -> Refs
refsExtend v refs = v : refs

checkMember :: VName -> Refs -> CheckM ()
checkMember v refs = CheckM $ \_ ->
    if v `elem` refs 
        then Right ()
        else Left ("Variable not in scope: " ++ v)

refsEmpty :: Refs
refsEmpty = []

check :: Exp -> CheckM ()
check (Let v e1 e2) = do
    check e1
    localRefs (refsExtend v) (check e2)
check (Lambda v e) = do
    localRefs (refsExtend v) (check e)
check (CstInt x) = pure ()
check (CstBool x) = pure ()
check (Add e1 e2) = do
    check e1
    check e2
check (Sub e1 e2) = do
    check e1
    check e2
check (Mul e1 e2) = do
    check e1
    check e2
check (Div e1 e2) = do
    check e1
    check e2
check (Pow e1 e2) = do
    check e1
    check e2
check (Eql e1 e2) = do
    check e1
    check e2
check (If cond e1 e2) = do
    check cond
    check e1
    check e2
check (Var v) = do
    refs <- askRefs
    (checkMember v) refs
check (ForLoop (loopparam, initial) (iv, bound) body) = do
    check initial
    check bound
    refs <- askRefs
    localRefs (refsExtend iv . refsExtend loopparam) (check body)
check (Apply e1 e2) = do
    check e1
    check e2
check (TryCatch e1 e2) = do
    check e1
    check e2
check (Print s e) = do
    check e
check (KvPut e1 e2) = do
    check e1
    check e2
check (KvGet e) = do
    check e

checkExp :: Exp -> Maybe Error
checkExp e =
    case check e of
        CheckM e' -> case e' refsEmpty of
            Right () -> Nothing
            Left err -> Just err
