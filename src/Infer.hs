module Infer where

import           Control.Monad.Except
import           Control.Monad.RWS
import qualified Data.List.NonEmpty         as NE
import           Data.Map (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Set (Set, (\\))
import qualified Data.Set                   as S
import           Language.Lua.Syntax

--------------------------------------------------------------------------------
-- Type stuff

type TVar = String

type Var = String

data Type
    = TVar TVar
    | TFun [Type] Type
    | TMany [Type]
    | TFloat
    | TBool
    | TString
    | TNil
    deriving (Eq, Ord, Show)

data Scheme = Forall (Set TVar) Type
    deriving (Eq, Ord, Show)

type TypeEnv = Map Var Scheme

data TypeError
    = InfiniteType TVar Type
    | UnificationFail Type Type
    | UnboundVariable String
    deriving Show

--------------------------------------------------------------------------------
-- Substitutable

newtype Subst = Subst (Map TVar Type)
    deriving (Eq, Show)

instance Monoid Subst where
    mempty = Subst mempty
    s1 `mappend` s2@(Subst m2) = Subst (fmap (apply s1) m2) <> s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set TVar

instance Substitutable Type where
    apply (Subst m) t@(TVar v) = M.findWithDefault t v m
    apply s (TFun ts t)        = TFun (apply s ts) (apply s t)
    apply s (TMany ts)         = TMany (apply s ts)
    apply _ t                  = t

    ftv (TVar t)    = [t]
    ftv (TFun ts t) = ftv ts <> ftv t
    ftv (TMany ts)  = ftv ts
    ftv _           = mempty

instance Substitutable Scheme where
    apply (Subst m) (Forall vs t) = Forall vs (apply (Subst (foldr M.delete m vs)) t)
    ftv (Forall vs t) = ftv t \\ vs

instance Substitutable TypeEnv where
    apply = fmap . apply
    ftv = ftv . M.elems

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldMap ftv

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    apply s (t1, t2) = (apply s t1, apply s t2)
    ftv (t1, t2) = ftv t1 <> ftv t2

--------------------------------------------------------------------------------
-- Inference

type Infer a = RWST TypeEnv [Constraint] NameSupply (Either TypeError) a

type NameSupply = [String]

nameSupply :: NameSupply
nameSupply = [1..] >>= flip replicateM ['a'..'z']

type Constraint = (Type, Type)

runInfer :: Infer a -> Either TypeError (a, [Constraint])
runInfer m = evalRWST m mempty nameSupply

unify :: Type -> Type -> Infer ()
unify t1 t2 = tell [(t1, t2)]

freshType :: Infer Type
freshType = do
    (s:ss) <- get
    put ss
    pure (TVar s)

instantiate :: Scheme -> Infer Type
instantiate (Forall (S.toList -> vs) t) = do
    vs' <- mapM (const freshType) vs
    pure $ apply (Subst (M.fromList (zip vs vs'))) t

generalize :: Type -> Infer Scheme
generalize t = do
    env <- ask
    pure $ Forall (ftv t \\ ftv env) t

infer :: TypeEnv -> Block a -> Either TypeError Scheme
infer env block = do
    (t, cs) <- runInfer (inferBlock block)
    s <- constraintSolver mempty cs
    pure . closeOver $ apply s t

inferBlock :: Block a -> Infer Type
inferBlock (Block _ stmts mr) = do
    f <- go stmts
    case mr of
        Nothing -> pure TNil
        Just (ReturnStatement _ (ExpressionList _ es)) ->
            case es of
                []  -> pure TNil
                [e] -> local f (inferExpr e)
                _   -> TMany . mapInit adjustManyToOne <$> mapM (local f . inferExpr) es
  where
    go :: [Statement a] -> Infer (TypeEnv -> TypeEnv)
    go [] = pure id
    go (s:ss) = do
        f <- inferStmt s
        f' <- local f (go ss)
        pure $ f' . f

inferStmt :: Statement a -> Infer (TypeEnv -> TypeEnv)
inferStmt (EmptyStmt _) = pure id
inferStmt (Assign _ (VariableList1 _ vs) (ExpressionList1 _ es)) = go (NE.toList vs) (NE.toList es)
  where
    go :: [Variable a] -> [Expression a] -> Infer (TypeEnv -> TypeEnv)
    go [] es = id <$ mapM_ inferExpr es
    go vs [e] = do
        t <- inferExpr e
        graft t vs
    go (v:vs) (e:es) =
        case v of
            VarIdent _ (Ident _ x) -> do
                t <- adjustManyToOne <$> inferExpr e
                f <- M.insert x <$> generalize t
                f' <- go vs es
                pure $ f' . f
            _ -> do
                _ <- inferExpr e
                go vs es

    graft :: Type -> [Variable a] -> Infer (TypeEnv -> TypeEnv)
    graft _ [] = pure id
    graft (TMany (t:ts)) (v:vs) =
        case v of
            VarIdent _ (Ident _ x) -> do
                f <- M.insert x <$> generalize t
                f' <- graft (TMany ts) vs
                pure $ f' . f
            _ -> graft (TMany ts) vs
    graft (TMany []) vs = allNil vs
    graft t (v:vs) = do
        f <- case v of
                 VarIdent _ (Ident _ x) -> M.insert x <$> generalize t
                 _ -> pure id
        f' <- allNil vs
        pure $ f' . f

    allNil :: [Variable a] -> Infer (TypeEnv -> TypeEnv)
    allNil vs = allNil' [x | VarIdent _ (Ident _ x) <- vs]
      where
        allNil' :: [Var] -> Infer (TypeEnv -> TypeEnv)
        allNil' = pure . foldr (\x f -> f . M.insert x (Forall mempty TNil)) id
inferStmt _ = error "inferStmt: TODO"

inferExpr :: Expression a -> Infer Type
inferExpr (Nil _) = freshType
inferExpr (Bool _ _) = pure TBool
inferExpr (Integer _ _) = pure TFloat
inferExpr (Float _ _) = pure TFloat
inferExpr (String _ _) = pure TString
inferExpr (PrefixExp _ (PrefixVar _ (VarIdent _ (Ident _ x)))) = lookupEnv x
inferExpr _ = error "inferExpr: TODO"

lookupEnv :: Var -> Infer Type
lookupEnv x = do
    env <- ask
    case M.lookup x env of
        Nothing -> throwError $ UnboundVariable (show x)
        Just s -> instantiate s

closeOver :: Type -> Scheme
closeOver t = Forall (S.fromList $ M.elems m) (f t)
  where
    m :: Map TVar TVar
    m = M.fromList $ zip (S.toList $ ftv t) nameSupply

    f :: Type -> Type
    f (TVar v)    = TVar (m M.! v)
    f (TFun ts t) = TFun (map f ts) (f t)
    f (TMany ts)  = TMany (map f ts)
    f t           = t

-- In many contexts, multiple return values are adjusted to only the first.
adjustManyToOne :: Type -> Type
adjustManyToOne (TMany (t:_)) = t
adjustManyToOne t = t

--------------------------------------------------------------------------------
-- Constraint solving

type Solve a = Either TypeError a

constraintSolver :: Subst -> [Constraint] -> Solve Subst
constraintSolver s [] = pure s
constraintSolver s ((t1,t2):cs) = do
    s' <- unifies t1 t2
    constraintSolver (s' <> s) (apply s' cs)

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = pure mempty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TFun xs x) (TFun ys y)
    | length xs == length ys = unifiesList (xs ++ [x]) (ys ++ [y])
unifies (TMany xs) (TMany ys)
    | length xs == length ys = unifiesList xs ys
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Precondition: lists are same length
unifiesList :: [Type] -> [Type] -> Solve Subst
unifiesList [] [] = pure mempty
unifiesList (x:xs) (y:ys) = do
    s1 <- unifies x y
    s2 <- unifiesList (apply s1 xs) (apply s1 ys)
    pure $ s2 <> s1
unifiesList _ _ = error "unifiesList: precondition violated"

bind :: TVar -> Type -> Solve Subst
bind v t | occursCheck v t = throwError $ InfiniteType v t
         | otherwise = pure $ Subst (M.singleton v t)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck v s = v `S.member` ftv s

--------------------------------------------------------------------------------
-- Misc. extras

mapInit :: (a -> a) -> [a] -> [a]
mapInit _ []     = error "mapInit: empty list"
mapInit f [x]    = [x]
mapInit f (x:xs) = f x : mapInit f xs
