module Infer where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.List.NonEmpty         as NE
import           Data.Map (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Set (Set, (\\))
import qualified Data.Set                   as S
import           Language.Lua.Syntax
import           Lens.Micro

import Language.Lua.Parser

-- Quick-and-dirty block inference
debugInfer :: FilePath -> IO (Either TypeError Scheme)
debugInfer = fmap (infer mempty . parseLua "") . readFile

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

type Subst = Map TVar Type

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = fmap (apply s1) s2 <> s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set TVar

instance Substitutable Type where
    apply m t@(TVar v)  = M.findWithDefault t v m
    apply s (TFun ts t) = TFun (apply s ts) (apply s t)
    apply s (TMany ts)  = TMany (apply s ts)
    apply _ t           = t

    ftv (TVar t)    = [t]
    ftv (TFun ts t) = ftv ts <> ftv t
    ftv (TMany ts)  = ftv ts
    ftv _           = mempty

instance Substitutable Scheme where
    apply m (Forall vs t) = Forall vs (apply (foldr M.delete m vs) t)
    ftv (Forall vs t)     = ftv t \\ vs

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

class MonadError TypeError m => MonadInfer m where
    freshType :: m Type
    getEnv :: m TypeEnv
    unify :: Type -> Type -> m ()

-- Expression inference monad.
-- * Read-only type environment
-- * Writes constraints to be solved
-- * Fresh type variable supply
-- * Can throw type errors
newtype InferExpr a = InferExpr { unInferExpr :: RWST TypeEnv [Constraint] TVarSupply (Either TypeError) a }
  deriving (Functor, Applicative, Monad, MonadReader TypeEnv, MonadWriter [Constraint], MonadState TVarSupply, MonadError TypeError)

instance MonadInfer InferExpr where
    freshType = do
        (s:ss) <- get
        put ss
        pure (TVar s)

    getEnv = ask

    unify t1 t2 = tell [(t1, t2)]

runInferExpr :: InferExpr a -> Either TypeError (a, [Constraint])
runInferExpr m = evalRWST (unInferExpr m) mempty tvarSupply

-- Block inference monad.
-- * Read/write global type environment
-- * Read/write stack of local type environments
-- * Writes constraints to be solved
-- * Writes all returned types seen so far in the block
-- * Fresh type variable supply
-- * Can throw type errors
newtype InferBlock a = InferBlock { unInferBlock :: WriterT ([Constraint], [Type]) (StateT InferBlockState (Either TypeError)) a }
  deriving (Functor, Applicative, Monad, MonadWriter ([Constraint], [Type]), MonadState InferBlockState, MonadError TypeError)

instance MonadInfer InferBlock where
    freshType = do
        InferBlockState localEnvs globalEnv (s:ss) <- get
        put $ InferBlockState localEnvs globalEnv ss
        pure (TVar s)

    -- Left-biased type environment due to shadowing.
    getEnv = do
        InferBlockState locals globals _ <- get
        pure $ mconcat locals <> globals

    unify t1 t2 = tell ([(t1, t2)], [])

data InferBlockState = InferBlockState
    { inferBlockLocalEnvs  :: [TypeEnv]
    , inferBlockGlobalEnv  :: TypeEnv
    , inferBlockTVarSupply :: TVarSupply
    }

runInferBlock :: InferBlock a -> Either TypeError (a, [Constraint])
runInferBlock m = (\((a, (cs, _)), _) -> (a, cs)) <$> runStateT (runWriterT (unInferBlock m)) initState
  where
    initState :: InferBlockState
    initState = InferBlockState [] mempty tvarSupply

returnsType :: Type -> InferBlock ()
returnsType t = tell ([], [t])

type TVarSupply = [TVar]

tvarSupply :: TVarSupply
tvarSupply = [1..] >>= flip replicateM ['a'..'z']

type Constraint = (Type, Type)

instantiate :: MonadInfer m => Scheme -> m Type
instantiate (Forall (S.toList -> vs) t) = do
    vs' <- mapM (const freshType) vs
    pure $ apply (M.fromList (zip vs vs')) t

generalize :: MonadInfer m => Type -> m Scheme
generalize t = do
    env <- getEnv
    pure $ Forall (ftv t \\ ftv env) t

infer :: TypeEnv -> Block a -> Either TypeError Scheme
infer env block = do
    (t, cs) <- runInferBlock (inferBlock block)
    s <- constraintSolver mempty cs
    pure . closeOver $ apply s t

inferBlock :: Block a -> InferBlock Type
inferBlock (Block _ ss mr) = do
    (_, (_, ts)) <- listen (mapM_ inferStmt ss)
    case mr of
        Nothing -> go ts TNil
        Just (ReturnStatement _ (ExpressionList _ es)) ->
            case es of
                [] -> go ts TNil
                [e] -> inferExpr e >>= go ts
                -- Multiple return statements, as in
                --
                --     return x, y, z, f(), g()
                --
                -- Create a TMany with each of their inferred types, but adjust
                -- each type to one, except the last expression's type.
                _ -> mapM inferExpr es >>= go ts . TMany . mapInit adjustManyToOne
  where
    -- Given all types of return statements that occurred in this block,
    -- and the type of the final return statement, unify each pair of types
    -- and return the unified type.
    go :: [Type] -> Type -> InferBlock Type
    go ts t = do
        returnsType t
        tv <- freshType
        mapM_ (uncurry unify) (pairs (tv:t:ts))
        pure tv

    pairs :: [a] -> [(a, a)]
    pairs []     = []
    pairs [x]    = []
    pairs (x:xs) = map (x,) xs ++ pairs xs

    -- Map over the init of a list (every element but the last)
    mapInit :: (a -> a) -> [a] -> [a]
    mapInit _ []     = error "mapInit: empty list"
    mapInit f [x]    = [x]
    mapInit f (x:xs) = f x : mapInit f xs

inferStmt :: Statement a -> InferBlock ()
inferStmt (EmptyStmt _) = pure ()
inferStmt (Assign _ (VariableList1 _ vs) (ExpressionList1 _ es)) = do
    newVars <- go (NE.toList vs) (NE.toList es)
    localEnvs <- gets inferBlockLocalEnvs
    forM_ newVars $ \(v, t) -> do
        (localEnvs', wasInserted) <- insertLocal v t localEnvs
        if wasInserted
            then modify (\(InferBlockState _ y z) -> InferBlockState localEnvs' y z)
            else do
                globalEnv <- gets inferBlockGlobalEnv
                case M.lookup v globalEnv of
                    Just s -> do
                        t' <- instantiate s
                        tv <- freshType
                        unify t tv
                        unify t' tv
                        let globalEnv' = M.insert v (Forall [] tv) globalEnv
                        modify (\(InferBlockState x _ z) -> InferBlockState x globalEnv' z)
                    Nothing -> do
                        s <- generalize t
                        let globalEnv' = M.insert v s globalEnv
                        modify (\(InferBlockState x _ z) -> InferBlockState x globalEnv' z)
  where
    -- Perform type inference on the expressions and build up a list of
    -- inferrred types to apply to the appropriate environment after the fact.
    go :: [Variable a] -> [Expression a] -> InferBlock [(Var, Type)]
    -- We ran out of variables to assign to, as in the statement
    --
    --     x, y = f(), g(), h()
    --
    -- Simply infer the types of the unused expressions (we still want to write
    -- constraints and throw type errors), and toss the results.
    go [] es = [] <$ mapM_ inferExpr es
    -- One expression is being assigned to one or more variables. Graft the
    -- result type over the list.
    go vs [e] = do
        t <- inferExpr e
        pure (graft t vs)
    -- More than one expression is being assigned to one or more variables;
    -- therefore, if the expression returns multiple values, it gets adjusted
    -- to only return the first.
    go (v:vs) (e:es) =
        case v of
            VarIdent _ (Ident _ x) -> do
                t <- adjustManyToOne <$> inferExpr e
                ((x, t) :) <$> go vs es
            -- TODO: Other variables
            _ -> do
                _ <- inferExpr e
                go vs es

    -- Graft a single type over multiple variables, as in
    --
    --     x, y, z = a
    --
    -- or
    --
    --     x, y, z = f()
    --
    -- When the type is a TMany, assign types to variables left to right, until
    -- the types run out. All remaining variables are nil.
    --
    -- When the type is not a TMany, assign it to the first variable. All
    -- remaining variables are nil.
    graft :: Type -> [Variable a] -> [(Var, Type)]
    -- We ran out of variables, as in
    --
    --     x, y = f()
    --
    -- where f() returns three values. Just toss the remaining types.
    graft _ [] = []
    graft (TMany (t:ts)) (v:vs) =
        case v of
            VarIdent _ (Ident _ x) -> (x, t) : graft (TMany ts) vs
            -- TODO: Other variables
            _ -> graft (TMany ts) vs
    -- We ran out of types to assign, as in
    --
    --     x, y, z = f()
    --
    -- where f() returns two values. All remaining variables are nil.
    graft (TMany []) vs = allNil vs
    -- We have a single type to assign to a list of variables, as in
    --
    --     x, y, z = 5
    --
    -- Assign the type to the first. All remaining variables are nil.
    graft t (v:vs) = do
        case v of
            VarIdent _ (Ident _ x) -> (x, t) : allNil vs
            -- TODO: Other variables
            _ -> allNil vs

    allNil :: [Variable a] -> [(Var, Type)]
    allNil vs = map (, TNil) [x | VarIdent _ (Ident _ x) <- vs] -- TODO: Other variables

    -- Look for the given variable in all local environments; if found,
    -- unify with the existing type and re-insert. Returns the (possibly)
    -- modified stack of local environments, and whether or not any
    -- modification took place.
    insertLocal :: Var -> Type -> [TypeEnv] -> InferBlock ([TypeEnv], Bool)
    insertLocal = insertLocal' []
      where
        insertLocal' :: [TypeEnv] -> Var -> Type -> [TypeEnv] -> InferBlock ([TypeEnv], Bool)
        insertLocal' acc _ _ [] = pure (reverse acc, False)
        insertLocal' acc v t (env:envs) = do
            case M.lookup v env of
                Just s -> do
                    t' <- instantiate s
                    tv <- freshType
                    unify t tv
                    unify t' tv
                    let env' = M.insert v (Forall [] tv) env
                    pure (reverse acc ++ [env'] ++ envs, True)
                Nothing -> insertLocal' (env:acc) v t envs
inferStmt _ = error "inferStmt: TODO"

inferExpr :: MonadInfer m => Expression a -> m Type
inferExpr (Nil _) = pure TNil
inferExpr (Bool _ _) = pure TBool
inferExpr (Integer _ _) = pure TFloat
inferExpr (Float _ _) = pure TFloat
inferExpr (String _ _) = pure TString
inferExpr (PrefixExp _ (PrefixVar _ (VarIdent _ (Ident _ x)))) = lookupEnv x
inferExpr _ = error "inferExpr: TODO"

lookupEnv :: MonadInfer m => Var -> m Type
lookupEnv x = do
    env <- getEnv
    case M.lookup x env of
        Nothing -> throwError $ UnboundVariable (show x)
        Just s -> instantiate s

closeOver :: Type -> Scheme
closeOver t = Forall (S.fromList $ M.elems m) (f t)
  where
    m :: Map TVar TVar
    m = M.fromList $ zip (S.toList $ ftv t) tvarSupply

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
    constraintSolver (s' `composeSubst` s) (apply s' cs)

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
    pure $ s2 `composeSubst` s1
unifiesList _ _ = error "unifiesList: precondition violated"

bind :: TVar -> Type -> Solve Subst
bind v t | occursCheck v t = throwError $ InfiniteType v t
         | otherwise = pure (M.singleton v t)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck v s = v `S.member` ftv s

--------------------------------------------------------------------------------
-- Misc. extras

infixl 4 %=
(%=) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
l %= f = modify (l %~ f)
