module Trav
    ( Callbacks(..)
    , traverseLuaChunk
    ) where

import Control.Monad.Reader
import Data.Foldable
import Language.Lua.Parser
import Language.Lua.Syntax

data Callbacks m = Callbacks
    { onIdent            :: Ident            NodeInfo -> m ()
    , onIdentList        :: IdentList        NodeInfo -> m ()
    , onIdentList1       :: IdentList1       NodeInfo -> m ()
    , onBlock            :: Block            NodeInfo -> m ()
    , onStatement        :: Statement        NodeInfo -> m ()
    , onReturnStatement  :: ReturnStatement  NodeInfo -> m ()
    , onFunctionName     :: FunctionName     NodeInfo -> m ()
    , onVariable         :: Variable         NodeInfo -> m ()
    , onVariableList1    :: VariableList1    NodeInfo -> m ()
    , onExpression       :: Expression       NodeInfo -> m ()
    , onExpressionList   :: ExpressionList   NodeInfo -> m ()
    , onExpressionList1  :: ExpressionList1  NodeInfo -> m ()
    , onPrefixExpression :: PrefixExpression NodeInfo -> m ()
    , onFunctionCall     :: FunctionCall     NodeInfo -> m ()
    , onFunctionArgs     :: FunctionArgs     NodeInfo -> m ()
    , onFunctionBody     :: FunctionBody     NodeInfo -> m ()
    , onTableConstructor :: TableConstructor NodeInfo -> m ()
    , onField            :: Field            NodeInfo -> m ()
    , onFieldList        :: FieldList        NodeInfo -> m ()
    , onBinop            :: Binop            NodeInfo -> m ()
    , onUnop             :: Unop             NodeInfo -> m ()
    }

type Trav m a = ReaderT (Callbacks m) m a

traverseLuaChunk :: Monad m => Callbacks m -> Chunk NodeInfo -> m ()
traverseLuaChunk cbs b = runReaderT (travBlock b) cbs

travIdent :: Monad m => Ident NodeInfo -> Trav m ()
travIdent i = asks onIdent >>= \f -> lift (f i)

travIdentList :: Monad m => IdentList NodeInfo -> Trav m ()
travIdentList is = do
    asks onIdentList >>= \f -> lift (f is)
    case is of
        IdentList _ is' -> traverse_ travIdent is'

travIdentList1 :: Monad m => IdentList1 NodeInfo -> Trav m ()
travIdentList1 is = do
    asks onIdentList1 >>= \f -> lift (f is)
    case is of
        IdentList1 _ is' -> traverse_ travIdent is'

travBlock :: Monad m => Block NodeInfo -> Trav m ()
travBlock block@(Block _ ss mr) = do
    asks onBlock >>= \f -> lift (f block)
    traverse_ travStatement ss
    traverse_ travReturnStatement mr

travStatement :: Monad m => Statement NodeInfo -> Trav m ()
travStatement stmt = do
    asks onStatement >>= \f -> lift (f stmt)
    case stmt of
        EmptyStmt _ -> pure ()
        Assign _ vs es -> do
            travVariableList1 vs
            travExpressionList1 es
        FunCall _ f -> travFunctionCall f
        Label _ i -> travIdent i
        Break _ -> pure ()
        Goto _ i -> travIdent i
        Do _ b -> travBlock b
        While _ e b -> do
            travExpression e
            travBlock b
        Repeat _ b e -> do
            travBlock b
            travExpression e
        If _ es mb -> do
            traverse_ (\(e, b) -> do
                travExpression e
                travBlock b) es
            traverse_ travBlock mb
        For _ i e1 e2 me3 b -> do
            travIdent i
            travExpression e1
            travExpression e2
            traverse_ travExpression me3
            travBlock b
        ForIn _ is es b -> do
            travIdentList1 is
            travExpressionList1 es
            travBlock b
        FunAssign _ fn fb -> do
            travFunctionName fn
            travFunctionBody fb
        LocalFunAssign _ i fb -> do
            travIdent i
            travFunctionBody fb
        LocalAssign _ is es -> do
            travIdentList1 is
            travExpressionList es

travReturnStatement :: Monad m => ReturnStatement NodeInfo -> Trav m ()
travReturnStatement r = do
    asks onReturnStatement >>= \f -> lift (f r)
    case r of
        ReturnStatement _ es -> travExpressionList es

travFunctionName :: Monad m => FunctionName NodeInfo -> Trav m ()
travFunctionName fn = do
    asks onFunctionName >>= \f -> lift (f fn)
    case fn of
        FunctionName _ is mi -> do
            travIdentList1 is
            traverse_ travIdent mi

travVariable :: Monad m => Variable NodeInfo -> Trav m ()
travVariable v = do
    asks onVariable >>= \f -> lift (f v)
    case v of
        VarIdent _ i -> travIdent i
        VarField _ pe e -> do
            travPrefixExpression pe
            travExpression e
        VarFieldName _ pe i -> do
            travPrefixExpression pe
            travIdent i

travVariableList1 :: Monad m => VariableList1 NodeInfo -> Trav m ()
travVariableList1 vs = do
    asks onVariableList1 >>= \f -> lift (f vs)
    case vs of
        VariableList1 _ vs' -> traverse_ travVariable vs'

travExpression :: Monad m => Expression NodeInfo -> Trav m ()
travExpression e = do
    asks onExpression >>= \f -> lift (f e)
    case e of
        Nil _ -> pure ()
        Bool _ _ -> pure ()
        Integer _ _ -> pure ()
        Float _ _ -> pure ()
        String _ _ -> pure ()
        Vararg _ -> pure ()
        FunDef _ fb -> travFunctionBody fb
        PrefixExp _ pe -> travPrefixExpression pe
        TableCtor _ tc -> travTableConstructor tc
        Binop _ _ e1 e2 -> do
            travExpression e1
            travExpression e2
        Unop _ _ e1 -> travExpression e1

travExpressionList :: Monad m => ExpressionList NodeInfo -> Trav m ()
travExpressionList es = do
    asks onExpressionList >>= \f -> lift (f es)
    case es of
        ExpressionList _ es' -> traverse_ travExpression es'

travExpressionList1 :: Monad m => ExpressionList1 NodeInfo -> Trav m ()
travExpressionList1 es = do
    asks onExpressionList1 >>= \f -> lift (f es)
    case es of
        ExpressionList1 _ es' -> traverse_ travExpression es'

travPrefixExpression :: Monad m => PrefixExpression NodeInfo -> Trav m ()
travPrefixExpression pe = do
    asks onPrefixExpression >>= \f -> lift (f pe)
    case pe of
        PrefixVar _ v -> travVariable v
        PrefixFunCall _ fc -> travFunctionCall fc
        Parens _ e -> travExpression e

travFunctionCall :: Monad m => FunctionCall NodeInfo -> Trav m ()
travFunctionCall fc = do
    asks onFunctionCall >>= \f -> lift (f fc)
    case fc of
        FunctionCall _ pe fa -> do
            travPrefixExpression pe
            travFunctionArgs fa
        MethodCall _ pe i fa -> do
            travPrefixExpression pe
            travIdent i
            travFunctionArgs fa

travFunctionArgs :: Monad m => FunctionArgs NodeInfo -> Trav m ()
travFunctionArgs fa = do
    asks onFunctionArgs >>= \f -> lift (f fa)
    case fa of
        Args _ es -> travExpressionList es
        ArgsTable _ tc -> travTableConstructor tc
        ArgsString _ _ -> pure ()

travFunctionBody :: Monad m => FunctionBody NodeInfo -> Trav m ()
travFunctionBody fb = do
    asks onFunctionBody >>= \f -> lift (f fb)
    case fb of
        FunctionBody _ is _ b -> do
            travIdentList is
            travBlock b

travTableConstructor :: Monad m => TableConstructor NodeInfo -> Trav m ()
travTableConstructor tc = do
    asks onTableConstructor >>= \f -> lift (f tc)
    case tc of
        TableConstructor _ fs -> travFieldList fs

travField :: Monad m => Field NodeInfo -> Trav m ()
travField field = do
    asks onField >>= \f -> lift (f field)
    case field of
        FieldExp _ e1 e2 -> do
            travExpression e1
            travExpression e2
        FieldIdent _ i e -> do
            travIdent i
            travExpression e
        Field _ e -> travExpression e

travFieldList :: Monad m => FieldList NodeInfo -> Trav m ()
travFieldList fs = do
    asks onFieldList >>= \f -> lift (f fs)
    case fs of
        FieldList _ fs' -> traverse_ travField fs'
