-- | Uniplate queries on a Lua Chunk. Not strictly necessary, but improves code
-- readability.

module Queries where

import Data.Data
import Data.Generics.Uniplate.Data
import Language.Lua.Parser
import Language.Lua.Syntax

allBlocks :: Data a => Chunk a -> [Block a]
allBlocks = universe

allStatements :: Data a => Chunk a -> [Statement a]
allStatements = universeBi

allReturnStatements :: Data a => Chunk a -> [ReturnStatement a]
allReturnStatements = universeBi

allFunctionNames :: Data a => Chunk a -> [FunctionName a]
allFunctionNames = universeBi

allVariables :: Data a => Chunk a -> [Variable a]
allVariables = universeBi

allVariableList1s :: Data a => Chunk a -> [VariableList1 a]
allVariableList1s = universeBi

allExpressions :: Data a => Chunk a -> [Expression a]
allExpressions = universeBi

allExpressionLists :: Data a => Chunk a -> [ExpressionList a]
allExpressionLists = universeBi

allExpressionList1s :: Data a => Chunk a -> [ExpressionList1 a]
allExpressionList1s = universeBi

allPrefixExpressions :: Data a => Chunk a -> [PrefixExpression a]
allPrefixExpressions = universeBi

allFunctionCalls :: Data a => Chunk a -> [FunctionCall a]
allFunctionCalls = universeBi

allFunctionArgs :: Data a => Chunk a -> [FunctionArgs a]
allFunctionArgs = universeBi

allFunctionBodies :: Data a => Chunk a -> [FunctionBody a]
allFunctionBodies = universeBi

allTableConstructors :: Data a => Chunk a -> [TableConstructor a]
allTableConstructors = universeBi

allFields :: Data a => Chunk a -> [Field a]
allFields = universeBi

allFieldLists :: Data a => Chunk a -> [FieldList a]
allFieldLists = universeBi

allBinops :: Data a => Chunk a -> [Binop a]
allBinops = universeBi

allUnops :: Data a => Chunk a -> [Unop a]
allUnops = universeBi
