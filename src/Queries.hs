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

allVariables :: Data a => Chunk a -> [Variable a]
allVariables = universeBi
