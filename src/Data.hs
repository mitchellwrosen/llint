module Data where

import Data.Derive.UniplateDirect
import Language.Lua.Parser
import Language.Lua.Syntax

{-!
deriving instance UniplateDirect (Block NodeInfo)
!-}
