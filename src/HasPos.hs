module HasPos where

import SrcLoc
import Data.Sequence (Seq, ViewL(..), ViewR(..), viewl, viewr)
import Language.Lua.Parser
import Language.Lua.Syntax
import Language.Lua.Token
import Lens.Micro

-- Simple typeclass to abstract getting positions out of a structure.
class HasPos a where
    firstPos     :: a -> Pos -- Start of the first pos (the "very first" pos).
    firstEndPos  :: a -> Pos -- End of the first pos.
    lastPos      :: a -> Pos -- End of the last pos (the "very last" pos).
    lastStartPos :: a -> Pos -- Start of the last pos.

-- The positions of a NodeInfo are that of its tokens, except in the case that
-- it has no tokens (e.g. an empty block), in which case its own Loc is is used.
instance HasPos NodeInfo where
    firstPos info =
        case viewl (info^.nodeTokens) of
            EmptyL       -> let Loc pos _ = info^.nodeLoc in pos
            L loc _ :< _ -> let Loc pos _ = loc in pos

    firstEndPos info =
        case viewl (info^.nodeTokens) of
            EmptyL       -> let Loc _ pos = info^.nodeLoc in pos
            L loc _ :< _ -> let Loc _ pos = loc in pos

    lastPos info =
        case viewr (info^.nodeTokens) of
            EmptyR       -> let Loc _ pos = info^.nodeLoc in pos
            _ :> L loc _ -> let Loc _ pos = loc in pos

    lastStartPos info =
        case viewr (info^.nodeTokens) of
            EmptyR       -> let Loc pos _ = info^.nodeLoc in pos
            _ :> L loc _ -> let Loc pos _ = loc in pos

-- A Seq's position functions assume there is at least one element.
instance HasPos (Seq (L Token)) where
    firstPos     (viewl -> L (Loc pos _) _ :< _) = pos
    firstEndPos  (viewl -> L (Loc _ pos) _ :< _) = pos
    lastPos      (viewr -> _ :> L (Loc _ pos) _) = pos
    lastStartPos (viewr -> _ :> L (Loc pos _) _) = pos

instance HasPos Pos where
    firstPos     = id
    firstEndPos  = id
    lastPos      = id
    lastStartPos = id

instance Annotated f => HasPos (f NodeInfo) where
    firstPos         = firstPos     . (^.ann)
    firstEndPos      = firstEndPos  . (^.ann)
    lastPos          = lastPos      . (^.ann)
    lastStartPos     = lastStartPos . (^.ann)
