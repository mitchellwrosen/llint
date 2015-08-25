module HasPos where

import Data.Loc
import Data.Sequence (Seq, ViewL(..), ViewR(..), viewl, viewr)
import Language.Lua.Parser
import Language.Lua.Syntax
import Language.Lua.Token
import Lens.Micro

-- Simple typeclass to abstract getting positions out of a structure. srcloc's
-- Loc is assumed never to NoLoc.
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
            EmptyL -> firstPos (info^.nodeLoc)
            l :< _ -> firstPos l

    firstEndPos info =
        case viewl (info^.nodeTokens) of
            EmptyL -> firstEndPos (info^.nodeLoc)
            l :< _ -> firstEndPos l

    lastPos info =
        case viewr (info^.nodeTokens) of
            EmptyR -> lastPos (info^.nodeLoc)
            _ :> l -> lastPos l

    lastStartPos info =
        case viewr (info^.nodeTokens) of
            EmptyR -> lastStartPos (info^.nodeLoc)
            _ :> l -> lastStartPos l

-- A Seq's position functions assume there is at least one element.
instance HasPos (Seq (L Token)) where
    firstPos     (viewl -> l :< _) = firstPos     l
    firstEndPos  (viewl -> l :< _) = firstEndPos  l
    lastPos      (viewr -> _ :> l) = lastPos      l
    lastStartPos (viewr -> _ :> l) = lastStartPos l

instance Annotated f => HasPos (f NodeInfo) where
    firstPos         = firstPos     . (^.ann)
    firstEndPos      = firstEndPos  . (^.ann)
    lastPos          = lastPos      . (^.ann)
    lastStartPos     = lastStartPos . (^.ann)

instance HasPos Loc where
    firstPos     (Loc pos _) = pos
    firstEndPos  (Loc _ pos) = pos
    lastPos      (Loc _ pos) = pos
    lastStartPos (Loc pos _) = pos

instance HasPos (L a) where
    firstPos     (L loc _) = firstPos     loc
    firstEndPos  (L loc _) = firstEndPos  loc
    lastPos      (L loc _) = lastPos      loc
    lastStartPos (L loc _) = lastStartPos loc

instance HasPos Pos where
    firstPos     = id
    firstEndPos  = id
    lastPos      = id
    lastStartPos = id
