module Style where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Loc
import Lens.Micro
import Language.Lua.Parser
import Language.Lua.Syntax

type Warning = String

data StyleGuide = StyleGuide
    { styleGuideIndent :: Int
    }

type Styler a = ReaderT StyleGuide (Writer [Warning]) a

warn :: HasPos a => Warning -> a -> Styler ()
warn w p = tell [w <> ": " <> displayPos (getPos p)]

ok :: Styler ()
ok = pure ()

indent :: Styler Int
indent = asks styleGuideIndent

runStyler :: StyleGuide -> Styler a -> [Warning]
runStyler g s = snd $ runWriter (runReaderT s g)

class HasPos a where
    getPos :: a -> Pos

instance HasPos NodeInfo where
    getPos (NodeInfo (Loc pos _) _) = pos

instance HasPos Pos where
    getPos = id

-- Checks:
-- * There is at least one statement, including the optional return statement.
-- * All statements, including the optional return statement, begin at the same column.
styleCheckBlock :: Block NodeInfo -> Styler ()
styleCheckBlock (Block info [] Nothing) = warn "Empty block" info
styleCheckBlock (Block info [] (Just _)) = ok
styleCheckBlock (Block info (s:ss) mr) = do
    let Loc pos _ = nodeLoc (s^.ann)
    forM_ ss $ \s' -> do
        let Loc pos' _ = nodeLoc (s'^.ann)
        when (posCol pos /= posCol pos') $
            warn ("Statement should begin at column " ++ show (posCol pos)) pos'
    case mr of
        Just r -> do
            let Loc pos' _ = nodeLoc (r^.ann)
            when (posCol pos /= posCol pos') $
                warn ("Statement should begin at column " ++ show (posCol pos)) pos'
        Nothing -> ok

styleCheckStatement :: Statement NodeInfo -> Styler ()
-- Checks:
-- * TODO: Warn if length vs /= length es
-- * TODO: Warn if there is not one space around the '='
styleCheckStatement (Assign _ vs es) = ok
-- Checks:
-- * TODO: Warn if there is more than one space after 'goto'
styleCheckStatement (Goto _ _) = ok
-- Checks:
-- * The inner block begins one line down and one indent in from the 'do'
-- * TODO: 'end' lines up with 'do' and is one line after inner block
styleCheckStatement (Do info block) = do
    case block of
        Block _ [] Nothing -> ok -- don't bother style checking the "do end"
        _ -> do
            let Loc doPos    _ = nodeLoc info
                Loc blockPos _ = nodeLoc (block^.ann)
            i <- indent
            when (not $ posLine doPos + 1 == posLine blockPos &&
                        posCol  doPos + i == posCol  blockPos) $
                warn ("Do-block should be on a new line and indented " <> show i <> " spaces") blockPos
-- TODO: The rest.
styleCheckStatement _ = ok
