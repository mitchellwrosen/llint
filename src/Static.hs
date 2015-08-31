-- | Static analysis of Lua code; each AST node is analyzed in isolation.

module Static where

import HasPos

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Loc
import           Data.Sequence        (Seq, ViewL(..), ViewR(..), viewl, viewr)
import qualified Data.Sequence        as Seq
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Lens.Micro
import           Language.Lua.Parser
import           Language.Lua.Syntax
import           Language.Lua.Token

-- | A suggestion to be output.
data Suggestion = Suggestion Severity Text

showSuggestion :: Suggestion -> Text
showSuggestion (Suggestion s t) = showSeverity s <> " | " <> t

-- | Message severity.
data Severity = Style | Warning | Error
  deriving (Eq, Ord)

showSeverity :: Severity -> Text
showSeverity Style   = "Style  "
showSeverity Warning = "Warning"
showSeverity Error   = "Error  "

data StyleGuide = StyleGuide
    { styleGuideIndent :: Int
    }

-- | Static analysis monad. It has a style guide environment and outputs
-- suggestions.
type Static a = ReaderT StyleGuide (Writer (Seq Suggestion)) a

runStatic :: StyleGuide -> Static a -> Seq Suggestion
runStatic g s = snd $ runWriter (runReaderT s g)

style :: HasPos a => Text -> a -> Static ()
style = suggest Style

warn :: HasPos a => Text -> a -> Static ()
warn = suggest Warning

err :: HasPos a => Text -> a -> Static ()
err = suggest Error

suggest :: HasPos a => Severity -> Text -> a -> Static ()
suggest s t p = tell [Suggestion s $ t <> ": " <> pshow p]
  where
    pshow :: HasPos a => a -> Text
    pshow = T.pack . displayPos . firstPos

ok :: Static ()
ok = pure ()

indent :: Static Int
indent = asks styleGuideIndent

--------------------------------------------------------------------------------
-- Static checks

-- Checks:
-- * WARNING if there are no statements
-- * STYLE if all statements do not begin at the same column.
staticBlock :: Block NodeInfo -> Static ()
staticBlock (Block info [] Nothing) = warn "Empty block" info
staticBlock (Block _ [] (Just _)) = ok
staticBlock (Block info (s:ss) mr) = do
    let pos = firstPos s
    forM_ ss $ \s' -> do
        let pos' = firstPos s'
        when (posCol pos /= posCol pos') $
            style ("Statement should begin at column " <> tshow (posCol pos)) pos'
    case mr of
        Just r -> do
            let pos' = firstPos r
            when (posCol pos /= posCol pos') $
                style ("Statement should begin at column " <> tshow (posCol pos)) pos'
        Nothing -> ok

staticStatement :: Statement NodeInfo -> Static ()
staticStatement (EmptyStmt _) = ok
-- Checks:
-- * ERROR if length vs < length es
-- * STYLE if there is not one space around the '='
staticStatement (Assign info (VariableList1 vinfo vs) (ExpressionList1 einfo es)) = do
    let vlen = length vs
        elen = length es
    when (vlen < elen) $
        err ("Unused expression(s) in assignment: found " <> vstr vlen <> estr elen) info

    let pos1 = lastPos vinfo
        pos2 = firstPos einfo
    when (posCol pos1 + 4 /= posCol pos2) $
        style "Improper whitespace around '='" pos1
  where
    vstr 1 = "1 variable and "
    vstr n = tshow n <> " variables and "

    estr 1 = "1 expression"
    estr n = tshow n <> " expressions"
staticStatement (FunCall _ _) = ok
-- Checks:
-- STYLE if there is any whitespace between either '::' and the identifier
staticStatement (Label info ident) = do
    let [L (Loc _ pos1) _, L (Loc pos2 pos3) _, L (Loc pos4 _) _] = info^.nodeTokens
    when (posCol pos1 + 1 /= posCol pos2 ||
          posCol pos3 + 1 /= posCol pos4) $
        warn "Improper label whitespace (no spaces before/after '::' allowed)" pos2
staticStatement (Break _) = ok
-- Checks:
-- ERROR if this node exists at all
-- STYLE if there is more than one space after 'goto'
staticStatement (Goto info ident) = do
    err "Edsger Dijkstra (March 1968). \"Go To Statement Considered Harmful\". Communications of the ACM 11 (3): 147-148." info

    let pos1 = firstEndPos info
        pos2 = firstPos ident
    when (posCol pos1 + 2 /= posCol pos2) $
        style "Unnecessary whitespace after 'goto'" info
-- Checks:
-- * The block is is an inner block
staticStatement (Do info block) = do
    case block of
        Block _ [] Nothing -> ok -- don't bother style checking the "do end"
        _ -> innerBlockStyleCheck (firstPos info) (lastStartPos info) block "do" "end"
-- Checks:
-- * STYLE if there is not one space after 'while'
-- * STYLE if there is not one space before 'do'
-- * STYLE if 'while' and 'do' are not on the same line
-- * The block is an inner block
staticStatement (While info exp block) = do
    let whilePos    = firstPos info
        whileEndPos = firstEndPos info
        condPos     = firstPos exp
        condEndPos  = lastPos exp
        doPos       = firstPos $ (info^.nodeTokens) ! (1 + length (exp^.ann.nodeTokens))
        endPos      = lastStartPos info

    when (posCol whileEndPos + 2 /= posCol condPos) $
        style "Unnecessary whitespace after 'while'" whilePos

    -- Don't want to double-warn when both the line and column of 'do' are incorrect.
    if posLine whilePos /= posLine doPos
        then style "Unnecessary newline before 'do'" doPos
        else when (posCol condEndPos + 2 /= posCol doPos) $
                 style "Unnecessary whitespace before 'do'" doPos

    innerBlockStyleCheck whilePos endPos block "while" "end"
-- Checks:
-- * STYLE if there is not one space after 'until'
-- * STYLE if the condition and 'until' are not on the same line
-- * The block is an inner block
staticStatement (Repeat info block exp) = do
    let repeatPos   = firstPos info
        untilToken  = (info^.nodeTokens) ! (1 + length (block^.ann^.nodeTokens))
        untilPos    = firstPos untilToken
        untilEndPos = lastPos untilToken
        condPos     = firstPos exp

    innerBlockStyleCheck repeatPos untilPos block "repeat" "until"

    -- Don't want to double warn when both the line and column of the expression
    -- are incorrect.
    if posLine untilPos /= posLine condPos
        then style "Unnecessary newline after 'until'" untilPos
        else when (posCol untilEndPos + 2 /= posCol condPos) $
                 style "Unnecessary whitespace after 'until'" untilPos
staticStatement (If info ebs mb) = ok
-- Checks:
-- * STYLE if 'for' and 'do' are not on the same line
-- * The block is an inner block
-- TODO: Check whitespace around '=' and ','
staticStatement (For info x e1 e2 me3 block) = do
    let forPos = firstPos info
        doPos  = firstPos $
            (info^.nodeTokens) ! (4 + length (e1^.ann.nodeTokens)
                                  + length (e2^.ann.nodeTokens)
                                  + maybe 0 (\e3 -> 1 + length (e3^.ann.nodeTokens)) me3)
        endPos = lastStartPos info

    when (posLine forPos /= posLine doPos) $
        style "Unnecessary newline before 'do'" doPos

    innerBlockStyleCheck forPos endPos block "for" "end"
-- Checks:
-- * STYLE if 'for' and 'do' are not on the same line
-- * The block is an inner block
-- TODO: Check whitespace around 'in' and ','
staticStatement (ForIn info is es block) = do
    let forPos = firstPos info
        doPos  = firstPos $
            (info^.nodeTokens) ! (2 + length (is^.ann.nodeTokens)
                                  + length (es^.ann.nodeTokens))
        endPos = lastStartPos info

    when (posLine forPos /= posLine doPos) $
        style "Unnecessary newline before 'do'" doPos

    innerBlockStyleCheck forPos endPos block "for" "end"
-- Checks:
-- * STYLE if there is a space between function name and '('
-- * The body's block is an inner block
staticStatement (FunAssign info name body) = do
    let functionPos              = firstPos info
        nameLastPos              = lastPos name
        bodyPos                  = firstPos body
        FunctionBody _ _ _ block = body
        endPos                   = lastStartPos info

    when (posCol nameLastPos + 1 /= posCol bodyPos) $ do
        style "Unnecessary whitespace after function name" nameLastPos

    innerBlockStyleCheck functionPos endPos block "function" "end"
-- Checks:
-- * STYLE if there is a space between function name and '('
-- * The body's block is an inner block
staticStatement (LocalFunAssign info name body) = do
    let localPos                 = firstPos info
        nameLastPos              = lastPos name
        bodyPos                  = firstPos body
        FunctionBody _ _ _ block = body
        endPos                   = lastStartPos info

    when (posCol nameLastPos + 1 /= posCol bodyPos) $ do
        style "Unnecessary whitespace after function name" nameLastPos

    innerBlockStyleCheck localPos endPos block "local" "end"
-- Checks:
-- * ERROR if length is < length es
staticStatement (LocalAssign info (IdentList1 _ is) (ExpressionList _ es)) = do
    let ilen = length is
        elen = length es
    when (ilen < elen) $
        err ("Unused expression(s) in local assignment: found " <> istr ilen <> estr elen) info
  where
    istr 1 = "1 identifier and "
    istr n = tshow n <> " identifiers and "

    estr 1 = "1 expression"
    estr n = tshow n <> " expressions"

-- Checks:
-- STYLE if inner block is not one line down and one indent in from beginning location.
-- STYLE if there is a blank line before 'end'
-- STYLE if the 'end' does not align with the corresponding begin token.
innerBlockStyleCheck :: Pos -> Pos -> Block NodeInfo -> Text -> Text -> Static ()
innerBlockStyleCheck p1 p2 block name1 name2 = do
    let Loc blockBeginPos blockEndPos = block^.ann.nodeLoc

    i <- indent
    when (posLine p1 + 1 /= posLine blockBeginPos ||
          posCol  p1 + i /= posCol  blockBeginPos) $
        style ("Inner block should be on a new line and indented " <> tshow i <> " spaces") blockBeginPos

    when (posLine blockEndPos + 1 /= posLine p2) $
        style "Unnecessary newline before 'end'" p2

    when (posCol p1 /= posCol p2) $
        style ("'" <> name2 <> "' does not align with corresponding '" <> name1 <> "'") p2

staticReturnStatement :: ReturnStatement NodeInfo -> Static ()
staticReturnStatement _ = ok

staticFunctionName :: FunctionName NodeInfo -> Static ()
staticFunctionName _ = ok

staticVariable :: Variable NodeInfo -> Static ()
staticVariable _ = ok

staticVariableList1 :: VariableList1 NodeInfo -> Static ()
staticVariableList1 _ = ok

staticExpression :: Expression NodeInfo -> Static ()
staticExpression _ = ok

staticExpressionList :: ExpressionList NodeInfo -> Static ()
staticExpressionList _ = ok

staticExpressionList1 :: ExpressionList1 NodeInfo -> Static ()
staticExpressionList1 _ = ok

staticPrefixExpression :: PrefixExpression NodeInfo -> Static ()
staticPrefixExpression _ = ok

staticFunctionCall :: FunctionCall NodeInfo -> Static ()
staticFunctionCall _ = ok

staticFunctionArgs :: FunctionArgs NodeInfo -> Static ()
staticFunctionArgs _ = ok

staticFunctionBody :: FunctionBody NodeInfo -> Static ()
staticFunctionBody _ = ok

staticTableConstructor :: TableConstructor NodeInfo -> Static ()
staticTableConstructor _ = ok

staticField :: Field NodeInfo -> Static ()
staticField _ = ok

staticFieldList :: FieldList NodeInfo -> Static ()
staticFieldList _ = ok

staticBinop :: Binop NodeInfo -> Static ()
staticBinop _ = ok

staticUnop :: Unop NodeInfo -> Static ()
staticUnop _ = ok

infixl 9 !
(!) :: Seq a -> Int -> a
s ! n = case viewl (Seq.drop n s) of
            x :< _ -> x
            _ -> error "(!): index out of range"

tshow :: Show a => a -> Text
tshow = T.pack . show
