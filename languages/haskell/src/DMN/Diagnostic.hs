-- | A complaint about an input document, and the severity rule that decides
-- whether we still emit anything.
--
-- This lived in "DMN.XML.XmlToDmnmd" until the L4 backend needed it too, and a
-- transpiler importing the XML reader to get a two-constructor type is
-- backwards. Nothing about a 'Diagnostic' is XML-specific; 'XmlToDmnmd'
-- re-exports the whole module so its own callers did not change.
--
-- The rule these exist to serve, from @CLAUDE.md@:
--
-- > Anything we parse but do not yet honour must produce a loud, located
-- > diagnostic. Never silently discard. Accepting input and quietly giving a
-- > different answer is strictly worse than rejecting it.
--
-- So the severity is not a style choice:
--
--  * 'Warning' means something was dropped, and says what. The output is still
--    emitted and the run still exits 0.
--  * 'Error' means we could not represent the input faithfully. Whatever it
--    attaches to is __not emitted__, and the run exits 1 — because a table that
--    can never match, or one whose rules have been silently widened, is a wrong
--    answer that exits 0.
module DMN.Diagnostic
  ( Severity (..)
  , Diagnostic (..)
  , warnAt
  , errorAt
  , isError
  , anyErrors
  , renderDiagnostic
  ) where

data Severity = Warning | Error
  deriving (Show, Eq)

-- | A complaint about the document, attributed to the table (and where
-- possible the column and rule) it came from.
data Diagnostic = Diagnostic
  { diagSeverity :: Severity
  , diagMessage :: String
  }
  deriving (Show, Eq)

warnAt :: String -> Diagnostic
warnAt = Diagnostic Warning

errorAt :: String -> Diagnostic
errorAt = Diagnostic Error

isError :: Diagnostic -> Bool
isError = (== Error) . diagSeverity

anyErrors :: [Diagnostic] -> Bool
anyErrors = any isError

renderDiagnostic :: Diagnostic -> String
renderDiagnostic (Diagnostic Warning m) = "warning: " ++ m
renderDiagnostic (Diagnostic Error m) = "error: " ++ m
