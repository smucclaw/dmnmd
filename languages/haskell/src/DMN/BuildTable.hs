{-# LANGUAGE LambdaCase #-}

-- | Building a 'DecisionTable' from the markdown reader's rows, and every
-- reason to refuse one, for both readers.
--
-- These two functions lived in "DMN.DecisionTable" until D-22 part 2. They
-- moved because the gate now asks "DMN.Regions" whether a table has a conflict
-- region, and "DMN.Regions" is built on "DMN.DecisionTable"'s matcher
-- ('DMN.DecisionTable.fEvals') and its default ('DMN.DecisionTable.uniqueCatchAll').
-- The gate therefore sits above both. The individual checks stay where they
-- were; only the function that runs them all, and the one that builds a
-- markdown table and runs it, are here.
module DMN.BuildTable
  ( mkDTable
  , tableErrors
  ) where

import Data.List          (transpose)

import DMN.DecisionTable  (domainErrors, getCommentHeaders, getInputHeaders, getOutputHeaders,
                           inferTypes, inferenceErrors, reprocessRows, retypeEnums,
                           structuralErrors, uniquenessErrors)
import DMN.Diagnostic     (Diagnostic, anyErrors, errorAt)
import DMN.Regions        (conflictErrors)
import DMN.Types

-- | Type inference, re-typing, and every reason to refuse the result — as
-- @([Diagnostic], 0-or-1 tables)@, the same shape
-- 'DMN.XML.XmlToDmnmd.convTable' has always returned.
--
-- __The list is the gate.__ An 'DMN.Diagnostic.Error' means the table list is
-- empty, so a caller cannot emit a table it was told to refuse merely by
-- forgetting to look at the diagnostics. That property used to be supplied by
-- @error@ — and, at the CLI, by the accident that @app\/Main.hs@'s
-- @tableWarnings@ loop and @--pick@'s @tableName@ filter both forced every
-- table to WHNF before anything was written. Both accidents are gone; this is
-- the design that replaces them.
--
-- __Cell diagnostics short-circuit 'tableErrors'.__ Same reasoning as
-- 'tableErrors' running 'DMN.DecisionTable.structuralErrors' first: a cell whose meaning we could
-- not read cannot meaningfully be checked against a domain, and the follow-on
-- complaints would be about the placeholder 'reprocessRows' left behind rather
-- than about anything the author wrote.
mkDTable :: String -> HitPolicy -> [ColHeader] -> [DTrow] -> ([Diagnostic], [DecisionTable])
mkDTable origname orighp origchs origdtrows =
--  Debug.Trace.trace ("mkDTable: starting; origchs = " ++ show origchs) $
  let newchs   = zipWith inferTypes (getInputHeaders origchs ++ getOutputHeaders origchs)
                                     (transpose $ [ row_inputs r ++  row_outputs r | r@DTrow{} <- origdtrows])
      (enumDiags, typedchs) =
        (\pairs -> (concatMap fst pairs, snd <$> pairs))
          (retypeEnums origname <$> (if not (null newchs) then newchs ++ getCommentHeaders origchs else origchs))
      rowResults =
        (\case
            (DTrow rn ri ro rc) ->
              let (di, ri') = reprocessRows origname rn (getInputHeaders typedchs)  ri
                  (dobs, ro') = reprocessRows origname rn (getOutputHeaders typedchs) ro
              in (di ++ dobs, DTrow rn ri' ro' rc)) <$> origdtrows
      cellDiags = enumDiags ++ concatMap fst rowResults
      built = DTable origname orighp typedchs (snd <$> rowResults) Nothing
  in -- Debug.Trace.trace ("mkDTable: finishing...\n" ++
        --                 "origchs = " ++ show(origchs) ++ "\n" ++
           --             "newchs = " ++ show(newchs) ++ "\n" )
    -- A cell outside the domain its own sub-header row declares is a typo, not
    -- a new domain member, and a rule built from it can never match. Emitting it
    -- would be a silently-widened table that exits 0. See BUILD-SPEC-dmnmd-e4.md
    -- §8. The XML reader calls 'domainErrors' directly, because 'convTable'
    -- bypasses this function on purpose; both readers now report the same way.
    if anyErrors cellDiags
    then (cellDiags, [])
    else case tableErrors built of
      []   -> (cellDiags, [built])
      errs -> ( cellDiags ++ (errorAt . (("table " ++ show origname ++ ": ") ++) <$> errs)
              , [] )

-- | Every reason to refuse a table, in one place, for both readers.
--
-- 'structuralErrors' is about the table's SHAPE — a cell whose meaning dmnmd
-- will not guess at. 'domainErrors' is about a cell disagreeing with the domain
-- the table itself declares. Structural first, because a cell that has no
-- meaning cannot meaningfully be checked against a domain.
--
-- The last two are about the hit policy's promise. 'uniquenessErrors' (D-13)
-- refuses two rows of a @U@ table with identical guards. 'conflictErrors'
-- (D-22 rule 2) refuses every other conflict region on scalar columns: under
-- @U@ two rules that can both match, not counting a trailing catch-all, and
-- under @A@ two that can both match and give different outputs. Each speaks
-- about a row at most once between them: 'conflictErrors' is silent about a
-- row 'uniquenessErrors' reports. Where 'DMN.Regions.regionMap' cannot compute
-- regions, 'conflictErrors' is silent altogether and only D-13 speaks, as
-- before D-22.
tableErrors :: DecisionTable -> [String]
tableErrors dt = structuralErrors dt ++ inferenceErrors dt ++ domainErrors dt
                 ++ uniquenessErrors dt ++ conflictErrors dt
