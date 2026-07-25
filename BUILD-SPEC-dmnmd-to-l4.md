# BUILD SPEC — `dmnmd --to=l4` (BRANCH + ditto) and the l4-ide ditto codegen tweak

Status: design / build plan. Two coordinated deliverables across two repos.

- **Part A — dmnmd:** a new `--to=l4` backend that transpiles a DMN decision table to L4
  source text, emitting a `BRANCH` expression with column-aligned **ditto (`^`)**.
  Repo: `/Users/mengwong/src/smucclaw/dmnmd` (Haskell impl under `languages/haskell/`).
- **Part B — l4-ide:** a codegen tweak so the jl4 toolchain can itself **emit/round-trip
  ditto** from a structured representation, used to validate dmnmd output and to back any
  future DMN-import-in-IDE feature.
  Repo/worktree: `/Users/mengwong/src/legalese/l4wt/dmnmd-to-l4` (branch `dmnmd-to-l4`).

The acceptance gate is a **golden round-trip**:

```
dmnmd --to=l4 /Users/mengwong/src/mengwong/homelab/docs/miles-card-dmn.md
   ==  /Users/mengwong/src/mengwong/homelab/docs/miles-card.l4   (hand-written golden)
```

wired into `stack test`.

---

## 0. Background: the data the backend consumes

The DMN→L4 backend is a pure `DecisionTable -> String` pretty-printer. It never touches the
parser; it receives a fully-typed, type-inferred `DecisionTable`. Source of truth for the AST is
`languages/haskell/src/DMN/Types.hs`:

- `DecisionTable = DTable { tableName :: String, hitpolicy :: HitPolicy, header :: [ColHeader], allrows :: [DTrow] }`
- `ColHeader   = DTCH { label :: DTCH_Label, varname :: String, vartype :: Maybe DMNType, enums :: Maybe [FEELexp] }`
  with `DTCH_Label = DTCH_Comment | DTCH_In | DTCH_Out`.
- `DTrow       = DTrow { row_number :: Maybe Int, row_inputs :: [[FEELexp]], row_outputs :: [[FEELexp]], row_comments :: [Maybe String] }`
  — note the **two-layer lists**: outer = column, inner = the multiple values inside one cell
  (`Spring, Summer` → an inner list of two values → an OR / membership test).
- `FEELexp = FSection FBinOp DMNVal | FInRange Float Float | FAnything | FNullary DMNVal | FFunction FNumFunction`
  - `FBinOp = Flt | Flte | Fgt | Fgte | Feq`
  - `FAnything` is the wildcard `-` (always-true; **omit from the guard**).
  - `FNullary v` is a bare literal used for equality matching.
  - `FFunction` is an arithmetic expression tree (output side): `FNumFunction = FNF0 DMNVal | FNF1 String | FNF3 FNumFunction FNOp2 FNumFunction`, `FNOp2 = FNMul|FNDiv|FNPlus|FNMinus|FNExp`.
- `DMNVal = VS String | VN Float | VB Bool`.
- `DMNType = DMN_String | DMN_Number | DMN_Boolean | DMN_List DMNType`.
- `HitPolicy = HP_Unique | HP_Any | HP_Priority | HP_First | HP_OutputOrder | HP_RuleOrder | HP_Collect CollectOperator | HP_Aggregate`.

Helpers to reuse (do not reinvent):
- `DMN.DecisionTable.getInputHeaders / getOutputHeaders / getCommentHeaders` (`src/DMN/DecisionTable.hs:241+`).
- `DMN.DecisionTable.evalTable :: DecisionTable -> [FEELexp] -> Either String [[[FEELexp]]]` — the
  reference interpreter implementing full hit-policy semantics. **Use it as the oracle** to
  auto-generate `#EVAL`/assert lines and to cross-check L4 evaluation in tests.
- `DMN.Types.var_name / underscore` — snake_case a column name.
- From `DMN.Translate.PY` (cleanest template): `input_headers`, `comment_headers`, `nonBlankCols`,
  `wrapParen`, `annotationsAsComments`.

---

## 1. The DMN → L4 mapping

| DMN construct | L4 emission |
|---|---|
| `DecisionTable` (one table) | one `GIVEN … GIVETH … <name> MEANS BRANCH …` definition |
| input columns (`getInputHeaders`) | `GIVEN` parameters, one per column, typed via the type map below; names backtick-quoted if they contain spaces (`varname`) |
| output column(s) (`getOutputHeaders`) | `GIVETH` type. One output column → that scalar type. Multiple → a `DECLARE`d result record returned per arm (see §1.4) |
| each data row (`DTrow`) | one `BRANCH` arm: `IF <guard> THEN <result>` |
| hit policy `HP_First` / `HP_Unique` / `HP_Priority` | first-match `BRANCH`; arm order = row order; closed by a synthesized `OTHERWISE` (see §1.5) |
| hit policy `HP_Collect op` | **not** a BRANCH — a `filter`+fold over rows-as-data (see §1.6, deferred) |
| input cell `FAnything` (`-`) | conjunct **omitted** from the arm guard (the column contributes nothing on that row) |
| input cell single value | `` `field` EQUALS <val> `` (string/bool) or `` `field` <op> <num> `` (comparison) |
| input cell multi-value (`Spring, Summer`) | `OR`-of-`EQUALS` expansion `` (`field` EQUALS <v1> OR `field` EQUALS <v2>) `` (default); `` elem `field` (LIST …) `` only via the `useElem` opt (see §1.3) |
| repeated cell vs the arm directly above | **ditto `^`** for each identical token (see §3) |
| output cell literal | the L4 literal (`"str"`, number, `TRUE`/`FALSE`) |
| output cell `FFunction` arithmetic | infix L4 arithmetic (`FNMul`→`TIMES`/`*`, `FNPlus`→`PLUS`/`+`, etc.) |
| row comment (`row_comments`) | trailing `-- comment` (mirror `annotationsAsComments`) |

### 1.1 Type map (`DMNType -> L4 type`)

```
DMN_String   -> STRING
DMN_Number   -> NUMBER
DMN_Boolean  -> BOOLEAN
DMN_List x   -> LIST OF <type2l4 x>
Nothing      -> STRING   (dmnmd default; see baseType)
```

### 1.2 Guard operators (per `FEELexp`)

Default to **single-token** operators so each operator occupies exactly one ditto column
(`AT LEAST` etc. are two tokens and would need two aligned carets — supported but off by default,
see §3.4):

```
FSection Feq  v     -> `field` EQUALS <v>          -- (FNullary v desugars to FSection Feq v)
FSection Flt  (VN n)-> `field` <  n
FSection Flte (VN n)-> `field` <= n
FSection Fgt  (VN n)-> `field` >  n
FSection Fgte (VN n)-> `field` >= n
FInRange a b        -> `field` >= a AND `field` <= b
FAnything           -> (omit conjunct)
```

`AND`-join the per-column conjuncts of one row to form the arm guard. If **every** input cell on a
row is `FAnything`, the arm guard is vacuously true → that row becomes the `OTHERWISE` (or `IF TRUE
THEN …`).

Value rendering (`DMNVal`): `VS s -> "<s>"` (double-quoted); `VB True/False -> TRUE/FALSE`;
`VN n -> ` integral floats without a trailing `.0` (`9.0` → `9`), non-integral as-is. Provide
`showNumL4 :: Float -> String`.

### 1.3 Multi-value cells → membership

A cell whose inner list has >1 value (e.g. `Spring, Summer`) is an OR over equality. **This `l4`
build's prelude has no `elem`** (confirmed: `l4 check` errors `could not find a definition for the
identifier elem`), so the **default** emission is an explicit `OR`-of-`EQUALS` expansion:

```
(`field` EQUALS "Spring" OR `field` EQUALS "Summer")
```

For a single value, stay with `EQUALS` (keeps the common case ditto-friendly).

`elem` is available **only as an optional `L4Opts` toggle** (`useElem`, default `False`) for builds
whose prelude does provide it:

```
elem `field` (LIST "Spring", "Summer")     -- only when useElem = True
```

(`elem`'s `@nlg` renders as “_field_ is one of _list_”.) Membership-by-predicate (structural
equality against a statutory table) would use prelude `any (pred) list` — not needed for the flat
miles-card table.

Note: the hand-written golden additionally factors some of these multi-value groups into **named
ditto-flattened predicates** (e.g. `` `is a yuu grocer` ``, `` `is online four-mpd` ``) for
readability. That is a hand-authoring nicety, **not** something the mechanical emitter must reproduce
— the emitter may inline the `OR`-of-`EQUALS` directly. This divergence is exactly why the golden
round-trip is a **semantic** equivalence check, not a byte-exact one (see §7).

### 1.4 Outputs

- **One output column:** `GIVETH A <type>`; each arm’s `THEN`/`OTHERWISE` returns the bare value.
- **Multiple output columns:** emit a `DECLARE <Name> HAS f1 IS A <t1> …` record and `GIVETH A <Name>`.
  **Inline multi-field record literals on one line do NOT parse — L4 record literals are
  layout-sensitive.** Each arm (and the `OTHERWISE`) must therefore return the record in one of two
  forms:
  - **multi-line `WITH` block** — `<Name> WITH` followed by each `fN IS vN` on its own indented line; or
  - **constructor helper (recommended)** — synthesize one `mk<Name> v1 v2 …` function and have every
    arm call it. The golden uses this form: it declares `mkRec theCard theMpd` (whose body is itself a
    multi-line `Recommendation WITH` block) and every arm / `OTHERWISE` returns
    `` mkRec `PRVI` "1.4" `` etc. on a single line.

  Prefer the constructor helper: it keeps each BRANCH arm on one line, which is what the ditto grid
  (§3) needs to collapse. The multi-line `WITH` block is also valid but forces multi-line arms the
  ditto pass cannot align.

### 1.5 First/Unique/Priority → BRANCH + OTHERWISE

`BRANCH` requires a terminal `OTHERWISE`. DMN tables frequently have no explicit default row, so the
backend **synthesizes** one. The convention is **resolved by the validated golden**:

- **Default — bare-typed `OTHERWISE`.** `OTHERWISE` returns a **bare value of the result type**, with
  **no** `MAYBE`/`NOTHING` wrapping. The value is the catch-all row's output (the all-`FAnything` row
  when present), rendered to L4 and carried in `L4Opts.defaultResult`. For a **multi-output** table
  the default is therefore a **full record built via the same constructor helper** (§1.4), not a
  scalar. The golden ends its `card to use` BRANCH with `` OTHERWISE mkRec `PRVI` "1.4" `` (its
  catch-all row) and its `categorize` BRANCH with `` OTHERWISE `Other` ``.
- **Optional — `wrapMaybe = True` (non-default).** Emit `GIVETH A MAYBE <type>`, arms return
  `JUST <value>`, and `OTHERWISE NOTHING`. Total and faithful to “no rule matched”, but the golden
  does **not** use it; it is an opt-in mode, not the primary path.

`HP_Unique` and `HP_Priority` also emit first-match `BRANCH` (order = row order; `evalTable`
semantics for conflict/priority are not re-encoded into the L4 control flow in v1 — noted risk §9).

### 1.6 Collect (deferred to v1.1)

`HP_Collect Collect_Sum|Min|Max|Cnt|All` maps to “rows as DATA + fold”, mirroring the
`drafting-patterns` statutory-table idiom and `evalTable`’s collect semantics: build a `LIST OF`
rule-records, `filter` by the guard, then `sum`/`min`/`max`/`length`/identity. Out of scope for the
miles-card golden; stub `toL4` to `error "Collect not yet supported"` for those policies and cover
only `HP_First`/`HP_Unique` in v1.

---

## 2. Worked shape (illustrative — orchestrator owns the exact golden)

Conceptual input `miles-card-dmn.md` (a First-hit table; columns are illustrative):

```
| F   | card tier | spend category | THEN | miles per dollar |
|-----|-----------|----------------|------|------------------|
| 1   | platinum  | dining         |      | 10               |
| 2   | platinum  | travel         |      | 8                |
| 3   | platinum  | -              |      | 5                |
| 4   | gold      | dining         |      | 6                |
| 5   | gold      | -              |      | 3                |
```

Illustrative L4 the backend should produce (BRANCH + ditto; columns exactly aligned):

```l4
GIVEN `card tier`      IS A STRING
      `spend category` IS A STRING
GIVETH A NUMBER
`miles per dollar` MEANS
 BRANCH
  IF `card tier` EQUALS "platinum" AND `spend category` EQUALS "dining" THEN 10
  IF ^          ^      ^           ^   ^                ^      "travel" THEN 8
  IF ^          ^      ^                                                THEN 5
  IF ^          ^      "gold"      ^   ^                ^      "dining" THEN 6
  IF ^          ^      ^                                                THEN 3
  OTHERWISE 0
```

Notes that the algorithm must honor:
- Arm 3 drops the second conjunct entirely (the `-` cell): columns under `AND … "dining"` are
  **blank**, not `^` (you cannot ditto-copy an absent token).
- `card tier`, `EQUALS`, `AND`, `spend category`, its `EQUALS` are identical down the column → all
  become `^` after the first arm. Only changed values are re-typed.
- A `^` copies the **token on the previous non-blank line at the identical start column**, and
  resolves transitively (arm-4 `^` under `card tier` copies arm-3’s, which copied arm-2’s, … back to
  the literal on arm 1). Therefore **no blank or comment-only lines may appear between arms**.

(The default is **resolved**: `OTHERWISE` returns a **bare value** — here `OTHERWISE 0`, the catch-all
row's output carried in `L4Opts.defaultResult`. In a multi-output table it would be a full record via
the constructor helper, e.g. the golden's `` OTHERWISE mkRec `PRVI` "1.4" ``. `MAYBE`/`NOTHING` is an
opt-in mode (`wrapMaybe`), not the default — see §1.5.)

---

## 3. The ditto grid algorithm (shared by Part A and Part B)

Ditto is purely an emission-layer, column-positional concern. The lexer
(`jl4-core/src/L4/Lexer.hs:689,703-708`) resolves `^` by **exact start-column match against the
previous non-whitespace line** (`findMatchingToken c pts = find (\pt -> pt.range.start.column == c)`),
and a `^`-over-`^` resolves to the original token (`computedPayload`/`RealTCopy`). Column alignment is
therefore load-bearing: an off-by-one column silently copies the wrong token (or fails to resolve).

Algorithm (`renderDittoGrid`):

1. **Cells.** Render each arm to a list of logical cells. A cell is a `Maybe Text`: `Just tok` for a
   real token (field / operator / value / connector), `Nothing` for an absent token (a `-` column, or
   a row with fewer conjuncts). Every arm uses the **same column layout** (one triple per input
   column + connectors + `THEN` + result), so column index = logical slot across all arms.
2. **Widths.** `colWidth[j] = maximum (map (cellLen . (!! j)) arms)` where `cellLen Nothing = 0`.
3. **Lay out** each line left-aligned: pad every cell to `colWidth[j]`, separate by a fixed
   single-space gutter. This guarantees cell `j` starts at the identical absolute column on every
   line — the lexer’s precondition.
4. **Ditto pass.** For arm `i>0`, column `j`: if `cell[i][j] == cell[i-1][j]` **and** that cell is
   `Just` (a real, resolvable token), replace it with `^` (then pad to `colWidth[j]`). If the cell is
   `Nothing`, emit `colWidth[j]` spaces (copy nothing). Leave `Just` cells that differ as-is.
5. **Contiguity.** Never emit a blank or comment-only line inside the aligned block (the lexer skips
   whitespace/comment lines when finding the previous line, which would make a `^` copy the wrong
   row). Keep the BRANCH arm block contiguous; put any row comments at end-of-line, not on their own
   line.

### 3.1 What gets dittoed
Default: ditto the **guard** cells (field, operator, value, `AND`). Keep `IF`, `THEN`, and the result
literal/expression spelled out on every arm (more readable; matches `mixfix-garden-path.l4`). Make
this configurable (`emitDitto :: Bool`, `dittoKeywords :: Bool`).

### 3.2 Fallback
With `emitDitto = False`, skip step 4 entirely → fully spelled-out, still column-aligned arms. This is
the safe mode for debugging and the basis for the round-trip equivalence test (§7).

### 3.3 BRANCH needs no AST work in l4-ide
`BRANCH` is already a first-class node (`L4.Syntax.MultiWayIf`, `Parser.hs:1727`) and already prints
via `L4.Print` (`Print.hs:393-397`). The new work is **only** the ditto/column layer.

### 3.4 Multi-token operators (optional)
If word-form operators (`AT LEAST`, `AT MOST`) are preferred, treat each word as its own cell so each
gets an independently column-aligned `^`. Off by default; single-token symbolic operators are the
ditto-friendly default.

---

## 4. Part A — files in dmnmd

All paths under `/Users/mengwong/src/smucclaw/dmnmd/languages/haskell/`.

### 4.1 CREATE `src/DMN/Translate/L4.hs`
Module `DMN.Translate.L4`. Model on `src/DMN/Translate/PY.hs` (single-flag opts, no TS complications).

```haskell
module DMN.Translate.L4 where

import DMN.Types
import DMN.DecisionTable (getInputHeaders, getOutputHeaders, getCommentHeaders)
import Data.List (intercalate, transpose)

data L4Opts = L4Opts
  { emitDitto     :: Bool        -- ^ collapse repeated tokens to ^
  , wordOps       :: Bool        -- ^ AT LEAST vs >=
  , useElem       :: Bool        -- ^ multi-value cells as `elem … (LIST …)`; default False => OR-of-EQUALS
  , defaultResult :: String      -- ^ rendered L4 expr emitted after OTHERWISE (the catch-all row's output, e.g. mkRec `PRVI` "1.4")
  , wrapMaybe     :: Bool        -- ^ optional non-default: GIVETH A MAYBE …, arms JUST …, OTHERWISE NOTHING
  , emitAsserts   :: Bool        -- ^ append #EVAL/#ASSERT lines from evalTable oracle
  }

toL4 :: L4Opts -> DecisionTable -> String
```

Self-contained leaf renderers (do **not** thread `"l4"` through `FEELhelpers.hs` — L4 surface syntax
diverges too far from the C-family):

- `givenBlock  :: L4Opts -> [ColHeader] -> String`   — `GIVEN … IS A <type>` lines + `GIVETH`.
- `type2l4     :: DMNType -> String`                  — the §1.1 map.
- `feel2l4In   :: L4Opts -> ColHeader -> [FEELexp] -> Maybe Cell` — one guard conjunct (Nothing for `FAnything`); multi-value inner list → `OR`-of-`EQUALS` by default (`` elem … (LIST …) `` only when `useElem`).
- `showFeelL4  :: FEELexp -> String` / `fnf2l4 :: FNumFunction -> String` — output side; `FNF3 l FNMul r -> "(" <> … <> " TIMES " <> … <> ")"`, etc.
- `showValL4   :: DMNVal -> String`, `showNumL4 :: Float -> String`.
- `renderDittoGrid :: L4Opts -> [[Maybe Cell]] -> [String]` — §3 algorithm; the rule walker
  (analogous to PY’s `zipWith … mkIf`) builds the `[[Maybe Cell]]` then calls this.
- `annotationsAsComments` — reuse the PY shape, but emit `--` (L4) end-of-line comments.

Hit policy: implement `HP_First`/`HP_Unique`/`HP_Priority` → BRANCH; `HP_Collect`/others → `error`
stub for v1 (§1.6).

Optionally, when `emitAsserts`, run `evalTable` over sampled inputs and append `#EVAL <fn> <args>`
lines so the generated `.l4` self-checks against DMN semantics.

### 4.2 MODIFY `app/Options.hs`
- Line 58: `data FileFormat = Ts | Js | Py | Xml | Md | L4 | Unknown`.
- Line 67 `parseFileFormat`: add `"l4" -> return L4`; update the error string to list `l4`.
- Line 76 `fileExtensionMappings`: add `(".l4", L4)`.

### 4.3 MODIFY `app/Main.hs`
- Import: `import DMN.Translate.L4 ( toL4, L4Opts(..) )`.
- Extend the `FileFormat(..)` import list (line 43) with `L4`.
- `outputTo` (line 148): add
  `outputTo h L4 opts dtable = hPutStrLn h $ toL4 (defaultL4Opts) dtable`
  (define `defaultL4Opts = L4Opts { emitDitto = True, wordOps = False, useElem = False, defaultResult = "", wrapMaybe = False, emitAsserts = False }`; the backend fills `defaultResult` from the table's catch-all row (all-`FAnything` inputs) — e.g. the golden's `` mkRec `PRVI` "1.4" `` — and falls back to this field only when the table has no catch-all row).
- `showToJSON` (line 140): optionally add an `L4` clause for `--query`; otherwise `--query --to=l4`
  remains a partial-match error (document “query unsupported for L4 in v1”). Not required for file
  emission.

### 4.4 MODIFY `dmnmd.cabal`
Add `DMN.Translate.L4` to library `exposed-modules` (after `DMN.Translate.JS`, line 35). Keep
`package.yaml`/`dmnmd.cabal` in sync — regenerate with `hpack` if available (modules are
auto-discovered from `source-dirs`, but the checked-in `.cabal` lists them explicitly).

### 4.5 CREATE test module + fixtures (see §7)
- `test/TranslateL4Spec.hs` (registered in `test/Spec.hs` `forM_` list and in `dmnmd.cabal` test
  `other-modules`).
- `test/golden/miles-card-dmn.md`, `test/golden/miles-card.l4` — committed copies of the homelab
  files (provenance noted in a header comment / `test/golden/README.md`).

---

## 5. Part B — l4-ide ditto codegen tweak

All paths under `/Users/mengwong/src/legalese/l4wt/dmnmd-to-l4/`.

**Do not add a ditto node to the core AST.** The parser never produces ditto (it is expanded in the
lexer), so a `Ditto` constructor would pollute `jl4-core` and could never round-trip. Ditto is a
layout concern; keep it in an emission helper.

### 5.1 CREATE `jl4-core/src/L4/Print/Columnar.hs`
Module `L4.Print.Columnar`. A standalone, AST-agnostic grid emitter implementing the **same §3
algorithm** as dmnmd (the two repos do not share a package, so the algorithm is intentionally
duplicated — see risk §9):

```haskell
module L4.Print.Columnar
  ( Cell, Grid, renderDittoGrid, DittoOpts(..) ) where

type Cell = Maybe Text          -- Nothing = absent token (no caret)
type Grid = [[Cell]]
data DittoOpts = DittoOpts { gutter :: Int, enableDitto :: Bool }
renderDittoGrid :: DittoOpts -> Grid -> Text
```

This is the reusable primitive l4-ide gains. It is what a future DMN-import-in-IDE, or a
“render a decision table back to aligned L4” feature, calls.

### 5.2 Wire it into source generation
The canonical printer `L4.Print.prettyLayout` (`jl4-core/src/L4/Print.hs:29`) uses `group`/`softline`
(`prettyConj`, `Print.hs:776-781`) which collapses to single lines and gives no absolute-column
control — it cannot produce ditto. Two acceptable wirings; pick per appetite:

- **(preferred, minimal)** Add a ditto-aware path used only when emitting `MultiWayIf` whose arms are
  structurally a grid: a new entry point `prettyLayoutDitto :: … -> Text` that renders the BRANCH
  header structurally and delegates the arm block to `renderDittoGrid`. Leaves the default
  `prettyLayout` untouched.
- **(format round-trip)** `L4.ExactPrint` already round-trips an existing `^` (a `RealTCopy` displays
  back as `^`, `Lexer.hs:1037`), so `l4 format` preserves hand-written/dmnmd-emitted ditto **today**.
  Verify this in a test; no change needed for round-trip preservation, only for *generation*.

### 5.3 cabal
Add `L4.Print.Columnar` to `jl4-core/jl4-core.cabal` `exposed-modules`. Build with
`cabal build all` (GHC 9.10.2, `index-state: 2025-03-31`).

### 5.4 Validator used by the tests
`cabal run l4 -- check <file>.l4` is the typecheck gate; `cabal run l4 -- run <file>.l4` evaluates;
`cabal run l4 -- format <file>.l4` reformats. The `l4` skill / MCP validator wraps the same check.

---

## 6. CLI wiring summary for `--to=l4`

```
dmnmd --to=l4 input.md            # explicit
dmnmd -t l4 input.md
dmnmd input.md -o out.l4          # .l4 output extension auto-selects L4 (fileExtensionMappings)
```

`main` pipeline is unchanged: `parseOptions` → `parseTables` (md/xml) → `--pick` filter →
`mapM_ (outputTo h (outformat opts) opts)`. The only new dispatch is the `L4` arm of `outputTo`.

---

## 7. Test strategy

Three layers, the first being the required gate.

### 7.1 Golden round-trip wired into `stack test` (required)
`test/TranslateL4Spec.hs`, registered in `test/Spec.hs`:

```haskell
module TranslateL4Spec (l4Spec) where

import Test.Hspec
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import DMN.ParseTable (parseTable)
import DMN.ParsingUtils (parseOnly)         -- same helper the eval specs use
import DMN.Translate.L4 (toL4, L4Opts(..))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

milesOpts :: L4Opts
milesOpts = L4Opts { emitDitto = True, wordOps = False, useElem = False
                   , defaultResult = "", wrapMaybe = False, emitAsserts = True }

l4Spec :: Spec
l4Spec = describe "dmnmd --to=l4 golden (Option A — semantic, not byte-exact)" $ do
  it "miles-card emitter output is behaviourally equivalent to the golden" $ do
    md <- TIO.readFile "test/golden/miles-card-dmn.md"
    let dt  = either error id (parseOnly (parseTable "miles per dollar") md)
        got = T.pack (toL4 milesOpts dt)
    TIO.writeFile "test/golden/.out/miles-card.l4" got
    -- Option A: do NOT diff strings against test/golden/miles-card.l4. That file
    -- is the readable hand-authored reference and intentionally factors groups into
    -- named predicates the emitter need not reproduce (§1.3). Instead check BEHAVIOUR:
    -- emitAsserts = True carries the golden's #ASSERT block, so `l4 run` exits
    -- non-zero if any assertion fails. (A structural AST-equivalence variant — parse
    -- both and compare ASTs ignoring layout / helper naming — lives in §7.3.)
    (code, _out, _err) <- readProcessWithExitCode
        "/Users/mengwong/.local/bin/l4" ["run", "test/golden/.out/miles-card.l4"] ""
    code `shouldBe` ExitSuccess
```

Wiring:
- Add `l4Spec` to the `forM_ [spec1, …, l4Spec]` list in `test/Spec.hs:31` and add
  `TranslateL4Spec` to `dmnmd.cabal` test-suite `other-modules`.
- `test/golden/` holds **committed copies** of the two homelab files. A `Makefile` target keeps them
  in sync with the canonical homelab sources:
  ```make
  sync-golden:
  	cp /Users/mengwong/src/mengwong/homelab/docs/miles-card-dmn.md languages/haskell/test/golden/
  	cp /Users/mengwong/src/mengwong/homelab/docs/miles-card.l4     languages/haskell/test/golden/
  ```
  (Committed copies are used because the homelab repo is not guaranteed present in dmnmd CI.)

**Option A — semantic, not byte-exact.** Do **not** use exact-string `shouldBe` against the golden:
the hand-written `miles-card.l4` is the readable reference, not a byte-for-byte target, and it factors
groups into named predicates the emitter need not reproduce. "Golden is authoritative" now means
**semantically** authoritative — parse both sides and compare ASTs (ignoring layout / whitespace /
helper naming), and/or execute the golden's own `#ASSERT`s via `~/.local/bin/l4 run` as a behavioural
check (above). The golden is never regenerated to match the emitter byte-for-byte.

### 7.2 Out-of-tree CLI golden (runs the literal acceptance command, checked semantically)
A `Makefile` target / shell check that runs the **built binary** against the absolute homelab paths,
then validates the output with the L4 toolchain (Option A — semantic, not a raw `diff`):

```make
golden-homelab:
	stack run -- --to=l4 /Users/mengwong/src/mengwong/homelab/docs/miles-card-dmn.md > /tmp/miles-card.gen.l4
	~/.local/bin/l4 check /tmp/miles-card.gen.l4
	~/.local/bin/l4 run   /tmp/miles-card.gen.l4    # behavioural: the #ASSERTs must pass
	# informational only (NOT a gate): the golden factors groups into named predicates,
	# so a raw diff is expected to differ
	diff -u /Users/mengwong/src/mengwong/homelab/docs/miles-card.l4 /tmp/miles-card.gen.l4 || true
```

Non-blocking in CI (depends on the homelab checkout); run locally/manually.

### 7.3 Per-hit-policy unit tests + L4 validation
- Reuse the inline `dmn1`…`dmn6a` fixtures already in `test/Spec.hs` (Unique, First, OutputOrder,
  Collect Sum/Count, arithmetic output `dmn6a`). Assert `toL4 opts (parse dmnX)` against small
  expected strings; cover one table per policy class implemented in v1.
- **Equivalence test (catches column-misalignment that still typechecks):** two AST comparisons, both
  via the jl4 toolchain (Option A): (a) generate both `emitDitto = True` and `emitDitto = False` for
  the same table and assert equal ASTs — the spelled-out variant is the oracle; a mis-aligned `^`
  copies a different token → ASTs diverge; and (b) parse the emitter output **and the committed
  golden** (`test/golden/miles-card.l4`) and assert their ASTs are equivalent **ignoring layout /
  whitespace / helper naming** (the golden factors groups into named predicates the emitter inlines).
- **L4 validity gate:** pipe the emitted `.l4` through `cabal run l4 -- check` (in the l4wt worktree)
  or the `l4` skill validator. Necessary but not sufficient — pair with the equivalence test above.

### 7.4 Oracle assertions (back the Option A behavioural gate)
With `emitAsserts = True`, the backend appends `#ASSERT` lines computed from `evalTable` (and/or
carries over the golden's hand-written `#ASSERT` block), so running the generated file under
`~/.local/bin/l4 run` cross-checks L4 evaluation against DMN semantics and **fails the build** on any
divergence. This is what the §7.1 gate executes; it is no longer merely optional under Option A.

---

## 8. Build/run commands

dmnmd (`languages/haskell/`):
```
stack build
stack test                      # includes the golden round-trip
stack run -- --to=l4 path/to/table.md
make sync-golden                # refresh fixtures from homelab
make golden-homelab             # literal acceptance diff
```

l4-ide (`/Users/mengwong/src/legalese/l4wt/dmnmd-to-l4`):
```
cabal build all
cabal run l4 -- check  generated.l4
cabal run l4 -- format generated.l4
```

---

## 9. Open questions / risks

1. **OTHERWISE / default semantics — RESOLVED.** The validated golden exists
   (`/Users/mengwong/src/mengwong/homelab/docs/miles-card.l4`; passes `l4 check` and `l4 run`). The
   convention is a **bare-typed `OTHERWISE`** returning the catch-all row's output via the constructor
   helper — `` OTHERWISE mkRec `PRVI` "1.4" `` for the multi-output `card to use`, `` OTHERWISE `Other` ``
   for the scalar `categorize`. `L4Opts.defaultResult` carries that rendered expression; `MAYBE`/`NOTHING`
   (`wrapMaybe`) is an opt-in non-default mode.
2. **Byte-exact golden + ditto whitespace — RESOLVED / MOOT.** Superseded by **Option A** (§7): the
   golden round-trip is a **semantic / AST-equivalence** check plus a behavioural `~/.local/bin/l4 run`
   of the `#ASSERT`s, not exact-string `shouldBe`, so trailing-space / gutter brittleness no longer
   gates the build and the golden is never regenerated to match the emitter byte-for-byte. (Column
   alignment still matters for a *correct* `^`; a misaligned caret that copies the wrong token is
   caught by the §7.3 AST-equivalence test, not by string diffing.)
3. **Algorithm duplication across repos.** The §3 grid algorithm lives in both `DMN.Translate.L4`
   (dmnmd) and `L4.Print.Columnar` (jl4-core). They cannot share a package (dmnmd must stay
   dependency-light; it does not depend on jl4-core). Mitigation: identical spec here + a shared
   fixture set; consider extracting a tiny `ditto-grid` Hackage-style package later.
4. **Hit-policy fidelity.** v1 maps First/Unique/Priority all to a first-match BRANCH and defers
   Collect/Aggregate/OutputOrder’s richer semantics (which `evalTable` fully implements). A `Priority`
   table whose row order ≠ priority order would mis-translate; either reorder arms by `enums`/priority
   or restrict v1 to First/Unique. Document the limitation; gate unsupported policies with a clear
   `error`.
5. **Number formatting.** dmnmd stores numbers as `Float`. `showNumL4` must render `9.0` as `9` to
   match a hand-written golden, and must not lose precision on genuine decimals (`0.02`). Watch
   scientific-notation edge cases from `show :: Float -> String`.
6. **Field-name quoting.** Column names with spaces need backticks in L4 (`` `card tier` ``); names
   that are L4 keywords also need quoting. Always backtick multi-word/`varname`s; verify against the
   L4 keyword set.
7. **`l4` CLI availability.** Under **Option A** the `stack test` golden gate (§7.1) and the validation
   steps (§7.3) both invoke `~/.local/bin/l4` (`check` / `run`), so that binary must be built and on
   PATH (`cabal build all` first, GHC 9.10.2 via ghcup; the validated golden already passes both). The
   gate is therefore **no longer** a self-contained pure-string comparison — semantic equivalence needs
   the L4 parser/evaluator, which only the toolchain provides.
8. **Multi-value + ditto interaction.** A column that is a single `EQUALS` on some arms and a
   multi-value `OR`-of-`EQUALS` chain (or `elem … LIST` under `useElem`) on others breaks that column’s
   ditto alignment (different token shapes). Acceptable (ditto is best-effort per column), but the
   golden must reflect it; do not force ditto across heterogeneous cells.
```
