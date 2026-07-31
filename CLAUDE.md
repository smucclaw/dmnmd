# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Latitude

Per the author: dmnmd was written in a matter of weeks and **has never run in anger**. Bugs and
bitrot are expected rather than surprising, and rearchitecting, redesigning and refactoring are
explicitly welcome — do not contort a fix to preserve an existing shape that was never load-bearing.
Fix root causes rather than patching symptoms.

Two things this does *not* license, both from `BUILD-SPEC-dmnmd-extensions.md` §6: do not extend the
**markdown format** speculatively (every extension must survive "would a lawyer still recognise this
as a table?"), and this is not becoming a general DMN implementation. The latitude is about the
implementation, not the language surface.

## What this is

`dmnmd` is a CLI that reads DMN decision tables written as Markdown pipe tables and
evaluates them or transpiles them to other languages (TS, JS, Python, L4). Semantics are
DMN/S-FEEL; syntax is Markdown. `README.md` at the repo root doubles as the spec *and* as a
live test fixture — its example tables are real input (`dmnmd README.md --pick "Example 2"`).

`languages/` is a monorepo of would-be implementations, but only `languages/haskell/` is
real code. `languages/python/` and `languages/typescript/` are README-only placeholders.
(`languages/gf/`, a Grammatical Framework natural-language-generation experiment, was
removed — it was self-contained and nothing depended on it. The root README's
`--to=english` section is still aspirational; there is no English backend.)

## Commands

All build/test commands run from `languages/haskell/`.

```
cabal build
cabal test
cabal run -- dmnmd --to=l4 path/to/table.md     # or: cabal run -- dmnmd README.md --to=ts -r
cabal install exe:dmnmd --overwrite-policy=always \
  --install-method=copy --installdir=$HOME/.local/bin   # puts dmnmd on PATH
```

**dmnmd is cabal-only.** `stack.yaml`, `stack.yaml.lock` and `package.yaml` were removed to
match `legalese/l4-ide`, which builds with cabal and no stack. So **`dmnmd.cabal` is
hand-maintained and is the single source of truth** — edit it directly. There is no hpack
step, and nothing regenerates it. The one thing hpack did that nothing does now is
auto-discover modules: a new module under `src/` must be added to `exposed-modules` by hand,
or it is silently not compiled into the library.

GHC 9.10.3 (recorded in `tested-with`, and pinned in CI) with **megaparsec ≥ 9.7.0**, which
is a real lower bound in `dmnmd.cabal` rather than a convention: `DMN.Translate.L4` imports
`Text.Megaparsec.Unicode (isWideChar)`, which does not exist before it. CI runs
`cabal test` and then `make corpus`.

**There are no system dependencies.** `regex-pcre` — and with it `pkg-config` + `libpcre`,
which every install line in this repo used to name — was retired once the cell layer stopped
using regexes. Two of its five call sites went with the interval recogniser; the other three
were `inferType` classifiers whose patterns turned out to be eight literal substrings and one
anchored digit test, i.e. `isInfixOf` and five lines of `span isDigit`. `shell.nix` is now an
empty shell kept only as the machine-readable place to record that.

This matters beyond tidiness: `jl4-wasm` build-depends on `jl4-core`, so anything jl4-core
might one day depend on has to cross-build for wasm32, and a C-library binding cannot.
`legalese/l4-ide` hit the same wall from the other side and resolved it the same way — see its
`specs/done/WASM-LSP-SPEC.md`, which records dropping `pcre2` from `jl4-core.cabal` for exactly
that reason.

Single test / focused runs (hspec, via `--test-options`):

```
cabal test --test-options='--match "renders FInRange"'
cabal test --test-options='--match "/DMN.Translate.L4.toL4/"'   # a whole describe block
```

Watch loops and fixture refresh live in the `Makefile`. cabal has no `--file-watch`, so all
three go through `ghcid`, and the component split matters — `cabal repl test:dmnmd-test` does
**not** load `src/` (the library arrives as a built dependency), which is why one target uses
`--enable-multi-repl` and the other uses `--restart=src`:

```
make ghcid          # ghcid over the library
make ghcid-tests    # library + test suite together; typecheck only, :main is unsupported there
make tests-watch    # runs the suite on every change, failures first
make sync-golden    # re-copy test/golden/ fixtures from the homelab repo
```

## Pipeline

```
Markdown file
  → app/ParseMarkdown.hs   grepMarkdown: slice the file into InputChunks (runs of
                           pipe-prefixed lines), naming each from the preceding heading
  → DMN/ParseTable.hs      parseTable: one chunk → DecisionTable
  → DMN/Types.hs           the IR: DecisionTable / ColHeader / DTrow / FEELexp
  → DMN/DecisionTable.hs   evalTable (interpreter) + mkDTable (type inference)
  → DMN/Translate/*.hs     JS.hs (serves both --to=js and --to=ts), PY.hs, L4.hs
```

`app/Main.hs` is the driver: parse options → parse tables → `--pick` filter → either
`outputTo` a backend or `runInputT` an interactive eval REPL (`-q`).

Things that are only apparent across several files:

- **Type inference is a second pass.** `parseTable` first parses every cell as a string;
  `mkDTable` then infers each column's `DMNType` from the whole column (`inferTypes`) and
  re-runs `mkFAt` over the cells (`reprocessRows`). So a cell's `FEELexp` shape depends on a
  type that isn't known until the table is fully parsed. Explicit `Column : Number` headers
  short-circuit this.
- **`FEELexp` is the cell IR** for both inputs and outputs: `FSection` (a comparison
  section like `<= 8`), `FInRange` (`[5..8]`), `FAnything` (`-`), `FNullary` (a literal),
  `FFunction` (arithmetic like `age * 100`). Cells are `[[FEELexp]]` — the inner list is a
  multi-value cell (`Fall, Winter`), the outer list is the columns.
- **Table names come from Markdown headings**, cleaned by `cleanTableName`: the first
  backticked token (`` `Categorize` — hit policy `F` `` → `Categorize`), else the text
  before the first `:`. `--pick` matches those names.
- **Hit policy is the top-left cell** (`U A P F O R C`, `mkHitPolicy_` in `ParseTable.hs`);
  `evalTable` implements all of them, but the transpilers do not.
- **The sub-header row is a checked domain, and it goes *below* the `|---|`.** A row whose
  first cell is blank, before the first numbered row, declares what its columns may hold
  (`README.md` Example 3; DMN 1.3 fig 8.19). `DecisionTable.domainErrors` refuses a table
  whose plain-value cells fall outside it — but a *test* (`< 18`, `[18..65]`, `-`) is exempt,
  because a test selects a subset of the domain rather than naming a member. Membership is
  decided by `fEval`, the same function that decides run-time matching, so the check cannot
  drift from it and a declared numeric range constrains numeric cells for free. Both readers
  share it; the XML reader calls it directly because `convTable` bypasses `mkDTable` on
  purpose. Above the `|---|`, GFM does not render a table at all.
- **A collection column's cell means membership, and the ambiguous shapes are refused.**
  `tags : [Number]` is DMN's `isCollection`. `mkFEither` parses such a cell at the *element*
  type (`elemType`, which does **not** recurse — nested `[[T]]` is refused), so `FEELexp`
  gains nothing and the list-ness lives in the column type where every backend can see it.
  A plain value means "the collection contains this"; a comparison, range or arithmetic
  expression is refused, because "some element > 3" and "every element > 3" are different
  rules and DMN gives the construct no meaning. **The refusals live in
  `DecisionTable.structuralErrors`, which walks `allrows`** — not in `mkFEither`, which
  cannot tell an input cell from an output cell from a sub-header domain member, since
  `ParseTable` builds `enums` through the same `mkFsAt`. Putting them there would make a range
  domain `[0..150]` unwritable, and the located wrappers are still `either error id` underneath,
  so a `Left` would crash ordinary tables.
  A runtime *value* is a different thing from a cell and is parsed by `mkInputValue`, the
  sole producer of `DMNVal`'s `VL`.
- **A double-quoted cell is a string literal**, unwrapped **all-or-nothing per cell**
  (`unquoteCell`). Per-fragment unquoting has been tried and reverted — `mkFsEither` splits on
  commas first, so `not("Fall", …)` arrives shredded and unquoting the well-formed fragments
  yields something that is neither the source text nor a parse of it. Pinned from both sides by
  `policy/md-quoted-literal-all-or-nothing` and `symptom/xml-comma-split-negation`.
- **The parser is megaparsec.** `DMN/ParsingUtils.hs` holds attoparsec-shaped shims
  (`many1`, `anyChar`, `notChar`, `parseOnly`) left over from an atto→mega migration.

### Adding an output backend

Four places: a `FileFormat` constructor + `parseFileFormat` case + `fileExtensionMappings`
entry in `app/Options.hs`; an `outputTo` clause in `app/Main.hs`; the module under
`src/DMN/Translate/`; and `exposed-modules` in `dmnmd.cabal`. That last one is easy to forget
and fails late — nothing auto-discovers modules since hpack went.

## The L4 backend (`src/DMN/Translate/L4.hs`)

The newest and most constrained backend. `BUILD-SPEC-dmnmd-to-l4.md` is its design source
of truth and the code's comments cite its sections (§1.2, §3, §9.6…) — read the spec section
before changing behaviour it pins. That spec is **discharged**: it is retained because the
section numbers are load-bearing in `L4.hs`'s comments, not because anything in it is still
to be done. Its imperative sections 4 and 5 describe work already completed. It carries a
status header saying so.

- Emits `GIVEN`/`GIVETH` + a first-match `BRANCH` closed by a synthesized `OTHERWISE`.
  Multi-output tables get a `DECLARE` record plus a `mk<Name>` constructor.
- `Collect`/`Aggregate`/`OutputOrder`/`RuleOrder` `error` out deliberately: they are
  list-valued and a scalar `BRANCH` would silently drop matches. `Priority` reorders arms
  through `outputOrder` (the same function `evalTable` uses) rather than row order.
- With no all-wildcard catch-all row, `OTHERWISE` must **not** reuse the last data row's
  output — it falls back to `defaultResult` or a typed sentinel.
- **The ditto grid is column-alignment-critical.** `renderDittoGrid` collapses a guard token
  to `^` when it repeats the token directly above; the L4 lexer resolves `^` by *absolute
  source column*, so widths must be measured with `displayWidth`, which defers to
  megaparsec's `isWideChar` — the lexer's own table. Do not substitute a hand-rolled
  East-Asian-Width table; a one-column drift makes a caret copy the wrong token silently.
  A `^` copies exactly one token, so multi-token cells (an `OR`-of-`EQUALS` chain) are never
  dittoed.
- Column/table/field names that are L4 keywords get backtick-quoted (`reservedWordsL4`).

### The golden test shells out to an external binary

`test/TranslateL4Spec.hs` runs `l4 check` and `l4 run` over the emitted output, because the
golden gate is **semantic** — does it typecheck, do the `#ASSERT`s pass — rather than a
byte-exact diff against the hand-written `test/golden/miles-card.l4`. `l4 run` exits 0 even
on a failed assertion, so the test greps stdout for `assertion satisfied` / `assertion failed`.

It finds `l4` via `$L4_BIN`, else `findExecutable "l4"` on `PATH`, and when neither yields
anything it marks those examples **pending** rather than failing them — `l4` is not a build
dependency of dmnmd (the emitter is a pure `DecisionTable -> String` function) and does not
exist on the CI runner, so a missing toolchain must not be reported as an emitter regression.

This paragraph used to say the path was hardcoded to `/Users/mengwong/.local/bin/l4`. That was
true until `58a5f49`, which added the lookup; the doc was not updated, and the stale claim was
then copied into two more places before anyone read `findL4`. If a `TranslateL4Spec` example
fails on another machine it is a real failure, not a missing binary — a missing binary is
pending, and says so.

## Diagnostics and exit status

One rule governs both readers:

> Anything we parse but do not yet honour must produce a loud, located diagnostic. Never
> silently discard. Accepting input and quietly giving a different answer is strictly worse
> than rejecting it.

So `DMN.XML.XmlToDmnmd` returns `([Diagnostic], [DecisionTable])` rather than calling
`error`. A `Warning` means something was dropped and says what (`<defaultOutputEntry>`, the
`<annotation>` column names, a cell that is not a plain FEEL literal). An `Error` means a
table could not be represented faithfully — a temporal `typeRef`, a rule whose entry count
disagrees with the column count, a cell that cannot be built at the column's type — and
that table is **not emitted**, because a table that can never match, or one whose rules have
been silently widened, is a wrong answer that exits 0.

The markdown reader gets there differently, and deliberately. It has no reason to refuse one
table and carry on, so it raises through `error` — but a refusal from the cell layer now
carries a `DMN.DecisionTable.CellSite` and reads
`error: table "T": column "C": row N: …`, the same shape `structuralErrors` and
`domainErrors` already print. `mkFsAt`/`mkFAt` are the located wrappers; `mkFs`/`mkF` stay
for the XML reader (which frames its own) and for the test suite. `row N` is the rule number
the **author wrote** in the leftmost cell, so gaps and repeats survive into the message; the
XML reader stores a 1-based index in the same field, so the two readers mean different things
by "row". `Nothing` there is the sub-header row and prints no row segment at all. No file name
on the markdown path — `parseTable` is not given one, and the `CellSite` haddock records what
that would cost.

Three differences between the two readers' output survive, and only two are on purpose. The
file name and the missing in/out word are reasoned (the latter because `reviseInOut` can
relabel an explicitly-`(in)` column to `out`, so the word would sometimes contradict the
header). The third is not: **the markdown path still prints a Haskell `CallStack` and a
four-frame `HasCallStack backtrace:` of ghc-internal positions, and it is the only
user-facing abort in the tool that does.** `app/Main.hs:130` defines
`crash = errorWithoutStackTrace` and every other abort goes through it, so
`dmnmd -f xml -t ts test/dmn13/temporal-type.dmn` ends on a clean located line. This is not a
leftover to tidy in passing: that `CallStack` position is currently the **only** discriminator
between the `mkFsAt` and `mkFAt` recordings, including the `num-subheader-*` pair the corpus
README cites. Removing it needs the discriminator replaced first — it belongs to the
diagnostics conversion (`DECISIONS.md` D-7), not to a cleanup commit.

The exit status answers exactly one question: *did something we were asked to read fail to
read?*

| input | status |
|---|---|
| valid DMN 1.3/1.4/1.5 with decision tables | 0 |
| valid DMN with no `<decision>` (`test/simple.dmn`) | 0 |
| markdown with decision tables | 0 |
| markdown with no decision tables — prose, or prose pipe tables (`test/golden/README.md`) | 0 |
| malformed XML, or DMN 1.1/1.2 | 1 |
| a DMN 1.4/1.5 construct dmnmd does not model — refused by name before unpickling | 1 |
| a document mixing two releases' namespaces | 1 |
| a table refused by the converter | 1 |
| a table whose cell violates its own declared domain — either reader | 1 |
| markdown where *some* tables parsed and others did not | 1, and nothing is emitted |

A pipe table whose top-left cell is not a hit policy is prose, not a broken decision table:
`ParseMarkdown.isDecisionTable` asks `parseHitPolicy` itself, skips the chunk, and says so
on stderr as a `note:`. That is why a README full of documentation tables exits 0.

## The behavioural corpus (`languages/haskell/test/corpus/`)

The hspec suite **cannot** certify a change to the cell layer, because parts of it
assert the bugs as expected behaviour — `DmnXmlSpec.hs` expects `FNullary (VS "not(\"Fall\"")`,
which is the comma-split defect frozen into an expectation, and the `type inference` block in
`Spec.hs` passes *because* the inference regexes are unanchored. A green suite therefore cannot
distinguish "I preserved the behaviour" from "I preserved the bug".

`test/corpus/` exists to make that distinction. It records what the binary actually does
today — stdout, stderr **and** exit status — one small input per verified defect, and puts
every case in one of two directories:

- **`cases/policy/`** — behaviour that is right and must not change. A diff is a **regression**;
  fix the code, not the recording.
- **`cases/symptom/`** — behaviour that is wrong and should change. A diff is **progress**;
  read it, re-record, and `git mv` the case into `cases/policy/` once it is correct rather than
  merely different. A fixed symptom left in `cases/symptom/` is unprotected.

```
cd languages/haskell && cabal build     # the runner does not build
make corpus            # everything; exits nonzero only on a policy regression
make corpus-policy     # just the regression net
make corpus-list       # what is in there
make corpus-record     # re-record. Read the diff first — always.
```

CI runs `make corpus` as its own step. Read `test/corpus/README.md` before adding or
reclassifying a case; the classification is the entire value of the directory, and the two ways
to get it wrong are asymmetric but both bad. A real bug filed under `policy` makes a correct fix
look like a regression; intended behaviour filed under `symptom` lets a real regression through
in silence.

Three things there are easy to get wrong on sight:

- **The package unit id is normalised away, and must be.** A `CallStack` frame names the unit
  that raised it, and that spelling is a property of how the package was built: an in-place
  build writes `dmnmd-0.1.0.2-inplace`, an *installed* one writes a content hash over the
  dependency closure — and the runner's `PATH` fallback will happily run such a binary. Without
  the rule a recording only reproduces under the build that made it. It first bit as 14 red
  recordings with zero behavioural content, back when CI built with stack; stack is gone but
  the rule is not stack-specific, and Linux CI reproducing macOS arm64 recordings is the
  evidence it earns its place.
- **Source positions are kept, not normalised.** `mkFsAt` and `mkFAt` are the multi-value and
  single-value cell paths, and several cells produce byte-identical message text down both, so
  the position is the only discriminator. `policy/num-subheader-{declared,inferred}-refused`
  is the pair that pins it: identical text, different wrapper. Instead of stripping positions,
  a diff consisting of *nothing but* moved ones is reported as `cosmetic` and does not fail the
  run. Do not write the line numbers down anywhere — this bullet asserted `:121` and `:143` long
  after both had moved. (An earlier retraction added that `:143` "was never right". That is
  false: at `a670657`, the commit that wrote the sentence, `:121` was `mkFs`'s body and `:143`
  was `mkF`'s, and eight and six recordings cited them respectively. Both were exact when
  written and merely went stale — which is the whole argument for not writing them down, and is
  a *weaker* claim than the one that replaced it. Correcting a stale claim into a false one is
  worse than leaving it: rule 2 of `~/CLAUDE.md` names this exact move.)
- **Commit 6 inverted which classes cite a cell-path position.** Eleven `policy/` recordings now
  cite `mkFsAt`/`mkFAt`, where before the promotion those positions appeared only under
  `symptom/`. The runner still classifies a position-only diff as `cosmetic`, so this cannot
  produce a false regression — but an edit anywhere above those functions now dirties eleven
  policy recordings, where it used to dirty none.
- **The runner falls back to `dmnmd` on `PATH`** if it finds no build product, which silently
  tests whatever you last `cabal install`ed. It warns when it does this; read the
  `corpus: using …` line before believing a failure.

The runner fails closed: a missing `STDIN_FILE`, unparseable `ARGS`, a `case.conf` that does not
source, a case in an unrecognised class directory, a bad `WORKDIR`, or a `--only` glob matching
nothing are all refused rather than passed. Each was a real path to a green run against a wrong
baseline. `--record` checks them all before writing anything.

## Known-broken, don't be surprised

`BUILD-SPEC-dmnmd-extensions.md` §1 recorded probes against the tree **as of 2026-07-25**, and
PR #17 has since moved several of them — treat it as history, not as current behaviour, and
prefer `test/corpus/`, which is machine-checked. The items below are current:

- **The executable is not covered by `-Werror=incomplete-patterns`.** The flag is on the
  `library` stanza only, so `app/`'s partial functions still fail at run time — `showToJSON`
  is the live example, recorded as `symptom/cli-showtojson-*`.
- **`--from=xml` reads DMN 1.3, 1.4 and 1.5; `--to=xml` is not implemented at all**, despite
  `Xml` existing in `FileFormat`. The reader is deliberately strict — an element or attribute
  the XSD does not allow in that position is an error, and a DMN 1.1/1.2 document is refused
  by namespace with a message naming the version. Fixtures live in `test/dmn13/` and
  `test/dmn15/`; each README says which refusal each one exercises.

  **The namespace is a parameter, not a constant.** `DmnRelease` (`ParseDMN.hs`) is a name
  plus *two independent* URIs — model and DMNDI — because DMN 1.4 pairs a 1.4 model
  namespace with the **1.3** DMNDI one, so a date-into-a-template scheme is wrong on its
  first use. It is resolved once by `checkDmnRoot` and threaded through the pickler tree by
  `DmnPU`, a project-local replacement for hxt's `XmlPickler` whose method takes the release
  (hxt's `xpickle :: PU a` is a value with nowhere to put it). Adding DMN 1.6 is one record —
  *provided* its decision-table complex types are still byte-identical, which is the property
  that lets one tree serve every release and must be re-measured, not assumed.

  **What 1.4/1.5 add is refused by name, not by `xpCheckEmptyContents`.** `refuseUnmodelled`
  scans the tree before unpickling and names the element, what it is, the release that
  introduced it, and the `<decision>` or `<itemDefinition>` it sits under. Six names:
  `conditional`, `for`, `some`, `every`, `filter` — the boxed expressions, added in **1.4**,
  not 1.5 — plus `typeConstraint`, the only structural change 1.5 makes over 1.4. Five boxed
  names and not seven: `tIterator`/`tQuantified` are abstract and
  `tChildExpression`/`tTypedChildExpression` have no global element, so `<iterator>` and
  `<quantified>` cannot be written in a document at all.

  It is a pre-flight rather than an arm inside the picklers for three reasons, and
  `typeConstraint` is the one that settles it: `ItemDefinition` filters its children by name,
  and a name filter *deletes* what it does not list, so that element never reached the
  unpickler and was dropped in **silence** — the case the governing rule above calls strictly
  worse than rejection. The other two: all five boxed expressions substitute for `expression`,
  which the XSD writes in seven positions and dmnmd models one of; and a pre-flight needs
  nothing from hxt that `Text.XML.HXT.Core` does not re-export.

  `unmodelledConstructs` is the extension point. The DMN **1.3** boxed expressions dmnmd has
  never modelled — `<context>`, `<invocation>`, `<relation>`, `<list>`, `<functionDefinition>`
  — belong there too and still produce a generic error today.
- **Multi-table Markdown works — `test/safe.md` is a bad fixture, not a chunking limit.**
  Its file-level failure is a **missing final newline** (the last byte is `|`); append one
  and `grepMarkdown` succeeds and 3 of its 13 tables import. Also, the reported position is
  not where the problem is — the failure surfaces at `75:1` while the defect is at EOF,
  because `try` backtracks the failed table and the error resurfaces from the fallback
  parser some lines earlier. **Check for a final newline before trusting a `grepMarkdown`
  position.** The 10 tables that still fail after the fix are one-column tables that the
  grammar has no construct for; that is a separate, open gap.
- **CI was red from 2025-06-29 until the `feat/translate-l4` CI fixes.** Two stacked
  causes, both environmental: the workflow never installed `libpcre3-dev`/`pkg-config` (so
  `regex-pcre` failed to configure before any project code compiled), and the deprecated
  `haskell/actions/setup` resolved `latest` to stack 2.11.1, whose bundled Hackage TUF keys
  can no longer validate `root.json`. If CI goes red again, check whether project code was
  even reached before assuming a regression. **The first cause can no longer recur**:
  `regex-pcre` is gone and CI installs no system packages at all. The general lesson stands —
  a dependency that fails to *configure* fails before anything you wrote is compiled, and
  reads as a mysterious error in a package nobody here maintains.
