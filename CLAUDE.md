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
stack build
stack test
stack run -- --to=l4 path/to/table.md      # or: stack exec -- dmnmd README.md --to=ts -r
stack install                               # puts dmnmd on PATH
```

Both stack (`stack.yaml`, lts-24.20) and cabal (`dist-newstyle/`) are used in practice, and
both are on **GHC 9.10.3 with megaparsec 9.7.0** — keep them aligned. `DMN.Translate.L4`
imports `Text.Megaparsec.Unicode (isWideChar)`, which does not exist before megaparsec
9.7.0, so an older resolver silently makes the L4 backend uncompilable under stack while
cabal keeps working. CI runs `stack test`.

The cabal file is generated from `package.yaml` by hpack — **edit `package.yaml`**, not
`dmnmd.cabal` (stack regenerates it; a hand-edit to the `.cabal` will be overwritten).

macOS needs `brew install pkg-config pcre` for `regex-pcre` (Linux: `libpcre3-dev`).
`stack.yaml` also carries a `nix: pure: true` stanza supplying those.

Single test / focused runs (hspec, via `--test-arguments`):

```
stack test --ta '--match "renders FInRange"'
stack test --ta '--match "/DMN.Translate.L4.toL4/"'   # a whole describe block
```

Watch loops and fixture refresh live in the `Makefile`:

```
make ghcid          # ghcid over the library
make tests-watch    # stack build --file-watch --test, rerunning failures first
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
  re-runs `mkF` over the cells (`reprocessRows`). So a cell's `FEELexp` shape depends on a
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
- **The parser is megaparsec.** `DMN/ParsingUtils.hs` holds attoparsec-shaped shims
  (`many1`, `anyChar`, `notChar`, `parseOnly`) left over from an atto→mega migration.

### Adding an output backend

Four places: a `FileFormat` constructor + `parseFileFormat` case + `fileExtensionMappings`
entry in `app/Options.hs`; an `outputTo` clause in `app/Main.hs`; the module under
`src/DMN/Translate/`; and `exposed-modules` in `package.yaml`.

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

The exit status answers exactly one question: *did something we were asked to read fail to
read?*

| input | status |
|---|---|
| valid DMN 1.3 with decision tables | 0 |
| valid DMN 1.3 with no `<decision>` (`test/simple.dmn`) | 0 |
| markdown with decision tables | 0 |
| markdown with no decision tables — prose, or prose pipe tables (`test/golden/README.md`) | 0 |
| malformed XML, or DMN 1.1/1.2 | 1 |
| a table refused by the converter | 1 |
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
  that raised it, and the two build tools spell it differently for identical source: cabal writes
  `dmnmd-0.1.0.2-inplace`, stack writes a content hash over the dependency closure. Since CI
  builds with stack and most local work here is cabal, without this rule the corpus is red on
  whichever toolchain did not record it — 14 recordings, zero behavioural content.
- **Source positions are kept, not normalised.** `DecisionTable.hs:121` is `mkFs` and `:143` is
  `mkF`, and several cells produce byte-identical message text down both paths, so the line
  number is the only discriminator. Instead of stripping them, a diff consisting of *nothing but*
  moved positions is reported as `cosmetic` and does not fail the run.
- **The runner falls back to `dmnmd` on `PATH`** if it finds no build product, which silently
  tests whatever you last `stack install`ed. It warns when it does this; read the
  `corpus: using …` line before believing a failure.

The runner fails closed: a missing `STDIN_FILE`, unparseable `ARGS`, a `case.conf` that does not
source, a case in an unrecognised class directory, a bad `WORKDIR`, or a `--only` glob matching
nothing are all refused rather than passed. Each was a real path to a green run against a wrong
baseline. `--record` checks them all before writing anything.

## Known-broken, don't be surprised

`BUILD-SPEC-dmnmd-extensions.md` §1 recorded probes against the tree **as of 2026-07-25**, and
PR #17 has since moved several of them — treat it as history, not as current behaviour, and
prefer `test/corpus/`, which is machine-checked. The items below are current:

- **`--from=xml` reads DMN 1.3 only; `--to=xml` is not implemented at all**, despite `Xml`
  existing in `FileFormat`. The reader is deliberately strict — an element or attribute the
  vendored `xsd/DMN13.xsd` does not allow in that position is an error, and a DMN 1.1/1.2
  document is refused by namespace with a message naming the version. Fixtures live in
  `test/dmn13/`; its README says which refusal each one exercises.
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
  even reached before assuming a regression.
