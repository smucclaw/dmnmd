# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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
before changing behaviour it pins.

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

### The golden test needs an external binary

`test/TranslateL4Spec.hs` shells out to a **hardcoded absolute path**,
`/Users/mengwong/.local/bin/l4`, to `l4 check` and `l4 run` the emitted output — the golden
gate is semantic (does it typecheck, do the `#ASSERT`s pass), not a byte-exact diff against
the hand-written `test/golden/miles-card.l4`. Without that binary those examples fail; a
`stack test` failure in `TranslateL4Spec` on another machine is usually this, not a
regression. `l4 run` exits 0 even on a failed assertion, so the test greps stdout for
`assertion satisfied` / `assertion failed`.

## Known-broken, don't be surprised

`BUILD-SPEC-dmnmd-extensions.md` §1 records probes against the current tree:

- **XML is aspirational.** `--from=xml` parses but imports 0 tables; `--to=xml` is not
  implemented at all, despite `Xml` existing in `FileFormat` and `DMN.XML.*` existing in
  the library. Treat any claim that dmnmd "supports DMN XML" as unverified.
- **Multi-table Markdown works — `test/safe.md` is a bad fixture, not a chunking limit.**
  Its file-level failure is a **missing final newline** (the last byte is `|`); append one
  and `grepMarkdown` succeeds and 3 of its 13 tables import. Also, the reported position is
  not where the problem is — the failure surfaces at `75:1` while the defect is at EOF,
  because `try` backtracks the failed table and the error resurfaces from the fallback
  parser some lines earlier. **Check for a final newline before trusting a `grepMarkdown`
  position.** The 10 tables that still fail after the fix are one-column tables that the
  grammar has no construct for; that is a separate, open gap.
- The `~/.local/bin/dmnmd` shim on this machine is broken (`libpcre.1.dylib` not loaded);
  run the cabal/stack build output directly.
- **CI was red from 2025-06-29 until the `feat/translate-l4` CI fixes.** Two stacked
  causes, both environmental: the workflow never installed `libpcre3-dev`/`pkg-config` (so
  `regex-pcre` failed to configure before any project code compiled), and the deprecated
  `haskell/actions/setup` resolved `latest` to stack 2.11.1, whose bundled Hackage TUF keys
  can no longer validate `root.json`. If CI goes red again, check whether project code was
  even reached before assuming a regression.
