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
anchored digit test, i.e. `isInfixOf` and five lines of `span isDigit`. (Those nine are gone too
as of D-2 — inference now asks `DMN.ParseCell` — but that is a later change and this paragraph is
about the dependency, not about inference.) `shell.nix` is now an empty shell kept only as the
machine-readable place to record that.

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
  → DMN/Translate/*.hs     JS.hs (serves both --to=js and --to=ts), PY.hs, L4.hs,
                           XML.hs (--to=xml, which pickles back through DMN/XML/ParseDMN.hs)
```

`app/Main.hs` is the driver: refuse an unwritable `--to` → parse tables → `--pick` filter →
either `renderAll` a backend or `runInputT` an interactive eval REPL (`-q`).

**`renderAll` produces a `String` and `withOutHandle` opens `-o` only to write it, in that
order.** `openFile … WriteMode` truncates, so the previous order — open, then read, then render —
destroyed the user's output file on every failing run, including a run that printed "refusing to
emit … because partial output is indistinguishable from complete output" while the file was
already empty with a fresh mtime. Recorded as
`policy/cli-out-not-opened-until-render-succeeds`. Likewise `checkOutFormat` runs before
`parseTables` and consults ONE list (`implementedOutFormats`); the refusal used to live inside the
per-table loop, so it fired only if the input happened to contain a table.

Things that are only apparent across several files:

- **Type inference is a second pass, and it is allowed to give up.** `parseTable` first parses
  every cell as a string; `mkDTable` then infers each column's `DMNType` from the whole column
  (`inferTypes`) and re-runs `mkFAt` over the cells (`reprocessRows`). So a cell's `FEELexp` shape
  depends on a type that isn't known until the table is fully parsed. Explicit `Column : Number`
  headers short-circuit this — `inferTypes` tests the declared type **first** and returns early,
  which is an explicit guard now and used to be an accident of the disagreement branch.

  **It is anchored (D-2): the oracle is `DMN.ParseCell.parseNumberCell`,** the same function the
  declared path uses, so the two cannot drift — the pattern `domainErrors` already follows with
  `fEval`. `inferEvidence` grades one cell and `columnVerdict` aggregates, in three tiers that all
  matter: hard evidence (a number, comparison, interval, boolean word, quoted string); **weak**
  evidence (arithmetic, which decides a column only when nothing harder spoke, because
  `parseNumberCell`'s arithmetic arm accepts bare FEEL names and so accepts `Non-Participating`
  and `n/a`); and none (a wildcard).

  **A `Left` from the oracle is not always "not a number".** Its two NAMED refusals — FEEL
  negation and invocation — mean "unmistakably numeric, and dmnmd does not implement it", so
  `DMN.ParseCell.namedRefusal` reads them back as Number evidence. Without that, `not([1..5])`
  types its column `String` and the refusal becomes an equality test against literal text at
  exit 0.

  **A column that cannot be resolved is a located error, and lives in `inferenceErrors`** beside
  `structuralErrors` and `domainErrors` — not in `inferTypes`, which cannot name a row because
  `mkDTable` transposes the rows into columns and drops the rule numbers on the way. Two ways to
  fail: cells that **disagree** (`VConflict`), and a cell that reads as a number it does not spell
  (`VAmbiguous`, today only a redundant leading zero — `007`). A column with **no** evidence at all
  (`VNone`, an all-wildcard column) is a legitimate shape and stays silent: that distinction is the
  whole predicate, and `policy/infer-all-wildcard-column-silent` is the only thing pinning it.
- **`FEELexp` is the cell IR** for both inputs and outputs: `FSection` (a comparison
  section like `<= 8`), `FInRange` (`[5..8]`), `FAnything` (`-`), `FNullary` (a literal),
  `FFunction` (arithmetic like `age * 100`). Cells are `[[FEELexp]]` — the inner list is a
  multi-value cell (`Fall, Winter`), the outer list is the columns.
- **A number is a `Scientific`, and `DMN.Number` is the only module allowed an opinion about
  one.** `VN` holds an arbitrary-precision decimal because FEEL's number *is* decimal128
  (DECISIONS.md D-1); binary32 could hold neither `16777217` nor `1234567.89`, both at exit 0.
  Everything that follows lives in `src/DMN/Number.hs`, because before it there were five
  independent number-to-text sites and they disagreed:
  - **`show` is never the right renderer.** `show @Scientific` inherits `show @Float`'s
    fixed-vs-exponent rule, so it spells `16777217` as `1.6777217e7` — the right value with text
    the author never wrote, valid TypeScript, caught by nothing. `showNumPlain` (L4 + every
    diagnostic that quotes a cell back) and `showNumFloatish` (js/ts/py, which keeps the `.0`, so
    a generated Python value stays a `float`) both `formatScientific Fixed Nothing` instead. L4 is
    not merely stylistic here: it has no exponent production, so `1.0e8` lexes as `1.0` applied to
    a variable named `e8`.
  - **Arithmetic can fail, and `fNEval` returns `Either`.** `Scientific` has no `Floating`
    instance, and its `/` raises a bare library `error` on a repeating decimal. `divideFeel`
    computes the quotient as an exact `Rational` and rounds half-to-even to 34 significant digits;
    `powerFeel` is exact at an integral exponent and refuses a fractional one rather than routing
    through `Double`. Division by zero and an unspellable magnitude are refusals, where `Float`
    answered `Infinity` at exit 0 — which the L4 backend then printed as a plain `0`.
  - `ParseCell.numericLiteral` (S-FEEL rule 31, refuses `1e5`) and `ParseFEEL`'s
    `guardedScientific` (megaparsec's `Lexer.scientific`, accepts exponents) are **two grammars on
    purpose**. Do not unify them. And never reach for `realToFrac` to get *into* a `Scientific`:
    from a `Double` it is exact and therefore catastrophic, turning `Age * 0.1` into a 55-digit
    literal. `fromFloatDigits` is the conversion; the tree currently needs neither.
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

  **"Bypasses `mkDTable`" is not "is unaffected by inference", and D-2 widened the gap.**
  `XmlToDmnmd` calls `inferTypes` directly (`:408`) for any column whose `<inputExpression>`
  has no `typeRef` — the XSD makes it optional — and calls `tableErrors` (`:199`), which now
  contains `inferenceErrors`. So an inference change reaches the XML path in full: a DMN
  document with an untyped column and disagreeing cells went exit 0 to exit 1 under D-2.
  That is the right outcome (trunk read a *number* cell as literal text and emitted
  `shade === "5"`), but no fixture omitted an `<inputExpression>`'s `typeRef`, so nothing in
  the tree covered it until `test/dmn13/no-typeref-inferred.dmn` and
  `policy/xml-untyped-column-inference-refused`. (The qualifier is load-bearing:
  `output-without-label.dmn` has always omitted it on the `<output>` side. `test/dmn13/README.md`
  carried the same claim unqualified, where it was flatly false, and is corrected there.)
  **Do not assume a change to `DecisionTable` inference is markdown-only.**

  **A single-output column is typed by the DECISION, not by the `<output>` clause, and that is
  the second-largest thing that stops inference running.** DMN 1.3 **§8.3.2**, Table 34: "The
  OutputClause of a single output decision table SHALL NOT specify a typeRef", and likewise SHALL
  NOT specify a name. A single-output table states its result type on the enclosing
  `<decision>`'s own `<variable>` instead — "the instance of InformationItem that **stores the
  result of this Decision**" — so for such a table that is not merely *a* place the type may
  appear, it is the only one left. (**Mind that number.** l4-ide's `Emit.hs` cites §8.2.11, and
  D-13 established against the OMG PDF that §8.2.11 is *Default output values* — the miscite this
  repo has already corrected twice. §8.3.2 is read out of `~/Documents/omg-specs/DMN-1.3.pdf`;
  the fetch pattern is `https://www.omg.org/spec/DMN/<version>/PDF`, undocumented and not linked
  from any About page. Worth telling the l4-ide side.) The XSD cannot say
  so — `tOutputClause` declares both attributes unconditionally — so such a document validates
  either way and **only the reader can get it wrong, silently, at exit 0**. dmnmd used to get it
  wrong: `<variable>` was an `xpIgnoredElemOpt` beside `<question>` and `<allowedAnswers>`, so
  a declared type was parsed and thrown away and the column was inferred instead.
  `ParseDMN.Decision` now keeps it as `decVariable`, `convdec` hands its `typeRef` to
  `convTable`, and `convTable` applies it **only when there is exactly one output** — with two
  or more, each `<output>` is keyed by name and carries its own `typeRef`, and the variable
  names a composite whose type is an `<itemDefinition>` and not any one column's. A `typeRef`
  actually present on the clause still wins. Pinned from both sides by
  `policy/xml-decision-variable-types-single-output` and its negative control
  `policy/xml-decision-variable-not-applied-multi-output`.

  This is not a hypothetical shape. Measured over 355 DMN documents exported from
  `legalese/l4-ide`: `<inputExpression>` carries `typeRef` **429 of 429** times and `<output>`
  **0 of 271** — the exporter omits it deliberately and says why in
  `jl4-core/src/L4/Dmn/Emit.hs` (KIE reports `ILLEGAL_USE_OF_TYPEREF` on one that has it). On
  that corpus the change turned **8 of the 36** documents that emitted anything from a wrong
  answer at exit 0 into either a refusal naming the cell (6: a declared `number` column whose
  cells dmnmd cannot read, previously emitted as *quoted strings* —
  `{"output1":"1000 + (if a.isVeteran then 250 else 0)"}`) or a corrected one (2: a FEEL string
  literal `"yes"` against a declared `string`, previously emitted as the boolean `true`).

  > That first parenthesis said "cells were **arithmetic** dmnmd cannot read", which is rule 2 of
  > `~/CLAUDE.md` broken in the act of copying: `DECISIONS.md` says "expressions dmnmd cannot
  > read" and the narrowing was added here. Only 4 of the 6 are arithmetic
  > (`1000 + (if …)`, `500 + h.dependents * 100`). The other two are a bare name —
  > `ok/fixity-nary-guard.l4`, cells `t` and default `e` — and an invocation,
  > `openfisca/housing.l4`'s `max OF 200`. A reader hunting a related defect in the arithmetic
  > parser would have found nothing for a third of the cases.

  **Two traps this fallback fell into, both found only by adversarial review after it landed.**
  A collection's element type is wrapped with `DMN_List <$> ty`, so a `Nothing` that carries no
  diagnostic silently *deletes* the `isCollection` declaration — and `Any` was the first
  diagnostic-free `Nothing` in `convertType`. A collection of `Any` is therefore **refused**, not
  inferred. And the fallback does not apply under a list-valued hit policy (bare `C`, `R`, `O`),
  where the variable types the result *list* rather than the column; the four Collect aggregations
  are single-valued and unaffected. `policy/xml-collection-of-any`,
  `policy/xml-collect-variable-is-list`.

  **`typeRef="Any"` is a declaration that declares nothing, and infers.** It is FEEL's top
  type, so it says exactly what an absent `typeRef` says. It is deliberately not routed to the
  unknown-typeRef refusal, whose reasoning does not reach it: that refusal exists because a
  type dmnmd cannot model names a domain it would then misread — a `date` column read as a
  string turns every guard into a comparison that can never match — and `Any` names no domain.
  It only became reachable often once `<variable>` stopped being ignored, and it is reached
  often: **136 of those 355** documents declare it, being what the l4-ide exporter writes
  wherever an L4 type has no DMN counterpart. `policy/xml-any-typeref-infers`.
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
  domain `[0..150]` unwritable, because `reprocessRows` calls `mkFAt` with the full column
  type on live paths, so a `Left` there refuses ordinary tables. (Before D-7 it *crashed*
  them — the located wrappers were `either error id` underneath. They now return a
  `Diagnostic` and the table is dropped instead: different blast radius, same defect.)
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
entry in `app/Options.hs`; a `renderAll`/`renderOne` clause in `app/Main.hs`; the module under
`src/DMN/Translate/`; and `exposed-modules` in `dmnmd.cabal`. That last one is easy to forget
and fails late — nothing auto-discovers modules since hpack went. A fifth, easy to miss: add the
constructor to `implementedOutFormats`, or the up-front check refuses the format you just built.

`renderAll` is per FILE and `renderOne` is per table. L4 and XML are file-level (one L4 scope, one
`<definitions>`); js/ts/py are a run of independent function definitions.

## The XML backend (`src/DMN/Translate/XML.hs`)

`--to=xml`, ruled on as `DECISIONS.md` D-8 and landed there. The only backend whose output is read
by something other than a programming-language toolchain, which gives it a failure mode none of the
others has: **it can emit a well-formed, XSD-valid document that says the wrong thing**, and no
validator catches that.

- **It does not write XML. It builds a `ParseDMN.Definitions` and lets the READER's picklers run
  backwards.** hxt picklers are bidirectional, so `dmnPickler` is both halves. A hand-rolled writer
  would be a second description of DMN's element structure, free to drift from the reader's; there
  is only one description, so it cannot. What this module owns is a pure
  `DecisionTable -> Definitions` mapping and the cell language.
- **Two hxt facts, both load-bearing, neither visible in the picklers.** hxt drops namespace
  declarations when pickling — `xpElemNS` builds a universal name and the writer emits the
  qualified one — so `xmlns` is added to the root as an ordinary attribute after `pickleDoc`;
  without it dmnmd cannot read its own output. And `ShowXml.xshow` **does not escape**: it is the
  tree printer, not the document writer, and the first whole-file output was literally
  `<text><= 0</text>`. `escapeXmlRefs` (hxt's own table) is applied to the tree as a pure list
  arrow, which is what keeps the backend a `… -> String` function.
- **DMN 1.3, with the release as an `XMLOpts` field and no CLI flag.** `xsd/` stops at 1.3, so 1.3
  is the only release whose output this repo can validate; a `--to=xml15` would ship an
  unverifiable capability. The parameter exists (D-4's point) and a flag is one line once a DMN14
  or DMN15 schema arrives.
- **A wildcard is spelled differently on the two sides of a rule.** `-` is DMN 1.3 §9.2 rule 12
  syntax: legal in an `<inputEntry>` (a `tUnaryTests`) and meaningless in an `<outputEntry>` (a
  `tLiteralExpression`). An output wildcard becomes an EMPTY `<text/>`, which dmnmd reads back as
  the same `FAnything` and which another engine reads as null — so it warns.
- **Row comments go to `<description>` first, `<annotationEntry>` after.** Dictated by the reader,
  which builds `row_comments` as `description : annotationEntries`. Not cosmetic: the reader maps
  both `<annotationEntry/>` and `<annotationEntry><text/></annotationEntry>` to `Just ""`, so
  routing a comment column through annotations turns every comment-LESS row into an EMPTY comment.
- **A collection column forces a synthesized `<itemDefinition>`.** DMN has no column-level
  `isCollection`; it is an `<itemDefinition>` attribute. This is the only element in the document
  with no markdown counterpart, and the invention is confined to its name.
- **Each `<decision>` gets a `<variable>`, typed from the single output column.** DMN §8.3.2 puts
  a single-output table's result type there, so a specification-following consumer looks at the
  variable and not at `<output>/@typeRef` — and until `decisionOf` emitted one, dmnmd stated the
  type in only the place such a consumer ignores. Its `name` repeats the decision's, which is the
  DMN convention. With two or more outputs it is emitted with a name and **no** `typeRef`, because
  the variable then names a composite whose type would be a synthesized `<itemDefinition>` dmnmd
  does not build. A collection output names the `dmnmd_list_of_*` type, which `collectionItemDefs`
  already declares (it walks `header`, inputs *and* outputs), so the reference never dangles.
  `<output>/@typeRef` is still written as well: dropping it is what §8.3.2 and KIE actually ask
  for, but it is what every version of this backend has emitted, so that is a deliberate change
  and not a side effect of this one.
- **Refused, because DMN has no document for them:** a table with no output column
  (`tDecisionTable` is `output+`), a row with fewer cells than columns (padding with `-` would
  WIDEN the rule silently), a comparison in an output cell, and `HP_Aggregate`.
- **A trailing catch-all row in a `U` table is PROMOTED to a default output value (D-16 phase 2).**
  A row with `-` in every input column overlaps every other rule, which §8.2.10 says a `U` table
  must not contain — but the shape is idiomatic (**40 of 221** corpus fixtures, the README example
  among them), dmnmd's own matching is first-match, and `--to=l4` renders it as `OTHERWISE`. DMN's
  construct for the intent is the default output value of §8.2.11, and since phase 2 that is what
  `promoteTrailingCatchAll` emits: the row's outputs move into `dtDefaultOutput` →
  `<defaultOutputEntry>`, the `<rule>` is dropped, and the document is genuinely `U`-conformant for
  any engine, ordered or not. A warning still names the row (its authored NUMBER is genuinely
  lost — a default has no rule number), and it lives HERE, not in `tableWarnings`: put there in
  phase 1, it added a paragraph to 33 policy recordings about a problem the js/ts/py/l4 paths do
  not have. Eligibility is narrow (trailing, comment-free, full arity, not all-wildcard outputs, no
  default already present); an ineligible catch-all keeps a phase-1-style warning that says why it
  was not promoted. `dtDefaultOutput` itself is honoured everywhere: the reader carries a declared
  `<defaultOutputEntry>` (it used to warn-and-drop), `evalTable` answers it exactly when no rule
  matches under a single-hit policy, js/ts/py render it via `rowsPlusDefault` as the trailing arm
  it is equivalent to, and L4 feeds it to `OTHERWISE` — which is what keeps the round trip
  byte-identical on both the ts and l4 legs. Pinned by `policy/hp-unique-catchall-promoted`,
  `policy/xml-default-output-entry-carried`, negative control `policy/hp-first-catchall-not-warned`,
  and the promotion block in `TranslateXMLSpec`.

### The gate is a round trip, not a golden file (`test/roundtrip/`)

dmnmd already READS DMN, so the emitter can be checked with no hand-written expectation at all:

```
cabal build                                # neither script builds
./test/roundtrip/run-roundtrip.sh --xsd    # F --to=xml | --from=xml --to=ts  ==  F --to=ts
./test/roundtrip/backend-baseline.sh --check   # did any OTHER backend move?
```

131 of 183 fixtures pass byte-identically (42 skipped as recorded refusals, 10 XFAILs each with a
reason in the script), plus the same comparison through `--to=l4`; every emitted document validates
against `xsd/DMN13.xsd` with `xmllint`. (This paragraph said "117 of 120, eight XFAILs" from an
earlier count of the fixture set; the numbers above are the D-16-phase-2 run.)

**Two things about that harness are worth knowing before trusting a green run.** TS is a weak
surface on its own — measured, not assumed: `--to=ts` collapses eleven hit policies into two
outputs, so L4 is compared as well; and even L4 collapses `C`, `C+`, `C<`, `C>` and `C#` onto one
another, so **nothing here would notice a `COLLECT` emitted without its `aggregation` attribute**.
And `--xsd` is not a proxy for correctness in either direction: `test/dmn13/bad-rule-arity.dmn`
XSD-validates and dmnmd's reader refuses it.

`backend-baseline.sh` is the other half: every fixture × every implemented format, byte for byte.
Adding a `FileFormat` constructor is exactly the kind of edit that perturbs an unrelated format's
dispatch. **`--check` only** — re-recording after a change launders a regression.

> **This gate is currently dead, and you should not read a red run as a finding.** `test/roundtrip/
> baseline/` was last recorded at `8c18f22`, before D-1 (number rendering), D-2 (inference), D-7
> (diagnostic framing) and D-15 (message text) each changed output on purpose. `--check` now
> reports ~487 of 1,072 runs changed, essentially all of them already-reviewed landed work, which
> means a genuine cross-backend regression would be invisible in the noise.
>
> The fix is a deliberate re-record on trunk, audited against those four rulings — **not** a
> re-record folded into whatever change happens to notice. Until then, A/B against a binary built
> from `HEAD` for the change in hand, which is what D-16 did (1,520 paired invocations, and the
> point of the exercise was proving the 41 diffs were all `--to=xml`).

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
`error`. A `Warning` means something was dropped and says what (the DRG edges, the
`<annotation>` column names, a cell that is not a plain FEEL literal — no longer
`<defaultOutputEntry>`, which D-16 phase 2 carries in `dtDefaultOutput`). An `Error` means a
table could not be represented faithfully — a temporal `typeRef`, a rule whose entry count
disagrees with the column count, a cell that cannot be built at the column's type — and
that table is **not emitted**, because a table that can never match, or one whose rules have
been silently widened, is a wrong answer that exits 0.

**The markdown reader now does the same thing, and that is D-7.** It used to raise through
`error`, so one rule had two mechanisms and a bad markdown cell arrived with a Haskell
`CallStack` and a four-frame `HasCallStack backtrace:` of ghc-internal positions attached.
`app/ParseMarkdown.parseMarkdown` returns `([Diagnostic], [DecisionTable])`, fed by
`DMN.ParseTable.parseTableD` and `DMN.DecisionTable.mkDTable`, which have the same
`([Diagnostic], 0-or-1 tables)` shape `convTable` always had.

**The list is the gate, and that is the whole safety argument.** An `Error` means the table is
not in the returned list, so a caller cannot emit a table it was told to refuse merely by
forgetting to check. That property used to be supplied by two accidents of strictness — the
`tableWarnings` loop and `--pick`'s `tableName` filter both forced every table to WHNF before
anything was written — and those are gone. Three call sites receive markdown diagnostics and
all three act: `parseTableD` (skips `mkDTable` if pass 1 had an error), `mkDTable` (skips
`tableErrors` if a cell had one), and `Main.parseTables`' `Md` arm (prints them all, then
`exitFailure` on `anyErrors`, mirroring `parseDmnXml`).

A cell refusal carries a `DMN.DecisionTable.CellSite` and reads
`error: <file>: table "T": column "C": row N: …`, the same shape `structuralErrors` and
`domainErrors` already print. `mkFsAt`/`mkFAt` are the located wrappers — the first is pass 1
and handles **every** markdown cell, the second is the type-inference re-pass — and `showSite`
deliberately does **not** include the word `error:`, which `renderDiagnostic` supplies.
`mkFs`/`mkF` stay for the test suite only; `parseTable` is likewise an `error`-ing wrapper over
`parseTableD` kept for the 40-odd test call sites and unreachable from `app/`. `row N` is the
rule number the **author wrote** in the leftmost cell, so gaps and repeats survive into the
message; the XML reader stores a 1-based index in the same field, so the two readers mean
different things by "row". `Nothing` there is the sub-header row and prints no row segment.

Two consequences worth knowing:

* **A refused cell no longer stops at the first one forced.** `error` reported one cell and
  died; a returned list collects every one, in reading order. `policy/infer-declared-number-nonnumeric-refused`
  is the case that shows it — it reports rows 1 and 2 where it used to report only row 1. This
  matches what `tableErrors` has always done.
* **Markdown diagnostics now name the file.** `parseTableD` does not know it (the cell layer is
  shared with the XML reader), so `ParseMarkdown.parseChunk` adds it, which is where the two
  sibling markdown diagnostics already put it. That closes the gap the `CellSite` haddock
  records as a deferral.

One difference between the two readers' output survives on purpose: the missing in/out word,
because `reviseInOut` can relabel an explicitly-`(in)` column to `out`, so the word would
sometimes contradict the header. A second is cosmetic and unreasoned — XML writes
`<file>: error: …` and markdown writes `error: <file>: …`, because the markdown list serves
several input files at once and carries its file inside each message.

`errorWithoutStackTrace` (`crash`, in `app/Main.hs`) still covers the aborts that are not
diagnostics: an unsupported format, a multi-file `--from=xml`, an XML parse failure. What is
**not** covered by D-7, and still prints a `CallStack`, is the *evaluation*-time crashes on the
`-q` REPL path — `head0`, `fe2dval`, `fEval`'s type errors — recorded as
`symptom/eval-hp-first-no-match-crash` and `symptom/eval-collect-min-empty-crash`. Those already
have an `Either String` channel in `evalTable` to travel down; that is a separate, smaller job.

The exit status answers exactly one question: *did something we were asked to read fail to
read?* — with one extension the XML backend adds: **or fail to WRITE.** An emitter has a failure
mode a reader does not (a valid document that means something else), so a table it cannot express
faithfully is refused, and then nothing at all is emitted for any table in the file.

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
| a `U` table with two rows whose guards are identical — either reader (D-13) | 1 |
| markdown where *some* tables parsed and others did not | 1, and nothing is emitted |
| an output format the binary cannot write (`--to=md`) | 1, refused before anything is read |
| `--to=xml` over an input with no tables | 0, and an empty `<definitions/>` is written |
| `--to=xml` over a table DMN cannot express (no output column, short row) | 1, and nothing is emitted |

**Provenance.** Every row above was re-run against the built binary before and after D-7 and is
unchanged — including the two the change was most likely to break. `markdown where some tables
parsed and others did not` is the one to re-measure first if you touch this path; it holds now
because `Main.parseTables` checks `anyErrors` before `--pick` and before `withOutHandle`, where
it used to hold because forcing a table happened to raise. Measured alongside it, and also
unchanged: `-o` against a pre-existing file leaves it untouched on a refusal (`openFile … WriteMode`
truncates, so this is a real guarantee and not a tidiness), and all five emitting backends write
**zero bytes** of stdout for a two-table file whose second table is bad. The machine-checked
witness for that last row is `policy/md-partial-failure-emits-nothing`, a two-table case that
predates D-7. (`test/safe.md` is *not* a witness for it — that file fails at the file level on a
missing final newline, so it never reaches the some-parsed-some-did-not path. D-7's landing note
originally said no in-repo fixture existed at all; the corpus case above refutes that.)

A pipe table whose top-left cell is not a hit policy is prose, not a broken decision table:
`ParseMarkdown.isDecisionTable` asks `parseHitPolicy` itself, skips the chunk, and says so
on stderr as a `note:`. That is why a README full of documentation tables exits 0.

## The behavioural corpus (`languages/haskell/test/corpus/`)

The hspec suite **cannot** certify a change to the cell layer, because parts of it
assert the bugs as expected behaviour — `DmnXmlSpec.hs` expects `FNullary (VS "not(\"Fall\"")`,
which is the comma-split defect frozen into an expectation. A green suite therefore cannot
distinguish "I preserved the behaviour" from "I preserved the bug".

The `type inference` block in `Spec.hs` used to be the second example, and carried a KNOWN DEFECT
banner saying so; D-2 discharged it. Its replacement banner records the subtler trap that survived:
its `>23`/`<23` expectations pass under **both** the old substring rule and the anchored one, so a
green run of them proves nothing either way. The cases that discriminate were added below it.

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
- **Source positions are kept, not normalised** — but only because they are cheap and
  repo-relative, *not* because they discriminate anything. This bullet used to say `mkFsAt` and
  `mkFAt` "are the multi-value and single-value cell paths" and that the position was therefore
  "the only discriminator". Both halves are wrong. `mkFsAt` is the entry point for **every**
  markdown cell and `mkFAt` is the type-inference re-pass — in the very pair cited as proof,
  the single-value `0x10` raises at `mkFsAt` and the multi-value `0x10, 5` at `mkFAt`, the exact
  inversion of the gloss. And a diff consisting of *nothing but* moved positions is reported as
  `cosmetic` and does not fail the run, so a wrapper swap was already invisible to the runner.
  The discriminator now lives in `test/Spec.hs`'s `located cell refusals (mkFsAt / mkFAt)`
  block, which calls each by name — a test cannot be invalidated by a line moving. Since D-7
  no markdown recording carries a `CallStack` at all; the two that still do are eval-time
  crashes. Do not write the line numbers down anywhere — this bullet asserted `:121` and `:143` long
  after both had moved. (An earlier retraction added that `:143` "was never right". That is
  false: at `a670657`, the commit that wrote the sentence, `:121` was `mkFs`'s body and `:143`
  was `mkF`'s, and eight and six recordings cited them respectively. Both were exact when
  written and merely went stale — which is the whole argument for not writing them down, and is
  a *weaker* claim than the one that replaced it. Correcting a stale claim into a false one is
  worse than leaving it: rule 2 of `~/CLAUDE.md` names this exact move.)
- **No `policy/` recording cites a cell-path position any more.** Commit 6 had promoted eleven
  that did; D-7 removed the `CallStack` from all of them, so an edit above `mkFsAt`/`mkFAt` no
  longer dirties any policy recording. Exactly two recordings still carry a `CallStack`, both
  `symptom/` and both *evaluation*-time: `eval-hp-first-no-match-crash`, which is the only
  recording anywhere with a `src/DMN/` frame, and `eval-collect-min-empty-crash`, whose
  `CallStack (from HasCallStack):` header has **no** frame under it at all. (An earlier draft
  of this bullet said "two recordings carry a `src/DMN/` frame". One does. The count of
  `CallStack`s and the count of *frames* are different numbers.) If a D-7-shaped change ever
  dirties one of those, that is a scope leak, not a re-record.
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
- **`--from=xml` reads DMN 1.3, 1.4 and 1.5; `--to=xml` writes 1.3 only** (see "The XML backend"
  below for why). `--to=md` remains unimplemented and is now refused up front rather than after
  reading. The reader is deliberately strict — an element or attribute
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
  names and not seven, because `tIterator`, `tChildExpression` and `tTypedChildExpression` have
  no global `<xsd:element>` declaration; `<iterator>` cannot be written in a document at all.
  (This sentence used to add that `tQuantified` is abstract. It is not — `every` and `some` are
  its global elements, and they are in the list above. Nothing in `DMN15.xsd` is an abstract
  *complexType*.)

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
