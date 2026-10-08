# Audit of the 2026-09-26 `backend-baseline` re-record

The baseline was re-recorded on branch `test/rerecord-backend-baseline`, with a binary built at trunk `ea4df4a`.
The previous recording was committed at `8c18f22`.
This file lists every change that re-recording absorbed, and what caused each one.
**Nothing below is unattributed.**

It lives here rather than in `baseline/`, because `--record` runs `rm -rf` on that directory and `.gitignore` excludes everything in it except `MANIFEST.sha`.
It is named `README.md` because both `backend-baseline.sh` and `run-roundtrip.sh` treat every other `*.md` under `test/` as a fixture.

## Method

1. **The old recording was rebuilt byte for byte.**
   A binary built at `8c18f22`, run by `8c18f22`'s own script over `8c18f22`'s own fixtures, reproduces the committed `MANIFEST.sha` exactly (1,952 lines, `diff` empty).
   So the full old outputs, which were never committed, could be diffed rather than only checksum-compared.
2. **`--check` at `ea4df4a`, before re-recording, printed `checked 1224 run(s): 883 changed`.**
   The 883 counts stdout and stderr files separately.
   It is 331 `CHANGED (sha)` files (11 stdout, 320 stderr) plus 552 `MISSING FROM MANIFEST` files.
   In runs (fixture × format), that is 323 changed runs plus 276 runs with no recording, 599 of 1,224.
3. **Renames were paired by hand.**
   Of the 69 fixture slugs with no recording, 7 are `git mv` renames at 100% similarity (for example `symptom/infer-version-float-collapse` → `policy/hp-unique-duplicate-rows-refused`).
   Each was diffed against its old name, with the case path normalised.
   The other 62 slugs are new fixtures, listed at the end.
4. **The audited set is 387 files (351 runs).**
   It is the 331 changed files, plus the 56 files of the 7 renamed fixtures.
5. **Every changed line was classified by shape, and the residue is empty.**
   Each diff line had to be claimed by a rule naming its cause.
   Rules that could claim a generic line (an exit status, a brace, a blank line) are scoped to the fixtures the causing change names.
6. **Every shape was bisected.**
   The current fixture set was run through 23 binaries.
   Two were `8c18f22` and `c5ca4e1`.
   Fourteen were the first-parent trunk commits from `603676f` to `ea4df4a`, less four (`2bfbd9c 4007b32 c1edff1 c6c676f`) whose diffs touch nothing under `src/` or `app/`.
   Seven were commits along PR #44: `3f174f4 aa7bbb5 1a434ff c21fedf f21cbb4 e7543b0 df059cb`.
   PR #44 needed its own lineage, because `24a5fcf` (the merge of trunk into `feat/to-xml`) does not compile: `showFeelXML` lacks an `FNot` arm under `-Werror=incomplete-patterns`.
   A cross-check confirmed that each file's shapes arrive at the commit named below.
   It also confirmed that every other commit where the file changed only moved a `CallStack` source position.
   Those moves are `aa7bbb5` (D-5) in 108 files and `c21fedf` (D-9) in 56, and D-7 later deleted the `CallStack` entirely.

## Causes

Counts are per cause, so a file with two causes is counted under both.

| cause | bisected to | files | runs | fixtures |
|---|---|---:|---:|---:|
| D-6: DRG edge warning on stderr | `1a434ff` (PR #44, reached this lineage in PR #45 `603676f`) | 132 | 132 | 33 |
| D-7: located diagnostics, no `CallStack` | `580f4b3` (PR #49) | 124 | 124 | 31 |
| D-19: boxed expressions refused by name | `18481de` (PR #54) | 48 | 48 | 12 |
| D-15: refusal text rewritten | `4fcdc1d` (PR #48) | 40 | 40 | 10 |
| D-18: `<decisionService>` read and dropped | `f5a26e6` (PR #53) | 32 | 28 | 7 |
| rename only, output identical | — | 24 | 16 | 4 |
| #58: DMN 1.6 in the namespace list | `ac48356` (PR #58) | 16 | 16 | 4 |
| D-16 phase 2: `<defaultOutputEntry>` carried | `9535164` (PR #51) | 16 | 8 | 2 |
| D-9: FEEL negation | `c21fedf`, plus `f21cbb4` for one `.l4` (PR #44) | 15 | 11 | 3 |
| D-17: `Any` is a known `typeRef` | `b6525bb` (PR #52) | 8 | 8 | 2 |
| D-13: identical guards in a `U` table refused | `c604620` (PR #46) | 8 | 4 | 1 |

**No run changed because of** D-1 or D-2, which both landed before `8c18f22`.
`src/DMN/Number.hs` (D-1) is in `8c18f22`'s tree, and D-2's merge `3f174f4` is an ancestor of `8c18f22`.
Nor did any run change because of D-5 (positions only, see above), PR #47 or PR #50.
PR #57, PR #59 and PR #60 changed no fixture that existed at `8c18f22`; the runs they did change belong to fixtures they added, which are among the 62 below.

## One representative diff per shape

Long lines are cut at `…`.

**D-6** (`dmn13/baseline.dmn`, all four formats' stderr): a warning is added, and nothing is removed.

```
+<REPO>/languages/haskell/test/dmn13/baseline.dmn: warning: decision "Band": <informationRequirement> "InformationRequirement_age" declares a <requiredInput> on "#InputData_age", which dmnmd does not model. …
```

**D-7** (`policy/num-hex-refused`): the `CallStack` and backtrace go, the message gains the file, and a summary line follows.
Every message body the pre-D-7 binary printed is still printed, which was checked for all 148 files that changed at `580f4b3`.
One fixture, `policy/infer-declared-number-nonnumeric-refused`, now also reports row 2, as the D-7 section of `CLAUDE.md` says it should.
The count line changed as recorded in DECISIONS.md D-7: `dmnmd: 1 decision table(s) in …` became `dmnmd: one or more decision tables in …`.

```
-dmnmd: error: table "HexLiteral": column "Code": row 1: the cell reads "0x10" — …
-CallStack (from HasCallStack):
-  error, called at src/DMN/DecisionTable.hs:231:34 in dmnmd-0.1.0.2-inplace:DMN.DecisionTable
-HasCallStack backtrace:
-  … (three more frames, two blank lines)
+error: <REPO>/…/policy/num-hex-refused/input.md: table "HexLiteral": column "Code": row 1: the cell reads "0x10" — …
+dmnmd: one or more decision tables in <REPO>/…/policy/num-hex-refused/input.md could not be read
```

**D-15** (the same file, inside the D-7 line): `…an interval, or an arithmetic expression.` becomes `…an interval, or arithmetic dmnmd can read.`, and the closing advice is rewritten.
In `policy/num-dash-range-refused` the advice gains `— but a String column compares this cell as literal text rather than computing it, so a test written that way can never match.`

**D-19** (`dmn15/bad-conditional.dmn`): the closing advice names `<typeConstraint>` only when one is present.

```
-dmnmd models decision tables: a <decision> must hold a <decisionTable>, and an <itemDefinition> may carry <allowedValues> but not <typeConstraint>.
+dmnmd models decision tables: a <decision> must hold a <decisionTable>.
```

In the three `examples/Chapter 11 …` documents, the raw `xpCheckEmptyContents` dump becomes a named list: `decision "Loan Data": <context> is a boxed context (a list of name/value entries). dmnmd does not model it.`

**D-18** (`examples/Diagram Interchange/diagram-interchange-decision-service.dmn`): a refusal at exit 1 becomes a warning at exit 0.

```
-dmnmd: <REPO>/…/diagram-interchange-decision-service.dmn: this is valid DMN 1.3, but it contains <decisionService>, which dmnmd does not model. Only <decision>, <inputData> and <knowledgeSource> are read.
-xpCheckEmptyContents: unprocessed XML content detected
-… (context, contents, blank)
+<REPO>/…/diagram-interchange-decision-service.dmn: warning: <decisionService> "Decision Service 1" (DecisionService_1) is not modelled and is dropped. …
```

Its stdout, in all four formats, is `-### exit 1` / `+### exit 0`.
Where a `<businessKnowledgeModel>` is still refused, the sentence's tail becomes `are modelled; a <decisionService> is read and dropped with a warning.`

**#58** (`dmn13/not-dmn13.dmn`): the list of readable namespaces gains `DMN 1.6 ("https://www.omg.org/spec/DMN/20240513/MODEL/")`.

**D-16 phase 2** (`dmn13/default-output-entry.dmn`): the "does not carry" warning goes, and the default becomes the trailing arm.

```
.l4:  -    OTHERWISE ""
      +    OTHERWISE "unknown"
.ts:  +  else if ("default") { // 4
      +    return {"Band":"unknown"};
      +  }
```

**D-9**: the root `README.md` gained a `Negation` table in `c21fedf`, so this is an input change and a binary change together.
Its `.ts`, `.js` and `.py` each gain one function, `Negation`, whose `.ts` first arm is `if (!((1.0 <= Age && Age <= 5.0))) { // 1`.
Its `.l4` is unchanged, and exits 1 at both ends on Example 3's `O` hit policy.
The renamed `policy/md-negation-in-numeric-column-emitted` goes from a refusal at exit 1 to emitted code at exit 0, and its `.l4` gains `f21cbb4`'s parentheses, `IF (NOT ((Age >= 1 AND Age <= 5)))`.
In `policy/list-cell-feel-construct-refused`, `dmnmd does not implement FEEL function calls, negation or list literals` becomes `A collection column takes members, not tests, so negation is refused here even though dmnmd implements it elsewhere; …`.

**D-17** (`dmn13/unknown-type.dmn`): `Known types are: … boolean bool.` becomes `… boolean bool Any.`

**D-13** (renamed `symptom/infer-version-float-collapse` → `policy/hp-unique-duplicate-rows-refused`): a `U` table with two `Version: 1.1` rows was emitted at exit 0, and is now refused at exit 1 with `row 1 and row 2 have the same input cells (Version: 1.1), so row 2 can never match: …`.

## Admitted without a before

These 62 fixtures are new since `8c18f22`, so they had no recording to differ from.
Their outputs entered the baseline as trunk produces them.
The adding commits are `5f2fb50` (8), `2c64890` (6), `bd73f4f` (6), `07d6ae4` (5), `6b0e4d2` (5), `687c5b7` (4), `79949ef` (4), `8931389` (3), `c21fedf` (3), `c286deb` (3), `c5ca4e1` (3), `b206ca0` (2), `bc98614` (2), `f4c7104` (2), and one each from `1a434ff`, `38985c7`, `8c0ab89`, `afc0141`, `e8ba079` and `f21cbb4`.
Two of them were added under other names and renamed later: `8c0ab89` added `symptom/eval-hp-any-two-rows-disagree`, and `bc98614` added `policy/hp-unique-catchall-warned`, now `-promoted`.
12 of the 62 are `symptom/` cases, so the baseline now records known-wrong behaviour on purpose.
It is a change detector, not a correctness oracle.

## A gap in `--check` that this audit had to work around

`--check` walks the current fixtures, so a manifest entry with no fixture behind it is never reported.
The 7 renames therefore showed up only as 56 `MISSING FROM MANIFEST` lines under their new names.
Their 56 old entries vanished from the check silently.
A deleted fixture would vanish the same way, with nothing printed.
This is recorded, not fixed.

## A false red that was fixed in the same commit

`cabal test` writes scratch `.dmn` files into the git-ignored `test/golden/.out/` (45 of them at `ea4df4a`), and the script collected them as fixtures.
After the re-record, a `--check` run after `cabal test` in the same tree printed `checked 1404 run(s): 360 changed`, every one of them `MISSING FROM MANIFEST` under `golden/.out/`.
The recording itself was not affected, because it was made in a tree where `cabal test` had not yet run.
`collect_fixtures` now prunes dot-directories, which removes exactly those files and nothing tracked, and the same `--check` then printed `checked 1224 run(s): 0 changed`.

## Extension for D-22 part 1 (#62), 2026-09-26

This branch was then rebased onto #62, whose fixture changes left `--check` at `checked 1228 run(s): 20 changed`.
All 20 are fixture changes, not emitter changes, and `--record` followed by `--check` gives `0 changed`:
- `eval-hp-first-no-match-crash` moved from `symptom/` to `policy/`: 8 entries leave under the old path and 8 arrive under the new one, with identical hashes.
- `md-eval-unique-catchall-default` is new: 8 entries, and its four stdout hashes are exactly the old `md-eval-unique-conflict` hashes, because it is that table byte for byte.
- `md-eval-unique-conflict` was re-fixtured with a genuine overlap, so its four stdout hashes changed and its stderr did not.

## Extension for D-22 part 2 (refusing conflict regions), 2026-09-29

Branch `feat/d22-refuse-conflicts`, on #64 (`b2b7e83`).
Before recording, `--check` printed `checked 1260 run(s): 116 changed`: 36 `CHANGED (sha)` files and 80 `MISSING FROM MANIFEST` files.
After `--record`, it printed `checked 1260 run(s): 0 changed`.
In the manifest, 52 entries left and 116 arrived.

**Changed runs of existing fixtures: 28 runs, 36 files.**
- `policy/eval-hp-any-two-rows-disagree` and `policy/md-eval-unique-conflict`: 4 runs each, stdout and stderr.
  Each table is now refused when it is read, so stdout becomes `### exit 1` and stderr gains the conflict error.
- `policy/hp-unique-near-duplicate-rows-accepted`, `policy/md-prefix-comparisons`, `policy/md-multivalue-dash-reprocessed` and `policy/md-negation-in-numeric-column-emitted`: 4 runs each, stdout only.
  Each was re-fixtured without its overlap, so the emitted code differs; each still exits 0 with empty stderr.
- `safe2.dmn`: 4 runs, **stderr only**.
  It gains 7 errors, for three `U` tables whose rules overlap: `type of event`, `is liquidity event` and `is dissolution event`.
  Its stdout and exit status are unchanged, because another table in the same file was already refused.
  It is not a corpus case, so D-22's list of what moves did not name it, but its refusal follows from rule 2 as the listed ones do.

**Moved fixtures: 2, with 16 entries leaving and 16 arriving.**
`symptom/l4-hitpolicy-unique-silently-first` and `symptom/hp-any-duplicate-rows-disagree-silent` moved to `policy/`.
Their hashes changed as well as their paths, because both tables are now refused: stdout was code and is now `### exit 1`, and stderr was empty and now holds the conflict error.

**New fixtures: 8, with 64 entries arriving.**
They are `dmn13/bad-overlapping-unique-rules.dmn`, `policy/xml-unique-overlap-refused`, the four `policy/hp-unique-overlap-*-refused` cases, `policy/eval-unique-collection-overlap-at-runtime` and `policy/eval-hp-any-collection-disagree-at-runtime`.

## Extension for D-22 part 3 (no rule matched is `NOTHING` in L4), 2026-09-29

Branch `feat/d22-l4-nothing`, on #65 (`98cd8c5`).
Before recording, `--check` printed `checked 1260 run(s): 81 changed`, every one a `CHANGED (sha)` line for `--to=l4` stdout.
No `ts`, `js` or `py` run changed, and no stderr file changed.
After `--record`, it printed `checked 1260 run(s): 0 changed`.
In the manifest, 81 entries changed and none arrived or left.

**How each run was checked.**
The manifest holds only checksums, so the full outputs came from an A/B: binaries built at `98cd8c5` and at this branch, run over the same 315 fixtures in `ts js py xml l4`, 1,575 runs.
81 differ, all of them `--to=l4` stdout, and they are exactly the 81 runs `--check` named.
A script split each changed output into its function blocks and checked two things.
First, every block that differs differs only by rule 3's rendering: `GIVETH A T` becomes `GIVETH A MAYBE T` (with `(LIST OF T)` parenthesised), each `THEN v` becomes `THEN JUST v`, and the `OTHERWISE` becomes `NOTHING`.
Second, the tables whose blocks changed are exactly the tables the rule selects, read with the binary's own readers and asked of `DMN.Regions`.
There are 93 such tables in the 81 fixtures, and none has a catch-all row, a declared default or an all-wildcard arm.
83 have a no-match region, and the other 10 are tables `regionMap` cannot analyse: 4 with a collection column, 4 with a String cell holding FEEL test syntax, and 2 with a short row.
No table was already a `MAYBE`, and no fixture that the rule selects a table in was left unchanged, except where `--to=l4` exits 1.
`l4 check` was run on both sides of all 81: 77 typecheck before and after, and 4 fail before and after for pre-existing reasons.
Three of those are the undeclared result type of a table with no output column (`symptom/l4-zero-output-dangling-giveth`), and one is a record with two fields of one name (`symptom/struct-dup-rightmost-name-two-outputs`).

**The fixtures, one `--to=l4` run each.**
- 52 `policy/` cases: `cli-pick-multiple-names`, `eval-hp-any-collection-disagree-at-runtime`, `eval-hp-any-multirow-duplicated`, `eval-hp-first-no-match-crash`, `eval-unique-collection-overlap-at-runtime`, `infer-all-wildcard-column-silent`, `infer-hyphenated-words-stay-string`, `infer-negative-number-resolved`, `infer-prose-with-angle-is-string`, `infer-prose-with-dots-is-string`, `l4-otherwise-sentinel-not-last-row`, `list-input-membership-l4`, `list-input-membership-ts`, `list-output-l4-literal`, `md-declared-string-boolean`, `md-eval-unique`, `md-hash-annotation-column`, `md-list-output-array`, `md-multi-table-all-parse`, `md-multivalue-cell`, `md-multivalue-dash-reprocessed`, `md-pick-filter`, `md-prefix-comparisons`, `md-quoted-literal-all-or-nothing`, `md-string-equality`, `md-test-shaped-label-declared-string-kept`, `md-test-shaped-label-quoted-kept`, `num-cent-precision-preserved`, `num-large-integer-exact`, `num-leading-dot-accepted`, `num-multivalue-numeric-preserved`, `num-negative-range-accepted`, `num-negative-threshold-accepted`, `num-range-decimal-accepted`, `num-range-halfopen-honoured`, `num-suffix-gt-mirrored`, `num-suffix-gte-mirrored`, `num-suffix-lt-mirrored`, `num-suffix-lte-mirrored`, `num-thousands-with-space-accepted`, `struct-prose-table-is-decision-table`, `xml-any-typeref-infers`, `xml-decision-variable-not-applied-multi-output`, `xml-decision-variable-types-single-output`, `xml-id-uniqueness-not-name-derived`, `xml-informationrequirement-dropped-silently`, `xml-inputdata-name-type-conflict`, `xml-itemdefinition-warns`, `xml-no-output-column-refused`, `xml-output-label-names-single-output`, `xml-short-row-refused`, `xml-wildcard-output-warned`.
- 25 `symptom/` cases: `cli-pick-no-match-silent`, `cli-query-pick-no-match-misreported`, `infer-explicit-type-contradiction-silent`, `js-nonascii-escape`, `l4-output-range-upper-bound-dropped`, `l4-zero-output-dangling-giveth`, `md-dash-written-interval-silent`, `md-negation-in-string-column-silent`, `md-string-col-arith-dead-rule`, `struct-dash-rownum-dropped`, `struct-dup-column-names`, `struct-dup-rightmost-name-two-outputs`, `struct-fenced-code-ingested`, `struct-label-pre-wins`, `struct-midtable-continuation`, `struct-no-heading-default-name`, `struct-onecol-no-output`, `struct-short-row-truncated`, `struct-tablename-collision`, `struct-tablename-first-backtick`, `struct-zero-rule-table-accepted`, `xml-comma-split-negation`, `xml-literal-expression-decision-dropped`, `xml-unreadable-test-kept-as-text`, `xml-zero-rule-table-accepted-silently`.
- 4 others: `dmn13/any-typeref.dmn`, `dmn13/decision-variable-typeref.dmn`, `dmn13/output-label.dmn` and `examples/Diagram Interchange/diagram-interchange-dish-example.dmn`.

72 of the 77 corpus cases record another backend or the `-q` evaluator (58 `--to=ts`, 6 `-q`, 5 `--to=xml`, 2 `--to=js`, 1 `--to=py`), so their corpus recordings did not move.
The five whose recordings run `--to=l4` did, and were re-recorded in the same change: the three D-22 names, and `symptom/l4-output-range-upper-bound-dropped` and `symptom/l4-zero-output-dangling-giveth`, whose defects are unchanged.

## Extension for audit 10, the parser's silent drops (2026-10-08, branch `fix/audit10-parser-silent-drops`)

One paragraph per fix, in commit order.
Every figure was read from `--check` before `--record`, and each `--record` was followed by a `--check` that printed `0 changed`.
The binary has to be named `dmnmd`: a refused `.dmn` fixture's stderr begins with the program name, so a binary copied under another name (`dmnmd-before:` was seen) changes those fixtures' stderr checksums.

**f1, a short sub-header row (`ParseTable.subHeaderArityDiags`).**
Before recording, `--check` printed `checked 1268 run(s): 16 changed`, all `MISSING FROM MANIFEST`.
No run of an existing fixture changed.
In the manifest, 8 entries left and 16 arrived.
The 8 that left are `symptom/md-short-subheader-drops-column`, `git mv`d and renamed to `policy/md-short-subheader-refused`; its four formats now exit 1 with the sub-header error on stderr.
The 16 that arrived are that fixture and the new `policy/md-subheader-blank-cell-declares-no-domain`, which is accepted.

**f2, a short data row (`ParseTable.rowArityDiags`).**
Before recording, `--check` printed `checked 1272 run(s): 24 changed`: 8 `CHANGED` files and 16 `MISSING FROM MANIFEST`.
Only three fixtures in the whole set contain a short row, and the three are the only ones that moved.
The binary was compared with a trunk binary (`e368344`, named `dmnmd`) on each of the three, in `ts js py l4 xml`:
- `policy/xml-short-row-refused`, 8 files changed in place.
  `ts`, `js`, `py` and `l4` exited 0 with a function (159 to 253 bytes of stdout) and empty stderr, and now exit 1 with empty stdout and the reader's one error on stderr.
  `--to=xml`, which is not in the baseline, exited 1 before and after, with a different message.
- `symptom/struct-short-row-truncated`, now `policy/struct-short-row-refused`: 8 entries left and 8 arrived, with the same change from exit 0 to exit 1.
- `symptom/md-all-short-rows-drop-output-header`, now `policy/md-all-short-rows-refused`: 8 entries left and 8 arrived, the same way, with one error per row.
No other fixture moved, so no existing fixture has a short row beyond these three.
In the manifest, 16 entries left and 16 arrived, and 8 changed in place.

**f7, a column header with two meanings (`ParseTable.labelClash`).**
Before the corpus cases were moved, `--check` printed `checked 1276 run(s): 16 changed`, and the 16 were the two fixtures whose header labels disagree: `symptom/md-header-in-and-out-silently-in` and `symptom/struct-label-pre-wins`, every format, stdout and stderr, from exit 0 with a function to exit 1 with the clash error.
No other fixture in the set has a header with two meanings.
After the moves and the new control, `--check` printed `checked 1280 run(s): 24 changed`, all `MISSING FROM MANIFEST`.
In the manifest, 16 entries left (the two symptoms) and 24 arrived: `policy/md-header-in-and-out-refused`, `policy/struct-label-pre-post-clash-refused`, which is `symptom/struct-label-pre-wins` renamed, and the control `policy/md-header-agreeing-labels-accepted`, which is accepted.

