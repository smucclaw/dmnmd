# `test/roundtrip/` — the gate on `--to=xml`

Two scripts, written **before** the emitter, so the implementer inherits the measurement
rather than inventing one that happens to pass.

**Result at the commit that landed `--to=xml` (D-8): 117 pass, 0 fail, 8 xfail, 36 skipped,
0 XSD-invalid, over 161 fixtures.**

## `run-roundtrip.sh` — did the meaning survive?

dmnmd already **reads** DMN 1.3/1.4/1.5/1.6. So the emitter can be checked against dmnmd's own
reader with no hand-written expectation at all:

```
F.md --to=ts                          (direct)
F.md --to=xml | --from=xml --to=ts    (round trip)
```

If those differ, either the emitter or the reader is wrong.

```
cabal build                              # the script does not build
./test/roundtrip/run-roundtrip.sh        # every fixture
./test/roundtrip/run-roundtrip.sh --xsd  # + xmllint against xsd/DMN13.xsd
./test/roundtrip/run-roundtrip.sh -v --only 'num-*' --keep /tmp/rt
```

**156 fixtures, of which 120 are eligible** (the root `README.md`, `test/safe.md`,
`test/golden/*.md`, and all 152 corpus `input.md`). The other 36 are skipped and listed by
name every run: their direct `--to=ts` already fails, because they exist to record a refusal,
and a fixture dmnmd will not read cannot say anything about an emitter.

**TS is not the only surface, because TS alone is not enough.** Measured, not assumed:
`--to=ts` collapses every hit policy into two classes — `{U,P,F}` emit an `else if` chain and
`{A,O,R,C,C+,C<,C>,C#}` emit independent `if`s — so a TS-equal round trip cannot see an
emitter that writes `ANY` where the source said `RULE ORDER`. The script therefore also
compares `--to=l4`, which separates `O`, `R` and `C` from the rest.

**The residual hole, which this harness does NOT close:** L4 collapses `C`, `C+`, `C<`, `C>`
and `C#` onto one another, because it refuses all five with the same message. Nothing here
would notice a `COLLECT` emitted without its `aggregation` attribute, or with the wrong one.
That needs a targeted test against the emitted document.

### XFAIL, not a loosened comparison

Constructs dmnmd accepts that DMN has no spelling for round-trip to a *different but correct*
document. DECISIONS.md D-11's suffix comparison (`5 <=` meaning `>= 5`) is the named example.
Those belong on the `xfail_reason` list **with a reason**, and the comparison stays strict; a
weakened comparison that passes is worse than a strict one that fails with a known list. An
XFAIL that starts passing fails the run too, so the list cannot rot.

The list shipped **empty** and was filled in only after the emitter was measured, which is the
whole reason to trust it: an entry written before the emitter existed would have been a guess, and
a guessed XFAIL is indistinguishable from a loosened comparison.

Eight entries, in four groups (`run-roundtrip.sh` holds the list):

* **no output column** (4). `tDecisionTable` is `output+`. Refused with a located error.
* **authored rule numbers not `1..n`** (2). DMN identifies a `<rule>` by position. Warned, then
  renumbered.
* **`= v` outside a declared domain** (1). Correct DMN, which dmnmd's own domain check then refuses.
* **a unary test in an output cell** (1). The reverse case: DMN is right and the markdown reader is wrong.

A short markdown row used to be a group of its own (2). The markdown reader now refuses a short row, so those fixtures fail the direct `--to=ts` run and are skipped, and the entries were deleted.

Six of the eight are REFUSALS, so `expected_divergence` is consulted at the failing LEG and not
only at the final diff — otherwise the only honest answer available (refusing a construct DMN
cannot express) would be permanently red.

**D-11's suffix comparison is NOT on the list, and its absence is a measurement.** D-11 says
`--to=xml` "must emit the mirrored form and record a fidelity note", as though the emitter could
tell. `DMN.ParseCell.suffixCmp` mirrors `5 <=` into `FSection Fgte (VN 5)` at PARSE time and
`FEELexp` has no provenance field, so all four `policy/num-suffix-*` fixtures round-trip CLEAN.
Neither do multi-value cells, the four interval spellings, arithmetic in an output cell, or
declared sub-header domains — all were suspected, all have exact DMN spellings, all pass.

## `backend-baseline.sh` — did anything else change?

Every fixture (306, `.md` **and** `.dmn`) × `ts js py l4`, byte for byte, plus stderr and exit status: **1,224 runs**.
(This sentence said 238 fixtures and 952 runs when the script was written.)
Adding a `FileFormat` constructor and an `outputTo` clause is exactly the kind of edit that perturbs an unrelated format's dispatch.

```
./test/roundtrip/backend-baseline.sh --record   # from the PRE-change binary
./test/roundtrip/backend-baseline.sh --check    # after
```

Formats are `ts js py l4` and no more. **There is no `json` backend**: `showToJSON` in
`app/Main.hs` is, despite its name, the interactive `-q` REPL's result printer and has no
`FileFormat` constructor.
`md` is a `FileFormat` constructor with no implementation.
`xml` was one when this script was written, and has been implemented since D-8, but it is still not in `FORMATS`; `run-roundtrip.sh` above is its gate.

`baseline/MANIFEST.sha` is committed and is the record; the 2,448 output files are bulky
(7.5 MB) and gitignored. `--check` diffs full files when they are present and falls back to
comparing checksums against the manifest when they are not, so the baseline survives a clean
checkout.
It was re-recorded on 2026-09-26 with a binary built at trunk `ea4df4a`, and `--check` then reported `0 changed` both with the full outputs present and against the manifest alone.
It was then extended by 20 manifest entries for the three fixtures D-22 part 1 (#62) moves, adds or re-fixtures, and `--check` again reported `0 changed` over 1,228 runs.
Every change that re-record absorbed is audited in [`baseline-audit/README.md`](baseline-audit/README.md).
(The previous recording was committed at `8c18f22`; this paragraph said it was made at `3f174f4`.)

`--check` never reports a manifest entry whose fixture has gone.
A renamed or deleted fixture drops out of the check with nothing printed, which is how 7 renames went unreported before that re-record.

## How this differs from `test/corpus/`

The corpus pins ~192 behaviours somebody thought to record, with a title and an argument for
each. These two scripts are the blunt instruments that complement it: one asks a question no
recording can (does the emitted document still mean what the source meant), the other covers
everything at once rather than what was anticipated. Neither replaces the corpus, and a green
run of either is not coverage — D-2 shipped a silent wrong answer with 189/189 corpus cases
unchanged.
