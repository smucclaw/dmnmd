# The behavioural corpus

This directory records **what `dmnmd` actually does today**, one small input per
defect, with stdout, stderr and exit status all captured, and every case labelled
either `symptom` or `policy`.

It exists because of a specific problem. `dmnmd`'s markdown cell layer decides what
a cell means by running regexes over its text, twice — once at parse time and again
after type inference. An audit found roughly forty defects in that layer, most of
them *silent misparses*: the cell is accepted, nothing is reported, and it means
something other than what it looks like. `5 <=` compiles to `> 5`. `>= 1,000` means
"at least 1, or exactly 0". `not([1..5])` quietly loses the negation. The plan is to
replace the whole layer with a real S-FEEL grammar.

The existing hspec suite cannot be the safety net for that work, because **parts of
it assert the bugs as expected behaviour**. `test/DmnXmlSpec.hs` expects
`FNullary (VS "not(\"Fall\"")`; that is the comma-split defect frozen into an
expectation. `test/Spec.hs` has a `type inference` block whose passing cases pass
*because* the inference regexes are unanchored. A green suite therefore cannot tell
"I preserved the behaviour" from "I preserved the bug".

The corpus can, because every case says which of the two it is.

## symptom vs policy

This distinction is the point of the whole directory. Get it right when you add a
case, and re-examine it when a case changes.

**`cases/symptom/…`** — records behaviour that is **wrong** and **should change**.
A diff here during the rewrite is progress. The correct response is: read the diff,
satisfy yourself the new behaviour is right, re-record, and if the case is now
*correct* rather than merely *different*, move the directory to `cases/policy/` so
that from then on it is protected instead of merely observed.

**`cases/policy/…`** — records behaviour that is **right** and **must not change**.
A diff here is a regression. The correct response is to fix the code, not the
recording. Do not re-record a policy case until you have established that the new
behaviour is right and the old recording was wrong — and if you conclude that, say
so in the commit message, because you are changing what the project promises.

Policy covers five kinds of thing:

1. **Every currently-correct parse** — prefix comparisons (`< 18`), closed ranges
   (`[5..8]`), wildcards, multi-value cells, string equality, yes/no booleans,
   arithmetic output expressions, explicit `: Number` / `: String` / `: Boolean`
   headers, `(comment)` columns, multiple output columns. A corpus of only bugs
   cannot detect a rewrite that breaks something that already worked, so these are
   here deliberately. Several are the direct control for a symptom case: prefix
   comparisons are the control for the four inverted suffix cases, and
   `md-multivalue-cell` is the control for the two thousands-separator cases.
2. **Every hit policy that currently works** — `evalTable` has arms for all seven of
   `U A P F O R C`, and each one consumes the same `matches`/`FEELexp` comparison
   the S-FEEL grammar replaces, so all seven are exposed to the rewrite. Six of them
   work and are policy cases here; `A` does not, and is
   `symptom/eval-hp-any-always-left` instead — it has never returned an answer,
   because the arm tests `not (null (nub …))` where it means `length (nub …) > 1`.

   The `eval-hp-*` cases drive the interpreter through `-q` and are built to be
   **mutually distinguishing**: `F` and `R` share a table and an input and must
   return one row versus three; `P` and `O` share a table and must return one row
   versus three, *in enum order rather than rule order*; and Collect `+`, `<`, `>`
   share a table and must return 35, 5 and 20. A rewrite that collapsed two policies
   into one, or wired every Collect operator to the same aggregation, cannot pass all
   of these — which is the property a per-policy case in isolation would not have.

   Collect `#` and `A` return correct values but print a `Debug.Trace` line to
   stderr, so they are symptom cases. Nothing is lost by that: `+`, `<` and `>`
   already guard the aggregation path.
3. **Exit-status and diagnostic semantics** — see the table in the repo's
   `CLAUDE.md` under "Diagnostics and exit status". A parse failure exits 1 and emits
   nothing; prose markdown exits 0; a DMN document with no `<decision>` exits 0;
   partial success emits nothing at all.
4. **Deliberate refusals** — DMN 1.1/1.2 rejection, temporal `typeRef`, rule/column
   arity mismatch, the L4 backend's refusal of list-valued hit policies. These are
   places where the tool chooses to fail rather than give a plausible wrong answer,
   and that choice is load-bearing.
5. **Acceptances that were recently *won*** — the three `xml-*-namespace-*` /
   `xml-variable-under-inputdata` cases pin fixes made in PR #17, the commit this
   directory sits directly on top of: a vendor `xmlns` on `<definitions>`, an absent
   optional `xmlns:dc`, and an `<inputData>` carrying a child `<variable>` are all
   accepted now and were all rejected before. Each emits output byte-identical to
   `xml-baseline-13`, and that identity *is* the assertion. A fix nobody pinned is a
   fix waiting to be undone.

The class lives in the **directory path**, so it is readable at a glance and
filterable with a glob. Reclassifying a case is a `git mv`.

## Layout

```
test/corpus/
  README.md            this file
  run-corpus.sh        the runner
  cases/
    symptom/<slug>/
    policy/<slug>/
      case.conf        TITLE, WHY, SOURCE, ARGS, and optionally WORKDIR / STDIN_FILE
      input.md         the input (or input.dmn)
      stdin            optional; fed to the process, for -q interactive cases
      expected/
        stdout
        stderr
        exit
```

`case.conf` is a shell fragment sourced by the runner:

| key | meaning |
|---|---|
| `TITLE` | one line, what this case is about; shown by `--list` and in diffs |
| `WHY` | the paragraph you will want in six months: what the defect is and why it happens |
| `SOURCE` | where in the tree it lives, and which audit finding it came from |
| `ARGS` | the arguments after the binary name. Eval'd, so quoting works. `$CASE` is the absolute case directory |
| `WORKDIR` | `case` (default), `pkg` (`languages/haskell`), or `repo` (repo root). **No case uses this.** It is retained for a case that genuinely needs to exercise path handling; if you reach for it to read a fixture, copy the fixture in instead — see below |
| `STDIN_FILE` | a filename in the case directory to feed to stdin; omit for `/dev/null` |

One input file is **deliberately missing its final newline**:
`cases/symptom/md-error-position-misreported/input.md`. That is the defect. If your
editor is configured to insert a trailing newline on save, do not open it — the case
will start passing for the wrong reason.

Every case is self-contained: its input file lives beside it, even where that means
a copy of a fixture from `test/dmn13/` or `test/golden/` (a `PROVENANCE` file says
so). That is intentional. The corpus is a *frozen record*; a case whose expectations
shift because somebody edited a shared fixture is a source of confusing diffs — and
worse, a way to turn a policy recording stale without touching the corpus at all.

This is checkable, so check it if you add a case:

```
grep -hE '^ARGS=' cases/*/*/case.conf | grep -E '[A-Za-z0-9_.-]+/'   # must print nothing
grep -l WORKDIR cases/*/*/case.conf                                  # must print nothing
```

## Running it

Build the binary first — the runner does not build.

```
cd languages/haskell
cabal build
make corpus                     # or: ./test/corpus/run-corpus.sh
```

Useful variations:

```
./test/corpus/run-corpus.sh --list                 # what is in here
./test/corpus/run-corpus.sh --class policy         # just the regression net
make corpus-policy                                 # same thing
./test/corpus/run-corpus.sh --only 'num-*'         # one family
./test/corpus/run-corpus.sh --verbose              # print each command line
DMNMD=/some/other/dmnmd ./test/corpus/run-corpus.sh
```

**Exit status of the runner**: 0 if every selected policy case matched, 1 if any
policy case diverged or any case has no recording. Symptom divergences are printed
in full but do not fail the run — during the rewrite they are the expected outcome,
and a runner that goes red on progress will get ignored.

Selecting **nothing** is also an error. A mistyped `--only` or `--class` used to run
zero cases and print a reassuring all-zeros summary with exit 0, which in a CI step
is indistinguishable from success; it now exits 1 and says so.

More generally, the runner **fails closed**. Every one of these was a real fail-open
path that produced a green run against a wrong baseline, and each is now refused:

| situation | what used to happen | now |
|---|---|---|
| `STDIN_FILE` names a file that isn't there | redirections apply left to right, so the stdout redirect never ran and the case reported the **previous** case's output — then matched it forever after | `UNRUNNABLE`, exit 1 |
| `ARGS` doesn't parse | `eval` failed and `$@` still held the previous case's argv, so a case declaring `--to=py` recorded a TypeScript baseline | `UNRUNNABLE`, exit 1 |
| `case.conf` doesn't parse — an apostrophe in a single-quoted `WHY` closes it early | the rest of the file was abandoned, `ARGS` stayed empty, the case ran with no arguments and its diff blamed dmnmd | `UNRUNNABLE`, exit 1 |
| a case sits in `cases/policies/` (typo) | silently treated as a symptom: never checked, never able to fail, invisible to `--class policy` | whole run refused |
| `WORKDIR` is not `case`/`pkg`/`repo` | warned, then used the case directory anyway | whole run refused |

The first three mark only that case unrunnable; the last two stop everything, because
they mean the corpus itself is mis-arranged rather than one case being broken. In
`--record` mode all five are checked **before** anything is written, so a broken case
cannot have a bogus recording committed for it.

A third outcome sits between the two. A diff whose *entire* content is moved source
positions — `L4.hs:53:23` became `L4.hs:57:23` because somebody inserted a line — is
reported as `cosmetic`, printed in full, and does **not** fail the run even for a
policy case. See "Normalisation" below for why the positions are kept in the
recordings rather than stripped.

The runner finds the binary in this order: `$DMNMD`, `cabal list-bin exe:dmnmd`
(cheap, does not build), the newest `dist-newstyle` build product, stack's local
install root, `dmnmd` on `PATH`. It never hardcodes an absolute path.
(`test/TranslateL4Spec.hs` does hardcode one, to an external `l4`; that was a
mistake and this does not repeat it.)

That last fallback is a trap, so the runner warns loudly when it takes it. If you
have ever run `stack install` or `cabal install`, there is a `dmnmd` in
`~/.local/bin` that is as old as the day you installed it; running the corpus from a
tree with no build product will silently test *that* binary and report a screenful
of regressions that say nothing about your working tree. This happened while the
script was being written. Always `cabal build` first, and read the `corpus: using …`
line before believing a failure.

## Re-recording after an intentional change

```
./test/corpus/run-corpus.sh --record --only 'num-suffix-*'   # a family
./test/corpus/run-corpus.sh --record --class symptom         # all symptoms
./test/corpus/run-corpus.sh --record                         # everything
```

`--record` overwrites `expected/` for the selected cases with whatever the binary
does right now. **Always run the check first and read the diff.** The value of the
corpus is entirely in the moment where a human looks at a diff and decides whether
it is progress or damage; `--record` without reading is how that value is thrown
away.

After re-recording a symptom case that is now *correct*, `git mv` it into
`cases/policy/` and rewrite its `WHY` to say what the behaviour now guarantees. A
symptom case that has been fixed but left in `cases/symptom/` is unprotected: the
runner will not fail if it regresses.

## Normalisation

Two things are stripped from both channels before comparison, because they are
properties of the compiler rather than of `dmnmd`:

- the `HasCallStack backtrace:` header line, and
- stack frames pointing into `libraries/…` (ghc-internal, haskeline), which embed a
  build-specific package hash such as `haskeline-0.8.2.1-bd70`.

Trailing whitespace is stripped too. Everything else is compared byte for byte,
including the `CallStack (from HasCallStack): error, called at src/DMN/…` lines —
those are repo-relative, meaningful, and exactly the sort of thing we want to notice
changing.

**Source positions are deliberately *not* normalised**, and it is worth knowing why,
because stripping them looks like the obvious cleanup. `DecisionTable.hs:121` is
`mkFs` and `:143` is `mkF` — the multi-value and single-value cell paths — and
several cells produce byte-identical message text down both. `unable to parse an
alleged boolean: n` appears at both, in `infer-declared-boolean-n-crash` and
`infer-boolean-n-crash` respectively. Collapse the positions and those two distinct
defects record identically, and a rewrite that moved a crash from one path to the
other becomes invisible.

So the positions stay, and the *comparison* is what gives ground: a diff consisting
of nothing but moved positions is classified `cosmetic` and does not fail the run.
That keeps the information in the file while removing the one thing that would
genuinely corrode this directory — a policy case going red for a reason it never
claimed to protect, teaching whoever sees it that the right response to red is
`--record` rather than reading.

**The package's unit id is normalised away**, and it has to be. A `CallStack` frame
names the unit that raised it, and the two build tools spell that differently for the
*same source*: cabal writes `dmnmd-0.1.0.2-inplace`, stack writes a content hash over
the dependency closure, `dmnmd-0.1.0.2-Lh7ThTCGA728GOI7Am8Gmj`, which changes whenever
the closure changes. Both become ` in dmnmd:`.

Without that rule the corpus is red on whichever toolchain did not record it — and
since CI builds with stack while most local work here is cabal, that meant **CI red on
its first run**, on 14 recordings, for a difference with no behavioural content
whatsoever. It was caught by an adversarial audit rather than by CI, which is luck; it
would have arrived as a wall of `REGRESSION` lines inviting exactly the blanket
`--record` this file spends two paragraphs warning against.

A **GHC upgrade** may still move the reported source spans in `app/Main.hs:(175,1)-…`
style messages — those are handled by the cosmetic rule above.

## Not wired into `cabal test`

Deliberately. Three reasons:

1. `cabal test` can build and run the test suite without the *executable* being
   current, so a corpus spec inside it would compare fresh source against a stale
   binary and produce a false signal.
2. `test/TranslateL4Spec.hs` already makes `cabal test` machine-dependent by
   shelling out to an external `l4`. Adding a second shell-out dependency to the
   same suite makes "is `cabal test` green" mean even less than it does now.
3. The corpus is meant to be run constantly while the grammar is being written, and
   `cabal test` is the expensive cycle that discourages that.

Outside `cabal test`, but **not** outside CI. `.github/workflows/haskell.yml` runs
`make corpus` as its own step after `stack test`, which is where the binary-vs-source
staleness problem above goes away: `stack build` has already run in that job, so the
binary is current by construction.

CI runs the whole corpus rather than `--class policy`, deliberately. Both fail only
on a policy regression, but the full run also catches a case with no recording, and
it puts symptom diffs in the log where they are useful — as the S-FEEL grammar lands,
that list *is* the progress report.

## Provenance and honesty about the recordings

Every recording in here was produced by executing the binary built from the commit
that introduced this directory. None of it was copied from the audit transcripts.
That matters, because the audit ran against an older tree and PR #17 moved several
things underneath it. Where current behaviour differs from what the audit reported,
the case's `WHY` says so. The three that moved:

- **Markdown parse failures now exit 1**, not 0. The audit recorded exit 0 with
  empty stdout — "a redirect silently produces an empty output file". That is fixed;
  `policy/md-parse-failure-exits-nonzero` and
  `policy/md-partial-failure-emits-nothing` pin the new behaviour so it cannot drift
  back.
- **The unlocated `Prelude.read: no parse` is gone.** `mkF` was refactored into a
  total `mkFEither` using `readMaybe`, so the message now quotes the offending cell
  text (`expected a number, but this column is typed Number and the cell reads
  ".5"`). It is still an `error` with a call stack, still names no table, column or
  row, and still exits 1 — so the crash cases are still symptoms, but their recorded
  text is not what the audit shows.
- **An unmodelled XML `typeRef` is refused, not warned.** `test/dmn13/README.md`
  still describes `unknown-type.dmn` as "must warn and degrade, not crash"; the
  current reader raises an `Error` and exits 1. `policy/xml-unknown-typeref-refused`
  records the behaviour, which is what `CLAUDE.md` prescribes. The fixture README is
  stale.

Several defects in here were found while building the corpus and appear in no audit
document at all:

- `symptom/eval-hp-any-always-left` — hit policy `A` has never returned an answer.
- the two `symptom/cli-showtojson-*` cases, of which `cli-showtojson-unknown-format`
  is the more serious: bare `dmnmd -q file.md`, the documented interactive
  evaluator, dies on the first *successful* query. It survived because the failure
  path does not call the partial function, so every worked example that errors out
  looks fine.
- `symptom/md-quoted-string-cell-literal` — a quoted S-FEEL string literal keeps its
  quotes, so `"Fall"` compiles to a comparison against the six-character string
  including the quote marks. Found while filling a coverage gap: no case in the
  corpus contained a quote character, because the surface does not really support
  one. It matters because DMN XML writes every string that way
  (`<text>"adult"</text>`), so anything round-tripping through XML acquires cells
  this grammar misreads.
- `symptom/eval-collect-min-empty-crash` and `symptom/eval-hp-first-no-match-crash` —
  found while covering the hit policies. A Collect min/max table with no matching
  row dies inside `Prelude.minimum`, and a First table with no matching row dies in
  `head0`. Under DMN both are ordinary situations with defined answers.
- `symptom/eval-collect-{cnt,all}-debug-trace` — `Debug.Trace` left in two
  `HP_Collect` arms, printing `outputs has length 3` to stderr on every evaluation
  of a shipped binary.

## Adding a case

Keep it minimal — one defect per file, so that a diff points at one thing. Give it a
slug that names the *behaviour*, not the code (`num-suffix-lte-inverted`, not
`mkf-line-152`). Write `WHY` for somebody who has never read the audit. Then:

```
./test/corpus/run-corpus.sh --record --only 'your-new-slug'
```

and **read the recording** to confirm the case actually exercises what you think it
does. Several cases in here were rewritten at this step because the first attempt
crashed on an unrelated defect before reaching the interesting one, and one
(`policy/md-wildcard`) had a TITLE promising three cell forms while the fixture
contained two — for months, until somebody diffed the claim against the input.

Two mechanical traps when writing `case.conf`, both of which have bitten:

- **No apostrophes in `WHY`.** It is a single-quoted shell string; `dmnmd's` ends it
  early. The runner now reports this as `UNRUNNABLE` rather than letting the case run
  with no arguments, but it is still your bug to avoid. Write "of dmnmd" or reword.
- **Check the recording contains the artefact you are claiming.** A crash case is the
  easiest place to fool yourself, because every crash looks alike:
  `grep -F 'the thing you claim' expected/stdout` before you commit.
