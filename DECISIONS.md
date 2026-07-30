# DECISIONS

**Canonical.** This is the record of rulings on dmnmd's design, one entry per decision, newest
section first. If you find a decision restated anywhere else — a spec, a `WHY` field, a comment —
this file wins and corrections land here first.

**What belongs here.** A decision that a future reader would otherwise reopen: a deliberate
divergence from DMN, a choice between two defensible designs, or a "no" that looks like an
oversight. Not a bug (those go in `languages/haskell/test/corpus/`, which is machine-checked and
strictly better than prose) and not a plan (those go in a `BUILD-SPEC-*.md`).

Each entry states what was decided, why, and **what it costs us later** — because a decision whose
price is not written down gets reopened by whoever first pays it.

---

## 2026-07-30 — retiring the idiosyncrasies

Twelve divergences from DMN 1.3 were audited against the tree at `36df5a9` and ruled on. Numbering
is `D-n` and is referenced from commit messages and corpus `WHY` fields.

### D-1 — numbers become `Scientific`. **RULED: adopt.**

`VN Float` is binary32. Measured, at exit 0 and with no diagnostic: `16777217` emits as
`1.6777216e7`, `1234567.89` as `1234567.9`. In a tool aimed at insurance payout formulas that is
the worst defect in the repo, and it owes nothing to conformance work.

**Why `Scientific` over `Rational`.** FEEL's number *is* decimal128 (DMN 1.3 §10.3.2.3.1), so a
decimal representation matches the standard's own model instead of approximating it from outside.
Rounding at 34 significant digits is what the spec asks for — being *more* exact than that is not
more correct. `Rational` is what `legalese/l4-ide` chose for `VNum`, and adopting it for alignment
would buy IR similarity at the price of matching FEEL, and would still need a rendering policy in
all four backends because it renders ⅓ as `(1 / 3)`.

**Why not a decimal128 binding.** Letter-perfect, and the route `dsntk` took by binding Intel's
DFP library. It reintroduces a C dependency one commit after `36df5a9` removed the last one, will
not cross-build to wasm32, and both candidate Hackage packages are dead (`decimal-arithmetic`
2018, `deka` 2014).

**Prior art in our own tree:** `src/DMN/SFeelGrammar.hs:273` already chose `Scientific`. See D-10.

**Cost we accept.** `Types.hs`, `fromVN`, `showNumL4`, all four backends, `ParseCell.numericLiteral`.
Record `symptom/num-float32-integer-identity-lost` and `num-float32-cent-rounding` *first*, against
the current binary, or the change asserts an improvement rather than demonstrating one.

**Unblocks** native `--to=xml` (D-8), which cannot be written from a `Float` without emitting
numbers the source document does not contain.

### D-2 — type inference is anchored, and refuses what it cannot resolve. **RULED: adopt.**

`inferType` reads a column as Number if any cell contains `..`, `>`, `<`, `=` or a spaced operator;
as Boolean if a cell reads `y`, `n`, `positive`, `negative`. DMN types are declared (`typeRef`) and
never inferred, so this is dmnmd's largest single divergence — and the root of eleven recorded
`infer-*` symptoms. A cell containing `n` becomes Boolean false.

**The ruling is not "stop inferring".** Inference survives for unambiguous cells; what changes is
that a column it cannot resolve becomes a **located error** instead of a silent `String`.

**Why not require `Column : Type`.** Fully conformant, and a breaking change to the markdown
surface — which is the product. Every table written so far would stop parsing. Not without a
deprecation path.

**Cost we accept.** The failure mode being killed is not "it guessed" but "it guessed silently and
the answer was a plausible wrong one". Some tables that parse today will start refusing. That is
the intended effect and it will surprise someone.

### D-3 — temporal and enum types are **not** decided in dmnmd. **RULED: hold.**

They exist in `legalese/l4-ide` and not here, and the two trees diverged four days apart without
anyone noticing:

| | dmnmd | l4-ide |
|---|---|---|
| type vocabulary | `DMN_String \| DMN_Number \| DMN_Boolean \| DMN_List` | `DmnNumber \| DmnString \| DmnBoolean \| DmnDate \| DmnAny` (`jl4-core/src/L4/Dmn/IR.hs:91`) |
| enums | a `String` column with a domain in a sub-header row — `Types.hs:53`'s own comment says *"we use this for enums too"* | `itemDefinition`s for records and enums (`d4174bee`, merged 2026-07-29 as #175) |
| temporals | a **deliberate hard error** (`XmlToDmnmd.hs:614-641`) — degrading to `String` would turn `< date("2020-01-01")` into an equality test against that literal text | `DmnDate` |

**Both choices are defensible in isolation.** What is not defensible is that the person who made
both could not recall which repo held which. That is the first *lived* cost of maintaining two IRs
of one domain, and it is sharper evidence for the shared-core direction than the architecture
argument in the alignment memo, because it was paid rather than predicted.

**So the ruling is to spend it rather than resolve it.** Do not add a fifth `DMNType` constructor
unilaterally; fold the type-vocabulary question into the `dmn-core` conversation
(`DMN-CORE-HACKAGE-FINDINGS.md`) so the third decision is made once instead of twice more.

**Not blocked by this:** D-4 (DMN 1.5) and enum work at the *markdown* layer. What is blocked is
committing a type vocabulary that a shared core would inherit without discussion.

**dmnmd's temporal refusal stays** and is correct behaviour — it is recorded as
`policy/xml-temporal-typeref-refused`, not as a symptom.

### D-4 — accept DMN 1.4/1.5 by **parameterising** the namespace, not by re-pinning. **RULED: adopt.**

`xmlns_dmn` is pinned to 1.3 at `ParseDMN.hs:36`, with 18 references across a 1,117-line pickler
tree, and hxt's `xpElemNS` takes a concrete URI. Every model any current tool emits is 1.5
(`20230324`), including all 151 DMN TCK models — which is the entire reason dmnmd scores 0/3,493.

**The delta is additive.** For decision tables, `tDecisionTable`, `tInputClause`, `tOutputClause`,
`tDecisionRule` and `tUnaryTests` are byte-identical between 1.3 and 1.5. 1.5 adds seven
boxed-expression types (`conditional`, `for`, `filter`, `iterator`, `quantified`, and two
child-expression wrappers) and one optional `typeConstraint`, and removes nothing.

**Why parameterise rather than re-pin to 1.5.** DMN 1.6 Beta 1 is already published (OMG
`dtc/24-05-18`). Re-pinning buys exactly this week's work again.

**Why not pre-rewrite the document.** That is the cheap route and it is dishonest: dmnmd would
claim to read 1.5 while unpickling as 1.3, so the seven new constructs would surface as
`xpCheckEmptyContents: unprocessed XML content` — violating the module's own governing rule on the
very release that advertises the capability. **Refuse the seven by name, with fixtures.**

**Cost we accept.** ~1 week. Two `policy/` recordings move, because the refusal message's second
line becomes a version list rather than a single URI.

**Do not vendor `DMN15.xsd`.** `xsd/` is referenced by no code — only a comment at
`XmlToDmnmd.hs:134` — and is not a cabal `extra-source-file`. There is no runtime XSD validation to
extend.

### D-5 — `HP_Any` is two bugs, and they are fixed together or not at all. **RULED: adopt.**

The guard at `DecisionTable.hs:36` is `not (null (nub …))`, which is never false, so an `A` table
always returns `Left`. Separately, the success branch returns one entry per matched row when `ANY`
is single-hit.

**Fixing the guard alone converts six loud errors into three silently doubled answers** — and
`symptom/eval-hp-any-always-left` uses a one-row table, so it would go green while the table became
wrong. Record a two-row case first (task #22).

All six of dmnmd's Level-2 TCK failures are this hit policy; this is the whole distance from 45/51
to 51/51 on the Level-2 decision-table subset.

### D-6 — warn when the DRG is discarded. **RULED: adopt.**

`informationRequirement` is parsed into `Decision.decInfoReq` (`ParseDMN.hs:803`) and then never
mentioned in `XmlToDmnmd` — verified, zero occurrences. Same for `knowledgeRequirement`, `import`,
`textAnnotation`, `association`. Two decisions linked by `<requiredDecision>` arrive as two
unrelated functions.

This is the one place that module breaks its own governing rule; everything else honours, warns, or
refuses by name. **The Warning is an afternoon and does not wait for E1**, which is the large job of
actually modelling the graph.

### D-7 — the markdown reader returns diagnostics instead of calling `error`. **RULED: adopt, after commit 6.**

`mkFs = either error id`. One rule, two mechanisms: a bad markdown cell is an imprecise exception
carrying a Haskell `CallStack`, while the same cell arriving through XML is a structured
`Diagnostic` — which `XmlToDmnmd` does on purpose.

Commit 6 (`7a5e990`) fixes the *message* — which table, column and rule number — and deliberately
keeps the mechanism, because changing both at once would make the corpus diff unreadable at exactly
the moment it matters most. The mechanism change is its own piece of work.

### D-8 — build `--to=xml`; answer issue #13 now. **RULED: adopt.**

`Xml` is a `FileFormat` constructor with no implementation; `outputTo` falls through to a crash, and
with `-o` it creates a zero-byte file *before* erroring. dmnmd advertises a capability it does not
have.

**Gated on D-1.** Meanwhile the two-binary pipeline already answers #13 — 8,142 bytes of XSD-valid
DMN 1.3 with `@id` on every element, `<informationRequirement>` wiring and a DMNDI block, zero new
code. The reply must say three things honestly: it works today; the hit policy is lost in transit
and `--fail-on` cannot see it (task #21); native `--to=xml` is gated on the number fix, and why.

#13 is dmnmd's only open issue, filed 2023-04-11.

### D-9 — add `FNot`. **RULED: adopt.**

No `FNot` constructor exists anywhere. `not([1..5])` is refused — honest, but a hole in the
unary-test language that DMN §9.2 rule 12.b fills. Cheap now: the anchored grammar from commit 3
has somewhere to put it, which the guard chain it replaced did not. Destination for
`symptom/num-negation-refused-unlocated` is `!(1.0 <= Age && Age <= 5.0)`.

### D-10 — adopt `DMN.SFeelGrammar`. **RULED: adopt.**

It is a faithful transcription of clause 9.2's published EBNF — `UTComparison`, `UnaryTest`,
`IsOpen = Open | Closed`, `NumericLiteral Scientific` — in `exposed-modules`, imported by nothing
but its own test. dmnmd has had a conformant S-FEEL vocabulary parked for years, which is a large
part of why l4-ide had to write its own.

**Two decisions above independently want what it already has:** D-1 wants `Scientific`, and the
bracket flags added in commit 2 (`15e9f02`) are a re-derivation of its `IsOpen`. A third
re-derivation is the moment to stop. Leaving a second, better, unused type vocabulary in the library
is the worst of the three options — it is the shape of the mistake that produced this whole audit.

### D-11 — the suffix comparison form is mirrored and documented, not refused. **RULED: keep.**

`5 <=` means `>= 5`. S-FEEL's operator slot is prefix-only and there is no `<inputEntry>` spelling
for the suffix form, so this is a **dmnmd extension entered with open eyes** — author ergonomics
over conformance. Four recorded defects became four `policy/` cases.

**The price, payable at D-8:** `--to=xml` must emit the mirrored form and record a fidelity note.
Correct, but no longer isomorphic to the source.

### D-12 — a thousands-grouped number stays refused. **RULED: keep.**

`1,000` is refused, and **we are the non-conformant party**: FEEL has no grouping production, so
rule 11 plus rule 31 make it two unary tests, and `000` is a valid literal for zero.

We refuse it because the conformant reading is a wrong answer nobody wants — a threshold meaning
"at least 1, or exactly 0". The escape hatch is one space and the message says so.

Recorded here so the next reader does not rediscover it as a bug, and so the **retracted** diagnosis
in the original corpus recording stays visible: that recording claimed a misparse, and the misparse
claim was wrong. What was actually wrong was the silence.
