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

### D-1 — numbers become `Scientific`. **RULED: adopt. LANDED.**

> **Landed, with four things this entry did not anticipate.** Each is a measurement made while
> implementing, not a re-reading.
>
> 1. **`**` did not compile and `/` was a live trap — neither is in the "cost we accept" list.**
>    `Scientific` has no `Floating` instance, so `fNEval`'s `lhs ** rhs` was a hard type error with
>    no coercion available; and its `Fractional` instance *does* compile while raising a bare
>    library `error` on any repeating decimal (`(1 :: Scientific) / 3`), carrying a
>    `Data/Scientific.hs` call stack and a build-specific package hash that `run-corpus.sh`'s
>    normaliser does not strip. Both now go through `DMN.Number`: division is computed exactly as a
>    `Rational` and rounded half-to-even to 34 significant digits (decimal128, which is what the
>    ruling above already asks for), and a fractional exponent is **refused** rather than routed
>    through `Double`, which would reintroduce the binary rounding this decision exists to remove.
>    `fNEval` therefore returns `Either String DMNVal` and `evalTable` threads it through the `Left`
>    channel it already had.
>
> 2. **Two silent wrong answers at exit 0 became refusals, and that is a user-visible change.**
>    `x / 0` was `Infinity` — printed as `Infinity` by ts/js/py and as a plain, innocent-looking
>    `0` by L4, because `showNumL4` caught `isInfinite`. And a huge exponent had `Infinity` to fall
>    into; a decimal does not, so `DMN.Number.maxBase10Exponent` bounds the magnitude before
>    anything tries to spell it. Recorded as `policy/num-divide-by-zero-refused` and
>    `policy/num-fractional-exponent-refused`.
>
> 3. **`showNumL4` was not the only renderer that mattered — the DIAGNOSTICS were the fifth site,
>    and this entry does not mention them.** `showDomainMember` rendered numbers with bare `show`,
>    so a refusal quoted a cell the author wrote as `> 3` back at them as `"> 3.0"`, in violation of
>    that function's own haddock. `num-dash-range-refused` was the proof: one sentence said the cell
>    reads `"40.0 - 50.0"` and the next told the author to write `[40..50]` "not `40 - 50`". Five
>    `policy/` recordings were re-recorded for this, which is the one thing in the change a reviewer
>    must audit.
>
> 4. **The rendering blast radius was two cases, not seventy, and the reason is worth writing
>    down.** `show @Scientific` inherits `show @Float`'s fixed-vs-exponent rule exactly, so a naive
>    swap looks reassuringly small *and is wrong*: it spells `16777217` as `1.6777217e7`, the right
>    value with text the author never wrote, which no typechecker and no test would catch. All four
>    backends therefore **format** rather than `show`, through two functions in one module:
>    `showNumPlain` (L4 and diagnostics — L4 has no exponent production at all, so `1.0e8` there
>    lexes as `1.0` applied to a variable named `e8` and the file does not typecheck) and
>    `showNumFloatish` (js/ts/py, which keeps the `.0` these backends have always emitted, because
>    dropping it changes a generated Python value from `float` to `int`). Against Survey C's
>    5,544-file baseline over 231 fixtures × 8 modes, exactly the five diagnostics and the two
>    float32 fixtures move, and nothing else — not `README.md`, not `test/golden/miles-card`, not
>    the ditto grid.
>
> Also: this entry cites the prior art as `SFeelGrammar.hs:273`; it is at `:272`.

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

> Both halves of that pointer are now stale. The line is `:272` (corrected above), and the file no
> longer exists — D-14 deleted it. `Scientific` arrived in `DMN.ParseCell.numericLiteral` instead,
> and that is where the choice lives now.

**Cost we accept.** `Types.hs`, `fromVN`, `showNumL4`, all four backends, `ParseCell.numericLiteral`.
Record `symptom/num-float32-integer-identity-lost` and `num-float32-cent-rounding` *first*, against
the current binary, or the change asserts an improvement rather than demonstrating one.

**Unblocks** native `--to=xml` (D-8), which cannot be written from a `Float` without emitting
numbers the source document does not contain.

### D-2 — type inference is anchored, and refuses what it cannot resolve. **RULED: adopt. LANDED.**

> **Landed, with five things this entry did not anticipate.** Each is a measurement made while
> implementing, not a re-reading. The corrections to the entry's own *description* of the code
> were made first, as their own commit, and are the block after this one.
>
> 1. **A third of the symptom list is fixed by refusing LESS, not more.** This entry is framed
>    entirely as "start refusing", and `infer-prose-with-dots-crash`,
>    `infer-prose-with-angle-crash` and `infer-negative-poisons-column` are the opposite: they
>    currently exit 1 or emit string equality, and under anchoring they simply work. Anchoring is
>    two changes, not one — the recogniser gets *narrower* about what counts as numeric evidence
>    (prose containing `>` stops being evidence) and *wider* about what a number is (`-5` and `.5`
>    start being evidence, because the oracle is now `DMN.ParseCell.parseNumberCell`, the same
>    function the declared path already used). Reusing it is the house pattern: `domainErrors`
>    shares `fEval` "so the check cannot drift from it".
>
> 2. **"Some tables that parse today will start refusing" cost, measured: ZERO.** Across 237
>    fixtures × 8 output modes — every `.md` and `.dmn` under `test/`, both READMEs, the corpus, the
>    golden files — exactly eight recordings change stdout or exit status, and all eight are the
>    `infer-*` symptom fixtures this ruling exists to fix. Nine more change one sentence of one
>    diagnostic. Nothing else moves. The root `README.md` is byte-identical. The one file that
>    newly hits a *column-level* refusal, `languages/haskell/README.md`, already exited 1 before
>    the change, on the same column, for the same reason — it is the package README, it has been
>    broken all along, and it is in no test and no corpus case. The scary-sounding price is not
>    payable against this repo; it will be payable against tables in the wild.
>
> 3. **A `Left` from the oracle is not always "not a number", and getting that wrong reintroduced
>    the exact defect this ruling removes.** `parseNumberCell` refuses two constructs BY NAME —
>    FEEL negation and invocation — and those mean "unmistakably numeric, and dmnmd does not
>    implement it". Read as "not a number", `not([1..5])` stopped being numeric evidence, its
>    column typed `String`, and `symptom/num-negation-not-implemented` went from a located exit-1
>    refusal to `Age === "not([1..5])"` at exit 0. Caught by the corpus, not by reading. Hence
>    `DMN.ParseCell.namedRefusal`, and a new `policy/md-negation-in-numeric-column-refused`.
>
> 4. **The refusal predicate is `length coltypes > 1`, and the predicate that looks equivalent is
>    not.** "The column has no type" and "the column's cells disagree" are both spelled
>    `vartype = Nothing`, and a refusal keyed on the first would refuse every all-wildcard column —
>    a legitimate and common shape — while nothing in the tree turned red, because no hspec
>    expectation and no corpus recording covered one. One had to be constructed:
>    `policy/infer-all-wildcard-column-silent`. It is now covered in both places.
>
> 5. **The declared type winning was an ACCIDENT, and the accident was load-bearing for
>    collections.** `inferTypes` used to preserve a declared type only because its
>    declared-vs-inferred disagreement branch happened to return the header unchanged. That branch
>    fired on every declared collection column on every run — a `roles : [String]` column's cells
>    infer `DMN_String`, never `DMN_List DMN_String` — so turning it into a diagnostic without an
>    element-type exemption would have made `README.md` stop parsing. `inferTypes` now tests the
>    declared type *first* and returns early, which makes the invariant explicit and the exemption
>    unnecessary.
>
> **Where it landed.** `inferEvidence`/`columnVerdict` replace `inferType`'s raw-text arm;
> `inferenceErrors` sits in `tableErrors` beside `structuralErrors` and `domainErrors`, NOT inside
> `inferTypes`, because `mkDTable` transposes rows into columns before calling `inferTypes` and
> drops the rule numbers on the way — a refusal raised there could not name a row, and every other
> cell-layer diagnostic can. Putting it in `tableErrors` also hands the XML reader a located
> `Diagnostic` for free rather than an `error` it cannot catch.
>
> **One policy ADDITION this entry does not contain**, made explicitly rather than silently: a
> redundant *leading* zero (`007`, `042`) is refused as ambiguous between the number and an
> identifier, because no numeric representation can fix `Code === 7.0` matching a real-world key
> `"007"`. Deliberately narrower than the general rule "the source text is not the canonical
> spelling of the value", which would also catch `1.10` — and `10.50` and `2.0` with it. Refusing a
> money column for writing cents is worse than the defect. `1.10` is caught anyway, by the
> hit-policy check D-13 has since landed, which is the larger class and needs no types at all.
>
> **What this did NOT reach**, all three still recorded as symptoms with WHYs saying so:
> `infer-version-float-collapse` (both cells are genuine numbers, so there is no disagreement to
> refuse — fixed since, by D-13's hit-policy check rather than by anything about types, and now
> `policy/hp-unique-duplicate-rows-refused`); `infer-explicit-type-contradiction-silent` (the declaration must win, so the
> remedy is a *warning*, and the markdown path has no warning channel — D-7); and the two `n`
> crashes plus the multi-value re-parse bug, which are not inference at all and were fixed
> alongside in their own commit.

> **Three corrections to this entry's own description of the code, made before implementing it and
> confirmed independently by three readers of the tree at `f44881c`.** The ruling below survives
> unamended; what follows is only the mechanism it rules on, restated truthfully.
>
> 1. **It is a chain of `isInfixOf` tests, not a regex, and it lives where this entry says it
>    does.** `regex-pcre` went at `62d06d5` and `CLAUDE.md` records that "the `inferType`
>    classifiers" were rewritten — which reads, in context, as if inference had moved. It has not.
>    The guessing is still `inferType (FNullary (VS arg))` in `src/DMN/DecisionTable.hs`, and the
>    accepted language is unchanged: `anchoredDigits arg || any (`isInfixOf` arg) ["..",">","<","="]`
>    for Number, an eight-word list for Boolean, `[" * "," + "," - "," / "," ** "]` for Number
>    again. `anchoredDigits`' own haddock says it is deliberately `^\d+(\.\d+)?$` and deliberately
>    not rule 31. `DMN.ParseCell` — the anchored grammar — does no guessing; it is the downstream
>    reader that inference hands a column to, and every located "not a number" refusal in the
>    `infer-*` symptoms is ParseCell reporting truthfully about a type inference chose wrongly.
>
> 2. **`inferType`'s type signature is misleading and hides why this is one function clause.**
>    It is `FEELexp -> Maybe DMNType`, so it looks as though it classifies an already-parsed cell.
>    For a column with no `: Type` it does not: pass 1 calls `mkFEither Nothing`, whose only arm is
>    `Right (FNullary (VS (trim arg)))`, so every cell of an undeclared column arrives as raw text
>    and the `VS` arm is the *only* arm that ever fires. The other seven arms are reachable only for
>    declared columns, whose verdict `inferTypes` then discards. The whole surface of this ruling is
>    that one clause plus the aggregation in `inferTypes`.
>
> 3. **"A cell containing `n` becomes Boolean false" is false as written — `n` aborts.** The
>    tool holds *three* boolean vocabularies: `inferType`'s
>    `[true,yes,positive,y,false,no,negative,n]`, and `mkVB`'s two lists
>    `[true,yes,t,y,positive]` / `[false,no,t,y,negative]` — the second being a copy-paste of the
>    first's short forms, so `t` and `y` are unreachable there and `f` and `n` appear in neither.
>    So `n` infers Boolean and then fails to build: exit 1, recorded as
>    `symptom/infer-boolean-n-crash`. The honest demonstration of this ruling's premise is a `y`/`no`
>    column, which is inferred Boolean and emits `Middle === true` / `Middle === false` at exit 0.
>    That vocabulary disagreement is a separate one-line defect, not this ruling's business.

DMN types are declared (`typeRef`) and never inferred, so this is dmnmd's largest single divergence
— and the root of eleven recorded `infer-*` symptoms.

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

### D-4 — accept DMN 1.4/1.5 by **parameterising** the namespace, not by re-pinning. **RULED: adopt. LANDED.**

> **Landed, with four factual corrections to this entry.** Everything measured against the TCK's
> own `DMN13.xsd`, `DMN14.xsd` and `DMN15.xsd` while implementing; each correction is a
> measurement, not a re-reading of spec prose.
>
> 1. **The boxed expressions are DMN 1.4, not 1.5.** `DMN14.xsd` and `DMN15.xsd` have an
>    *identical* set of 50 complex types. The whole 1.4 → 1.5 delta is the namespace bump plus
>    `typeConstraint`. A 1.5 document inherits them, so what you refuse is unchanged — but what
>    you *say* when you refuse it is, and "a 1.5 construct" would be false.
> 2. **Five refusable names, not seven.** The seven new complex types yield exactly five global
>    elements: `conditional`, `for`, `some`, `every`, `filter`. The three that cannot be spelled
>    are `tIterator`, `tChildExpression` and `tTypedChildExpression`, which simply have **no
>    global `<xsd:element>` declaration**; the last two are the types of the named children
>    `in`/`return`/`satisfies`/`if`/`then`/`else`/`match`, declared only inside the five parents
>    above and so unreachable in a valid document without one of them. This entry's original list
>    also omitted `some`/`every` and named two unspellable things.
>
>    **A correction that had to be made twice.** The first fix said `tIterator` and `tQuantified`
>    are *abstract bases*. That is false on both counts, and it shipped to four files before a
>    verifier checked it. `tQuantified` has **two** global elements — `<xsd:element name="every">`
>    and `name="some"` at `DMN15.xsd:558-559` — which the very same paragraph already listed as
>    refusable. And **no** complexType in that schema is `abstract="true"`: all seven occurrences
>    of that attribute are on *element* declarations, such as the substitution-group head
>    `<xsd:element name="expression" abstract="true"/>` at `:223`. The count was always right; the
>    reason was invented, and then copied. `grep -n 'abstract="true"' DMN15.xsd` settles it.
> 3. **`xmlns_dmn` had 16 occurrences, 3 of them in comments, so 11 live use sites** — not 18. And
>    26 of the 27 element picklers already went through one wrapper, so the constant was never the
>    work. **The class was**: hxt's `xpickle :: PU a` is a value with nowhere to put a parameter.
> 4. **`xmlns_dmndi` had to be parameterised too, and this entry does not mention it.** DMNDI
>    versions *independently*: `DMNDI15.xsd` targets `…/20230324/DMNDI/`, and 141 of the TCK models
>    declare it. Worse, DMN 1.4 pairs a **1.4** model namespace with the **1.3** DMNDI namespace —
>    `DMN14.xsd` imports `schemaLocation="DMNDI13.xsd"` and no `DMNDI14.xsd` exists — so a release
>    is two independent URIs and any `mkRelease :: Date -> DmnRelease` is wrong on its first use.
>    Had only the model namespace been parameterised, `<dmndi:DMNDI>` would have resurfaced as
>    `xpCheckEmptyContents` — the exact generic failure this ruling forbids, by the back door.
> 5. **`typeConstraint` was worse than this entry feared, in the other direction.** The stated
>    rationale is that the new constructs "would surface as `xpCheckEmptyContents`" — loud but
>    generic. True of the five boxed expressions; false of `typeConstraint`, which was not
>    surfacing at all. `ItemDefinition`'s pickler filters its children by name, and a name filter
>    **deletes** what it does not list, so the element never reached the unpickler. Measured on the
>    pre-change binary: adding a `<typeConstraint>` that narrows an `<itemDefinition>` already
>    carrying `<allowedValues>` produced byte-identical output, empty stderr and exit 0, while
>    emitting a rule matching a value the document forbids. That is the silent-widening case, not
>    the generic-message case, and it is recorded as it behaved in
>    `policy/xml-typeconstraint-refused` (recorded as a `symptom/` first, then moved).
>
> Also worth pinning before anyone quotes it: **this buys a truthful conformance claim, not a TCK
> score.** Measured on `compliance-level-2`, 27 of 28 models now read where 0 did; but only 17 of
> those emit a decision table, and the single refusal is a `<businessKnowledgeModel>` — a DMN *1.3*
> construct dmnmd never modelled. Across the whole TCK only about a fifth of the models contain a
> `<decisionTable>` at all, and none of the five boxed-expression models does.

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

### D-5 — `HP_Any` is two bugs, and they are fixed together or not at all. **RULED: adopt. LANDED.**

> **Landed, with three notes.**
>
> 1. **The two-row recording this entry asks for already existed.** It says "Record a two-row case
>    first (task #22)", and `symptom/eval-hp-any-multirow-duplicated` — two matching rows with
>    identical outputs, recorded pre-fix, with a `WHY` naming the doubled-answer trap — had been in
>    the tree since `2edd4a3`. What was missing was its complement: two rows that match and
>    **disagree**. Every ANY case in the corpus agreed, so every one of them is satisfied by an arm
>    that merely stopped refusing. `policy/eval-hp-any-two-rows-disagree` is that control, and it is
>    the one ANY recording whose text is byte-identical either side of the fix — before, it was what
>    every A table got; after, only a genuine conflict reaches it.
> 2. **Agreement is compared on the whole output ROW, not per column.** A table whose matching rows
>    agree on one output column and differ on another is refused. Nothing in the tree held that
>    shape; it was probed directly, along with three-row partial agreement and the no-match case.
> 3. **The measurement holds, and the doubling was real.** TCK `compliance-level-2`, decision-table
>    subset: 45/51 → 51/51, all six from the two ANY models, measured on both binaries rather than
>    quoted. With a guard-only fix, `0117-multi-any-hitpolicy/001` returns its answer **twice** — a
>    value-presence comparator scores that as a pass, so a half-fix could have reported 51/51 while
>    being wrong. The hspec block added here catches exactly that: 3 of its 5 examples fail against
>    the unfixed code, and against the guard-only half-fix exactly one fails, the single-hit one.
>
> Also: this entry cites `DecisionTable.hs:36`; the guard was at `:45` when the work started. The
> prose was exact and only the number had drifted, so it is not corrected here — it would drift
> again on the next edit to the arm. See `test/corpus/README.md` on why line numbers are not written
> down.

The guard at `DecisionTable.hs:36` is `not (null (nub …))`, which is never false, so an `A` table
always returns `Left`. Separately, the success branch returns one entry per matched row when `ANY`
is single-hit.

**Fixing the guard alone converts six loud errors into three silently doubled answers** — and
`symptom/eval-hp-any-always-left` uses a one-row table, so it would go green while the table became
wrong. Record a two-row case first (task #22).

All six of dmnmd's Level-2 TCK failures are this hit policy; this is the whole distance from 45/51
to 51/51 on the Level-2 decision-table subset.

### D-6 — warn when the DRG is discarded. **RULED: adopt. LANDED, narrower than written.**

> **Landed for `informationRequirement` only, and the other four names in this entry are wrong.**
>
> 1. **"Same for `knowledgeRequirement`, `import`, `textAnnotation`, `association`" is false.**
>    Those four are not parsed-and-dropped: `xpIgnoredElemsOf` / `xpIgnoredElems` consume them to
>    `()` and keep **nothing**, not even a count. `XmlToDmnmd` cannot warn about what never entered
>    the AST, so warning about them is a change to the *pickler tree* — a different and larger job
>    than the afternoon this entry scopes. Only `informationRequirement` is genuinely
>    parsed-then-ignored, and it is the one that landed.
> 2. **Two further silent drops sit beside them and appear in no ruling.** `defInputData` is
>    written by the pickler and read by nothing in `src/`, so every `<inputData>` node and the
>    `typeRef` on its `<variable>` vanish; and `<knowledgeSource>` survives parsing as `DrgKS` only
>    to be filtered out by `allDecisions`, which comprehends `DrgDec` alone. `<authorityRequirement>`
>    is consumed to `()` like the four above.
> 3. **The remainder is recorded rather than described.** `symptom/xml-drg-siblings-dropped-silently`
>    is one document carrying seven DRG constructs, of which exactly one now warns. That is the
>    machine-checked version of this note, and the place a future fix will show up.
> 4. **The consequence sentence differs by edge kind**, because one sentence is wrong for one of
>    them: a `<requiredDecision>` tells you to run that decision first and pass its result in, while
>    a `<requiredInput>` tells you the `<inputData>` node is not modelled and its `typeRef` is not
>    applied.
> 5. **Collateral, larger than expected**: 15 further corpus recordings gained stderr lines, and
>    seven `DmnXmlSpec` examples asserted `warns == []` on fixtures that each carry one
>    `<informationRequirement>`. Those became `shouldBeOnlyDrgWarnings 1` — which counts the DRG
>    warnings *and* still requires every other diagnostic to be absent — rather than being relaxed
>    to ignore warnings, which would license a future warning about something genuinely wrong.
>
> This entry's citation is also stale: it gives `ParseDMN.hs:803`, which is `instance DmnPU
> InputEntry`. The real sites are the type at `:392`, the pickler at `:403`, the field at `:921`.

`informationRequirement` is parsed into `Decision.decInfoReq` and then never
mentioned in `XmlToDmnmd` — verified, zero occurrences. Two decisions linked by
`<requiredDecision>` arrive as two unrelated functions.

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

### D-8 — build `--to=xml`; answer issue #13 now. **RULED: adopt. LANDED.**

> **Landed, with five things this entry did not anticipate.** Each is a measurement made while
> implementing, not a re-reading.
>
> 1. **The zero-byte file is worse than "created before erroring": it is destruction of
>    pre-existing data, and it was never Xml-specific.** `myOutHandle` ran before `parseTables`
>    and `openFile … WriteMode` truncates, so ANY failing run with `-o` emptied the user's file —
>    measured on `-f xml -t ts test/dmn13/temporal-type.dmn`, a path that existed with no new code.
>    All three surveys found it independently. Fixed as its own commit: `renderAll` produces the
>    whole text and `withOutHandle` opens the destination only to write it.
>
> 2. **The write half of the DMN serialiser was already in the tree, unused.** hxt picklers are
>    bidirectional, so `dmnPickler` — the function that READS DMN — is the function that writes it.
>    What D-8 needed was a pure `DecisionTable -> Definitions` mapping and a cell renderer, not a
>    second description of DMN's element structure that could drift from the reader's. This is the
>    single biggest reason the backend was small.
>
> 3. **Two hxt facts that no amount of reading the picklers would have found.** hxt drops namespace
>    declarations in the pickling direction (`xpElemNS` builds a universal name, the writer emits
>    the qualified one), so `xmlns` has to be added to the root as an ordinary attribute after
>    `pickleDoc`; and `ShowXml.xshow` does **not escape**, so the first whole-file output was
>    literally `<text><= 0</text>` and was not well-formed XML at all. The second refutes Survey A,
>    which reported escaping working — measured through `writeDocumentToString`, the IO writer,
>    which is a different function.
>
> 4. **Four constructs dmnmd accepts that DMN cannot spell, and none was on the ruling's list.**
>    A wildcard OUTPUT cell (`-` is rule-12 syntax; an `<outputEntry>` is a literal expression) —
>    emitted as an empty `<text/>`, warned. A comparison in an output cell — refused. A table with
>    no output column, and a row shorter than the table — both refused, the second because padding
>    with `-` would widen the rule in a valid document at exit 0. Plus one silent normalisation
>    that is safe because it is invisible downstream: `= 5` in an input cell becomes a bare `5`,
>    which is DMN's equality test and which `JS.feel2jsIn` renders identically.
>
> 5. **The two-binary pipeline's byte count should never have been in this entry.** It says 8,142;
>    `DMN-CORE-HACKAGE-FINDINGS.md` §6 says 8,132; Survey B measured 8,152 today. It is a
>    build-pair fingerprint, not a property of the pipeline, and it got copied twice and sharpened
>    once. The claim that survives is the shape: the pipeline loses the hit policy in both
>    directions and `--fail-on lossy` cannot see it, and native `--to=xml` reads `hitpolicy dt` and
>    cannot.
>
> The gate: 117 of 120 eligible markdown fixtures round-trip `--to=xml | --from=xml --to=ts`
> byte-identically to `--to=ts`, every emitted document validates against `xsd/DMN13.xsd`, and the
> eight XFAILs are enumerated with reasons in `test/roundtrip/run-roundtrip.sh`.

`Xml` is a `FileFormat` constructor with no implementation; `outputTo` falls through to a crash, and
with `-o` it creates a zero-byte file *before* erroring. dmnmd advertises a capability it does not
have.

**Gated on D-1.** Meanwhile the two-binary pipeline already answers #13 — 8,142 bytes of XSD-valid
DMN 1.3 with `@id` on every element, `<informationRequirement>` wiring and a DMNDI block, zero new
code. The reply must say three things honestly: it works today; the hit policy is lost in transit
and `--fail-on` cannot see it (task #21); native `--to=xml` is gated on the number fix, and why.

#13 is dmnmd's only open issue, filed 2023-04-11.

### D-9 — add `FNot`. **RULED: adopt. LANDED, for Number columns and the single-test form.**

> **Landed, with five things this entry did not anticipate.** Each is a measurement made while
> implementing.
>
> 1. **"Every consumer becomes a compile error" is false, and the stated hazard is the wrong one.**
>    Four sites error; ten more carry catch-alls and compile silently. And `app/` — which the brief
>    warns is uncovered by `-Werror=incomplete-patterns` — needs **no change at all**, because it
>    pattern-matches no `FEELexp` constructor anywhere. The real risk was inside the library.
> 2. **An output cell had to be refused, or D-9 is a regression.** `parseNumberCell` cannot tell an
>    input cell from an output cell, so its old refusal covered both. Once `not(…)` builds, an output
>    cell would reach a backend and crash. `structuralErrors.outputNegationErrs` replaces it, and
>    mirrors `inputArithErrs` exactly: arithmetic is legal only in an output cell, negation only in
>    an input one.
> 3. **`wrapParen` returns a one-element list UNBRACKETED**, so the obvious `"!" ++ wrapParen …`
>    emits `!Age < 5.0`, which JS parses as `(!Age) < 5.0` — run under node, it returns the same
>    answer for every input. Both backends force their own parentheses.
> 4. **The named destination does not move.** This entry points at
>    `symptom/num-negation-refused-unlocated`; the case is called `num-negation-not-implemented`,
>    and it is byte-identical after a correct fix, because its refusal is column-level and driven by
>    a malformed row 2 rather than by the negation. The case that moves is a **`policy/`** one, so a
>    correct D-9 produces a policy regression on purpose.
> 5. **Three limits, all recorded rather than left to be rediscovered.** `mkFEither`'s `DMN_String`
>    arm consults no grammar, so `not(Fall)` is still `Season === "not(Fall)"` at exit 0 —
>    pre-existing, but now an *inconsistency*, and recorded as
>    `symptom/md-negation-in-string-column-silent`. It is not fixed here because there is no escape
>    hatch: `unquoteCell` strips quotes before the type arms run, so quoting cannot force the literal
>    reading, and that decision belongs with the quoting rule. `not(a, b)` needs a bracket-aware
>    splitter (`splitArgs` exists) that would move three unrelated policy recordings. And a
>    collection column still refuses negation, correctly.
>
> Also: `FNLog`'s `FNNot` constructor and its ts/py renderings had existed, dead, since before this
> work — half the emitter vocabulary was already written and wired to nothing.

No `FNot` constructor exists anywhere. `not([1..5])` is refused — honest, but a hole in the
unary-test language that DMN §9.2 rule 12.b fills. Cheap now: the anchored grammar from commit 3
has somewhere to put it, which the guard chain it replaced did not. Destination for
`symptom/num-negation-refused-unlocated` is `!(1.0 <= Age && Age <= 5.0)`.

### D-10 — adopt `DMN.SFeelGrammar`. **RULED: adopt. ~~RETRACTED~~ — see the correction below.**

It is a faithful transcription of clause 9.2's published EBNF — `UTComparison`, `UnaryTest`,
`IsOpen = Open | Closed`, `NumericLiteral Scientific` — in `exposed-modules`, imported by nothing
but its own test. dmnmd has had a conformant S-FEEL vocabulary parked for years, which is a large
part of why l4-ide had to write its own.

**Two decisions above independently want what it already has:** D-1 wants `Scientific`, and the
bracket flags added in commit 2 (`15e9f02`) are a re-derivation of its `IsOpen`. A third
re-derivation is the moment to stop. Leaving a second, better, unused type vocabulary in the library
is the worst of the three options — it is the shape of the mistake that produced this whole audit.

> **Retracted. The ruling is wrong on its facts, and it was written against a tree that did not
> contain the module it was ruling against.** Three independent surveys re-measured it; this note
> records only what I re-ran myself, in a `ghci` session against the library at `603676f` and
> through the built binary. The re-ruling is D-14.
>
> 1. **"A faithful transcription of clause 9.2's published EBNF" is true of the module's *comments*
>    and false of its *parsers*.** Measured, calling each parser directly with `<* eof`:
>    `interval` refuses **all** of `[1..5]`, `(1..5)`, `(1..5]`, `]1..5[`; rule 12
>    (`simpleUnaryTests`, the production an input entry *is*) refuses `5`, `-5`, `.5`, `<= 8`,
>    `>= 3`, `= 5`, `5 <=`, every interval spelling, `not([1..5])`, `not(> 3)` and the `-` wildcard,
>    accepting `< 5` and `<5` and returning `[]` on the empty string; `simpleLiteral "true"` is
>    `StringLiteral "true"`, never `BooleanLiteral True`; `simpleLiteral` refuses
>    `date("2020-01-01")` outright; `expression` refuses `(1+2)*3`, `-Age`, `Age * 2` and
>    `Units / 2`, and reads `--` as `Neg (QName ["-"])`.
>
>    Four root causes, each one line. `simplePositiveUnaryTests` is `sepBy … ","`, which succeeds
>    with `[]` on any input, so under `try` it makes rule 12.b (`not(…)`) and rule 12.c (`-`) at
>    `:172-173` unreachable dead code. The `choice` at `:124-127` lists `Lt <$ "<"` before
>    `Le <$ "<="`, so `<=` is unreachable — a defect `ParseCell.hs:167-169` already documents in
>    prose, three days before this ruling called the module faithful. `:155` spells
>    `closedIntervalStart = Closed <$ "]"` where rule 8 says `"["`, so `[` opens an interval
>    nowhere, and `:325`'s `option` has no `try`, so `numericLiteral` consumes the first dot of
>    `..` and cannot back out — the exact trap `ParseCell.hs:226-229` documents as the reason for
>    *its* `try`. And `ParsingUtils.inClass` is `(\`elem\` cs)`: it does **not** expand ranges, so
>    every character class in the module is literal set membership over its own notation.
>    `nameStartChar` is `[True,False,True,False,True]` on `A`, `B`, `z`, `t`, `-` — four of the 52
>    ASCII letters are legal name starts, and the hyphen from the literal `"A-Z"` is one too.
>    That is why `Age` reads as `QName ["A"]` and why rules 22-27 and 36-38 do not implement what
>    their comments say.
>
> 2. **"Imported by nothing but its own test" is still true** — `grep` finds no importer under
>    `src/` or `app/` at `603676f`.
>
> 3. **"A third re-derivation is the moment to stop" had its premise expire, and the git graph is
>    what shows it.** This entry landed in `63ecac8`, whose section header says the twelve
>    idiosyncrasies were "audited against the tree at `36df5a9`". `36df5a9` is **not an ancestor**
>    of `63ecac8` — they were concurrent branches on the same afternoon — and
>    `git ls-tree 63ecac8 languages/haskell/src/DMN/ParseCell.hs` is **empty**, while the same
>    command at `36df5a9` returns a blob. So the ruling weighed `SFeelGrammar` against a cell layer
>    that had no extracted anchored grammar in it, while citing a commit that did. Anyone
>    re-reading the entry and checking its cited commit would find `ParseCell` present and conclude
>    the ruling considered it. It did not, and could not: the second derivation was not in the tree
>    it saw.
>
>    The second derivation has since absorbed D-2's anchoring and inference oracle, D-9's `FNot`,
>    D-11's mirrored suffix form, D-12's thousands-grouping refusal, and every located diagnostic
>    in the cell layer. The question this entry answered — "should we re-derive again?" — is no
>    longer live.
>
> 4. **History already held the answer, three days early.** `6edea2c` (2026-07-27) removed the last
>    dead `import DMN.SFeelGrammar` and its message records that the module "has never been
>    connected to the pipeline", names the abandoned 2023 branch that tried, and reports that the
>    attempt changed nothing across the whole corpus. It also calls the module "the obvious
>    starting point whenever the cell layer is rewritten" — a hedged note about a future rewrite,
>    which three days later had become a ruling to adopt. That is the sharpening `~/CLAUDE.md`
>    rule 2 names, and this entry is an instance of it.
>
> **What survives.** The entry's closing sentence — leaving a second, unused type vocabulary in the
> library is the worst of the three options — is right, and still governs. It now argues for
> deletion.

### D-11 — the suffix comparison form is mirrored and documented, not refused. **RULED: keep.**

`5 <=` means `>= 5`. S-FEEL's operator slot is prefix-only and there is no `<inputEntry>` spelling
for the suffix form, so this is a **dmnmd extension entered with open eyes** — author ergonomics
over conformance. Four recorded defects became four `policy/` cases.

**The price, payable at D-8:** `--to=xml` must emit the mirrored form and record a fidelity note.
Correct, but no longer isomorphic to the source.

> **The second half of that price is unpayable where this entry puts it, and D-8 landing is what
> proved it.** `DMN.ParseCell.suffixCmp` mirrors at PARSE time — `5 <=` is built directly as
> `FSection Fgte (VN 5)` — and `FEELexp` has no provenance field. So by the time any backend runs,
> `5 <=` and `>= 5` are *the same value*, indistinguishable by construction. The emitter gets the
> mirrored form for free and has nothing to detect; a diagnostic written to fire on the suffix form
> would be one that never fires.
>
> The empirical proof, rather than the argument: all four `policy/num-suffix-*` fixtures round-trip
> **clean** through `--to=xml`, because both routes canonicalise identically. Two independent
> surveys reached this before implementation and the measurement agreed.
>
> **So the note lives where the information does, which is nowhere in the IR** — and rather than
> retire a recorded price in silence, it is paid three other ways: `policy/xml-suffix-cmp-mirrored`
> records the emitted `>= 5` and this whole argument in a machine-checked place; the XFAIL list in
> `test/roundtrip/run-roundtrip.sh` names the absence and says why; and `README.md`'s `to XML`
> section tells the author directly.
>
> Paying it properly needs one of three things, none of which belongs to D-8: provenance on
> `FSection`, which touches every backend and its `Eq`; an unconditional parse-time warning, which
> needs the markdown diagnostic channel D-7 has not landed; or accepting that the record IS the
> note. This entry now says the third, deliberately, rather than by omission.

### D-12 — a thousands-grouped number stays refused. **RULED: keep.**

`1,000` is refused, and **we are the non-conformant party**: FEEL has no grouping production, so
rule 11 plus rule 31 make it two unary tests, and `000` is a valid literal for zero.

We refuse it because the conformant reading is a wrong answer nobody wants — a threshold meaning
"at least 1, or exactly 0". The escape hatch is one space and the message says so.

Recorded here so the next reader does not rediscover it as a bug, and so the **retracted** diagnosis
in the original corpus recording stays visible: that recording claimed a misparse, and the misparse
claim was wrong. What was actually wrong was the silence.

### D-13 — two rows of a `U` table with identical guards. **RULED: refuse, exact duplication only. LANDED.**

`symptom/infer-version-float-collapse` writes `1.1` and `1.10` in an undeclared column. Both are
genuine FEEL numbers, so anchored inference (D-2) resolves the column unambiguously to `Number` and
has nothing to refuse; `Scientific` stores them distinguishably (11e-1 versus 110e-2) but its `Eq`
compares on value and ignores scale, so the two rows emit **byte-identical guards** and the second
is dead code in a table whose hit policy promises uniqueness.

**Two candidate fixes, and the obvious one is the worse one.**

*Extend the leading-zero ambiguity rule to any redundant zero* — "the source text is not the
canonical spelling of the value it denotes". This catches `1.10`. It also catches `10.50` and `2.0`,
so an insurance payout column written in cents stops parsing and is told to declare itself. A
redundant **trailing** zero is ordinary decimal notation; a redundant **leading** zero is not
notation at all, which is why D-2 adopted only the leading-zero half. Rejected.

*Check hit-policy overlap.* Two rules of a `U` table with identical guards violate DMN's own
uniqueness requirement (DMN 1.3 §8.2.10). It is detectable with **no reference to types**, it
catches a strictly larger class than any zero rule — including the ordinary case of two rows that
genuinely say the same thing — and it belongs to the hit policy, which is where the promise lives.
This is the better fix.

**Not done in D-2** because it is a new check with its own scope question (`U` only, or `A` and `P`
too? overlap, or only exact duplication? the interval case `[1..5]` versus `[3..8]` is a decision
in itself) and D-2 was not the place to make it. `evalTable` already detected the runtime symptom —
`HP_Unique` returns "multiple rows returned" — so what was missing is the *static* check that the
transpilers need, since they do not implement hit policies at all.

**Cost of leaving it**, which is why it did not stay left: a `U` table could be emitted with a
permanently dead row, at exit 0, in every backend. That is a silent wrong answer, exactly the class
D-2 exists to remove.

#### What landed

`DMN.DecisionTable.uniquenessErrors`, a fourth summand of `tableErrors`, so **both readers** get it
— the markdown path through `mkDTable`'s `error`, the XML path through
`DMN.XML.XmlToDmnmd`, which calls `tableErrors` directly and so locates the diagnostic and refuses
just that table. The message names the table, **both** colliding rows by the number the author
wrote, and the guard they share, and offers two repairs (delete a row, change a guard) that were
both run before shipping.

Four decisions inside it, each made deliberately:

- **A guard is the whole input side.** `row_inputs :: [[FEELexp]]`, every input column conjoined.
  One matching column is not a duplicate. The output side is irrelevant — two rows with the same
  guard have a dead row whether their outputs agree or not.
- **Parsed values, not source text.** `1.1` and `1.10` collide because `Scientific`'s `Eq` compares
  on value and ignores scale, so the parser has already built the same `FNullary (VN 1.1)` for
  both; they are not even distinguishable by `show`. A source-text comparison would have missed the
  recorded symptom outright.
- **A multi-value cell is a set.** `fEvals` is `or . map (fEval arg)`, so `Fall, Winter` and
  `Winter, Fall` select exactly the same rows. Cells are compared by mutual containment rather than
  list `Eq`, so the static check agrees with the runtime matcher. Nothing in the tree exercises
  this either way — measured — so it is decided on the semantics, not on a fixture.
- **Zero input columns decline rather than refuse.** Every guard would be the empty conjunction and
  every pair vacuously identical. Unreachable from markdown (`reviseInOut` guarantees an input
  column) but legal DMN, and the one false-refusal trap the surveys found.

It is **sound** against the runtime, not merely agreeing with it by luck: `fEval` dispatches on
constructor structure alone, so equal guards imply identical matching behaviour for every input.
A static refusal here can never contradict `evalTable`'s `HP_Unique` arm.

**Blast radius, measured before and after by byte comparison**: every fixture at `603676f`
(`README.md` plus every `.md`/`.dmn` under `test/`) run through the six real output formats —
`ts js py l4 xml md` — stdout + stderr + exit each, under a `603676f` binary and this branch's.
**1,500 invocations, 1,495 byte-identical.** All five differences are the one symptom fixture that
is *supposed* to newly refuse, in the five formats that emit; `md` is unimplemented and errors
identically under both. Exactly one table newly refuses and it is that fixture. Corpus 210 cases,
0 policy regressions; `make roundtrip` 0 FAIL.

> **A retracted number, kept visible because the retraction is the useful part.** This paragraph
> first read "all six output formats, 1560 invocations". That was wrong twice over: the capture
> script looped `ts js py l4 json xml`, and **there is no `json` backend** — all 260 of those runs
> were `option --to: Accepted file types are…` at exit 1, carrying no information — while `md`, a
> real `FileFormat`, was never run at all. Five real formats, not six. The conclusion was right and
> the evidence for it was one sixth weaker than advertised, which is exactly the shape `~/CLAUDE.md`
> rule 2 warns about: a confident number forecloses the checking that would have caught it. Caught
> by a verify lens reading the script rather than the summary. The figures above are a re-measure
> with the format list corrected.

#### What was deliberately left, and where the prior art is

**The scope was Meng's call: exact duplication, `U` tables only.**

- **`U` only.** `A` legitimately permits overlapping rows that agree (D-5 made that work) and `P`,
  `O`, `R` and `Collect` order or accumulate deliberately. `policy/struct-outputorder-enum-honoured`
  is an `O` table with two all-wildcard rows whose outputs are *both live*: applying this check to
  `O` would be a false refusal on a policy recording.
- **No interval reasoning.** `[1..5]` versus `[3..8]` overlap and are not refused; nor is
  `<= 20` versus `>= 10`, which is `symptom/l4-hitpolicy-unique-silently-first` and stays a
  symptom. The check is an under-approximation: it never refuses a table that is fine, and it does
  not claim to catch every table that is not.

**Full overlap analysis is deferred, not forgotten.** The prior art is Calvanese, Dumas, Laurson,
Maggi, Montali & Teinemaa, *Semantics and Analysis of DMN Decision Tables*, BPM 2016, LNCS 9850
(doi 10.1007/978-3-319-45348-4_13, arXiv:1603.07466). Per its abstract it gives a formal semantics
for DMN tables, a formal definition of the key analysis tasks, and scalable algorithms for **two**
of them — detection of **overlapping rules** and detection of **missing rules** — via a geometric
interpretation of the table, implemented in an open-source DMN editor. (Stated no more strongly
than that: the abstract does not claim masking or subsumption algorithms, and this entry should not
either.)

One thing a future implementer should know before starting, and it is the reason the deferral has
content rather than being a shrug. A pre-implementation survey ran a *partial* interval-overlap
approximation over the whole tree and reported 55 non-identical overlapping pairs across 36 of the
141 `U` tables, of which about forty involve an all-wildcard catch-all row. Those figures are that
survey's and are **not** re-derived here — treat them as an order of magnitude, not a count. What
does not depend on them, and is checked directly: `symptom/l4-hitpolicy-unique-silently-first`
already names four `policy/l4-*` recordings that are `U` tables ending in a catch-all row
(`md-backend-l4`, `l4-sumtype-emitted`, `md-l4-ditto-wide-chars`,
`l4-keyword-column-names-quoted`). So the wider check is not a drop-in: it needs its own ruling
first — *is a trailing catch-all in a `U` table an authoring error, or an accepted idiom?* That
question is the real content of the deferral, and it is a genuinely separate decision from this
one.

**Correction to this entry, made while landing it.** It cited "DMN 1.3 §8.2.11" for the uniqueness
requirement. That is wrong: §8.2.11 is *Default output values*; hit policy is **§8.2.10**, which
carries "The hit policy SHALL default to Unique" and "Decision tables with the Unique hit policy
SHALL NOT contain overlapping rules." Verified against the OMG PDF (formal/2021-01-01). The same
wrong cite had a second copy in `DMN-CORE-HACKAGE-FINDINGS.md` and was corrected there in the same
commit; the diagnostic itself cites §8.2.10 and always did.

### D-14 — delete `DMN.SFeelGrammar`; `DMN.ParseCell` is the grammar. **RULED: delete. LANDED.**

Supersedes D-10. The choice was adopt / harvest / delete, argued from a measured divergence list
rather than from D-10's authority.

**Adopt is disqualified twice over.** Behaviourally, it is a breaking change to the markdown
surface on a scale D-2 ruled out without a deprecation path: in input position, 33 of the 36 cells
dmnmd accepts today are refused by `simpleUnaryTests`, including every bare number, every interval,
`<= 8`, `= 5`, the suffix form, `not(…)` and the `-` wildcard, and the divergence list is
**one-directional** — there is no input-position cell SFeelGrammar accepts and dmnmd refuses. In
output position the 12 cells it accepts and dmnmd refuses are all one artefact, `unescapedLiteral =
some alphaNumChar` swallowing a bare word, so `Infinity` and `NaN` become `StringLiteral`s in a
`Number` column. Structurally, `UnaryTest` has no negation constructor: rule 12.b's arm *discards*
the `not`, so adopting the vocabulary would reinstate by construction the pre-D-9 defect — the
unanchored search matching `[1..5]` inside `not([1..5])` and throwing the `not` away — that
`c21fedf` fixed.

**Harvest yields nothing that is this module's to give.** The three candidates were measured and
all three fail on inspection of where the capability actually lives:

- *Operator precedence and chaining.* Real gap (see below), but it is `makeExprParser` from
  `Control.Monad.Combinators.Expr` doing the work, plus a 7-line `table`. And it does not survive
  contact with a real cell: `expression` refuses `Age * 2` and `Units / 2`, because `name` cannot
  read a multi-letter identifier. What would be ported is a library call, not a transcription.
- *Non-breaking-space tolerance.* Real gap, and it matters for a tool whose ingestion path is a
  paste out of Word. But it is `spaceConsumer = space`, i.e. megaparsec's `isSpace`; the fix in
  `ParseCell` is one character class, not a port.
- *String escape sequences, rules 29 and 38.* The only thing in the module with no counterpart in
  `ParseCell` — and it **validates rather than decodes** (`stringEscapeSequence` on `\t` yields the
  two characters `\` `t`), and its `hexDigit` inherits the `inClass` bug, accepting only
  `0 9 a f A F`. The existing tests pass because they happen to use `ꪪ` and `ꪪ`.

So: nothing in `DMN.SFeelGrammar` is simultaneously reachable, correct, and absent from
`DMN.ParseCell`. 396 lines of module and 53 of test go.

**The price, and how it is paid.** `test/SFeelGrammar.hs` was the only place in the repo naming two
real gaps in the live path, neither of which any corpus case or round-trip fixture covered. Deleting
it silently would delete the evidence they exist. Both are therefore **rehomed as `symptom/`
recordings against the live binary**, which is strictly better than an hspec assertion about a
module nothing imports. Measured through the built binary at `603676f` and recorded *before* the
deletion, so the evidence never lapsed:

- `symptom/md-nbsp-refused-input` and `symptom/md-nbsp-refused-output` — `1<NBSP>+<NBSP>2` is refused in an output cell and `<<NBSP>5` in an
  input cell, both with a loud located diagnostic that renders the codepoint as `\160`. Honest, but
  DMN 1.3 §9.2 rule 36 lists ` ` as white space, so this is a conformance gap and not a design
  choice.
- `symptom/md-string-escapes-uninterpreted` — a cell written `"a\tb"` emits TypeScript
  `S === "a\\tb"`, backslash-t and not a tab, at exit 0 with nothing on stderr. Self-consistent
  (dmnmd re-escapes what it read literally) but a silent divergence from rules 29 and 38.

The fourth thing `test/SFeelGrammar.hs` asserted — `.5`, and `1+2` as arithmetic — is already live
and already pinned, by `policy/num-leading-dot-accepted` and the arithmetic policy cases. Nothing is
lost there.

**Landed, and one thing the entry did not anticipate.** `ParsingUtils.inClass` — the
`(\`elem\` cs)` that made every character class in the deleted module wrong — turns out to have had
exactly one consumer that passed range notation, and it was `DMN.SFeelGrammar`. With the module
gone, the two remaining call sites both pass enumerated classes (`"UAPFOR"`, `"#<>+A"`) and are
correct, so deletion did not merely remove a broken user of the trap: **it emptied the trap.** The
function now carries a haddock saying so, because the next person to write `inClass "0-9"` will
otherwise re-set it in silence.

**Measured.** 448 lines removed (396 module, 52 test), plus one `exposed-modules` line, one
`other-modules` line, and two lines in `test/Spec.hs`. `cabal test` 244 → **228** examples, all
passing: the 16 lost are `test/SFeelGrammar.hs`'s own (11 escape, 4 arithmetic, 1 numeric), 3 of
which are rehomed above and 2 of which were already pinned. `make corpus` 213/213 unchanged, 0
policy regressions. `make roundtrip` 127 pass / 0 FAIL / 10 xfail. Not one corpus recording, golden
file, or round-trip fixture moved — which is what "imported by nothing but its own test" predicts,
and is the only part of D-10 that survived measurement.

Confirmed independently of the gates, by building the binary at `603676f` in a second worktree and
diffing it against this one over **271 fixtures × 6 output modes = 1,626 pairs**, stdout, stderr
and exit status together: **five differ, and all five are `DECISIONS.md` itself**, which is a
prose document dmnmd reads as a fixture. The whole difference is one extra `note: … skipping the
pipe table` line, emitted for the three-row table in D-15 that this session added. Zero code
behaviour moved.

**What this does not fix, and must not be lost with the module.** `ParseFEEL.parseFNumFunction` is
flat: `Age * 2 + 1` is refused in **both** positions, and only `(Age * 2) + 1` works. In an output
cell the refusal reads "row 1 \"Age \* 2 + 1\" reads as String", naming neither the limitation nor
the repair; in a declared-`Number` input cell `notATestMsg` offers "an arithmetic expression" as an
acceptable form while refusing one. That is a separate change with a real blast radius —
`parseFNumFunction` is shared with the XML reader and every backend renders `FNF3` positionally, so
precedence would newly matter to `showFeel` — and it does not ride along with a deletion. It is
D-15.

### D-15 — `Age * 2 + 1` is refused, and both diagnostics misdescribe why. **RULED: open.**

Split out of D-14 so that deleting `DMN.SFeelGrammar` does not delete the only record of the one
real capability gap the module pointed at. Measured through the built binary at `603676f`, in a
declared-`Number` column, in both positions:

| cell | position | today |
|---|---|---|
| `Age * 2 + 1` | output | refused; the message says the cell "is not … an arithmetic expression" |
| `Age * 2 + 1` | input | refused by `notATestMsg`, which offers "an arithmetic expression" as an acceptable form |
| `(Age * 2) + 1` | output | **accepted**, emits `((Age * 2.0) + 1.0)`, exit 0 |

**Re-measured at `7e01caa`; four corrections to the paragraphs below, all from independent
surveys.** Rows 1 and 3 hold. Row 2's *attribution* was wrong in a way that matters: both
positions reach the **same** `notATestMsg` (`ParseCell.hs:416`, raised at `DecisionTable.hs:243`),
because a chained cell never parses and so no `FFunction` exists for `inputArithErrs` to match.
`inputArithErrs` (`DecisionTable.hs:886`, raised at `:572`) is reached only by the *parenthesised*
form in an input cell. So the defect is symmetric — one message, both positions — and a fix confined
to `inputArithErrs` would not touch a single `Age * 2 + 1` author.

1. **The closing `: String` advice is a worse defect than the one this section was opened for,
   and both messages carry it.** D-15 says the messages fail to print the working repair. They also
   print a repair that is *accepted and wrong*: declaring the column `String` makes `--to=js` emit
   `return {"Result":"Age * 2 + 1"};` at exit 0 — the formula as a literal, never computed — and in
   an input cell emits `if (Age === "(Age * 2) + 1")`, a string test that can never fire. Both
   measured under `node`. A refusal that hands the author a silent wrong answer is strictly worse
   than one that hands them nothing, which is the rule `CLAUDE.md` states as governing.

2. **`showFNumFunction` misquotes the author's own cell, today, with no patch applied.** It renders
   `FNF3` flat, so `inputArithErrs` on a source cell of `(Age + 1) * (Age + 2)` reports
   `the input cell reads "Age + 1 * Age + 2"` — text the author did not write, that dmnmd itself
   refuses, and that is a different number. `DMN.Translate.XML.showArith` solved exactly this for
   the XML writer and its haddock names `showFNumFunction` as the copy that did not adopt it.

3. **"every backend renders `FNF3` positionally" is false.** All four *emitting* backends already
   parenthesise: `FEELhelpers.showFeel` (serving ts/js/py) and `L4.fnf2l4` fully, `XML.showArith` at
   nested operands. The only flat renderer is `showFNumFunction`, which produces **diagnostics**.
   The blast radius the paragraph below assigns to the expensive half is therefore empty, and the
   real one — type inference — it does not mention. Relatedly, the **L4 ditto grid cannot be moved
   by this at all**: `inputArithErrs` refuses every `FFunction` in an input position, so no
   arithmetic reaches a guard cell, and the output-side arithmetic sits in `armResult`, outside the
   width computation.

4. **`Age * (-1)` is refused**, and so is `Age * -1` and `-Age`, because `parseFNF0` uses
   megaparsec's *unsigned* `scientific`. A signed literal is legal as a whole cell (`-5`, which
   `notATestMsg` advertises) and nowhere inside arithmetic. That is a second instance of this
   section's own defect hiding in the sentence meant to be the message's most reliable part: the
   working negations are `0 - Age` and `Age * (0 - 1)`, and no message mentions either.

`DMN.ParseFEEL`'s `FNF3` is a single flat binary application whose operands are an atom or a
parenthesised sub-expression, so there is no operator chaining and no precedence. The parenthesised
form is the working repair and **neither message prints it** — which is the failure `~/CLAUDE.md`
and the corpus README both name: a diagnostic that recommends a repair it will itself refuse.

**Two changes, and they are separable.** The cheap half is the messages: name the chaining
limitation and print `(Age * 2) + 1`, verified by running it. That moves the ~10 corpus recordings
that quote the rule-31 message verbatim, each of which needs the ordinary policy-re-record
justification. The expensive half is `parseFNumFunction` itself — `makeExprParser` would give
precedence for one 7-line operator table, but `FNF3` is shared with the XML reader and every
backend renders it **positionally**, so precedence would newly matter to `showFeel` and the L4
ditto grid, and `--to=xml` round-trip fidelity would have to be re-measured across all 176
fixtures. Neither rode along with D-14.

**Not to be confused with two adjacent exit-0 wrong answers** found by *running* the emitted
JavaScript under `node`. Those belong in `cases/symptom/` rather than here, because they are bugs
and not decisions, and they are not refusals at all — `Non-Participating` in a `Number` output
column emits `(Non - Participating)` and `n/a` emits `(false / a)`, both throwing `ReferenceError`;
`< 5` and `[1..5]` in the same position emit an arrow function as the output *value*, which
`JSON.stringify` drops, so the field silently vanishes.
