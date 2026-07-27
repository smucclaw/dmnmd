# BUILD SPEC — L4 sum types for domained columns, and the DMN data model

> ## Status: **LANDED.** Per step, as of 2026-07-28:
>
> | step | what | state |
> |---|---|---|
> | A.8 1 | `Diagnostic` into its own module | **landed** — `43b57ff` |
> | A.8 2 | `toL4File` + `outputToAll` | **landed** — PR #33. Went further than "behaviour-preserving": refusals became `Diagnostic`s decided for the whole file *before* rendering, because the old per-table emission leaked 4KB of a refused run to stdout above the 2048-char buffer chunk |
> | A.8 3 | `DECLARE` + backticked constructors, outputs only | **landed** — `d882a43` |
> | A.8 4 | `MAYBE`/`JUST`/`NOTHING`, subsuming `wrapMaybe` | **landed** — `2d3acbb` |
> | A.8 5 | input columns | **landed** — PR #37. Read the `MEASURED` blocks in §A.4 and §A.7 first: both sections were refuted by measurement, and the shipped design is not the one written below |
> | A.8 6 | file-level dedup + the avoid-set | **landed** — dedup in PR #35 (`assignEnumNames`), the avoid-set with step 5 in PR #37 (`renameParams`) |
> | B.5 0 | a Warning naming each discarded `<itemDefinition>` | **landed** — `0f06b77` |
> | B.5 1 | `allowedValues` as an inherited named domain | **landed** — PR #34 |
> | B.5 2 | `isCollection` → `DMN_List` | **landed** — but NOT as one line. §B.3's "small" verdict was wrong, and the `MEASURED 2026-07-27` block there explains why; the `LANDED 2026-07-28` block below it records what was actually built |
> | B.5 3 | structured `itemComponent` | **out of scope**, deliberately |
>
> **Read the body as the design's voice, not as a description of the tree.** It was written
> before any of this existed, so its present tense means "as of `3d880d4`" and its "would" means
> "as proposed" — including for the steps that have since shipped. Where a shipped step diverged
> from what is written here, `L4.hs`'s own comments carry the correction and are authoritative;
> §A.7's `HP_Priority` note is the one place the divergence is material.
>
> This header itself was wrong for a day. It read *"PROPOSED — not landed. No code written for
> either half"* while six of the ten steps above had shipped, the first of them **the same day
> the header was written**. That is `CLAUDE.md`'s "never write a planned state in the present
> tense" failing in the repo that wrote the rule — and the mirror image of the drift this spec's
> own §B.4 is about. The fix that generalises is the table: a status expressed per step against a
> commit cannot rot into a single sentence that is wrong about all of them at once.
>
> Produced 2026-07-27 against trunk `3d880d4`, answering `BUILD-SPEC-dmnmd-e4.md` §12 q2 and q3.
>
> **Every L4 fragment here was run through the real `l4`.** Claims carry their transcripts. Two
> independent designs were produced and each was attacked by a reviewer who built a working
> prototype in-tree, measured it, and reverted. §B.4's `hasName`-vs-`localPart` blocker was real
> and is what tier 1 was built around.
>
> **§A.4 and §A.7 have since been REFUTED by direct measurement** — 24 probe agents against the
> real `l4`, recorded 2026-07-27. Both sections now carry `MEASURED` blocks. Two of the corrections
> matter more than anything else in this document:
>
> - l4 resolves a name collision by the **expected type at the use site**, not by arity. The
>   "l4 disambiguates by arity" claim is false in both directions.
> - a `GIVEN` parameter spelled like a constructor **silently shadows it**, and the claimed
>   protection ("l4 catches it when the types differ") does not exist in guard position. The
>   emitted guard becomes a tautology, `l4 check` exits 0, and every input matches the first arm.
>   This is the whole risk of step 5 and it is invisible to the typechecker.

---

## 0. Why both halves are in one document

Meng asked for the sum type; the DMN data model came up in the same breath — *"doesn't the DMN
format include a data model section where user defined types are given?"* It does, and the two
compose: an `<itemDefinition>` with `<allowedValues>` is a **named** enum type, which is exactly
what an L4 `DECLARE … IS ONE OF` wants to be called. Build A alone and the L4 type is named after
a column. Build both and it can be named after the type the author declared.

They are still separately landable, in either order.

---

# Part A — emit a real L4 sum type

## A.1 What changes

A column with a declared domain emits `STRING`, and the domain is enforced only on dmnmd's side.
L4 has a sum type, so the constraint can live in the emitted L4 and be enforced by l4's own
typechecker.

```
                                    today                    proposed
GIVEN    cat IS A STRING                                     cat IS A Cat
GIVETH   A STRING                                            A MAYBE Card
arm      IF cat EQUALS "Dining"                              IF cat EQUALS `Dining`
result   THEN "PRVI"                                         THEN JUST `PRVI`
fallback OTHERWISE ""                                        OTHERWISE NOTHING
```

## A.2 Candidate columns

Exactly: `vartype == Just DMN_String`, `enums == Just ms` with `ms` non-empty, every member
`FNullary (VS _)`.

The boundary is airtight rather than merely convenient, and the reviewer confirmed it
independently: `DecisionTable.hs:164` gives a `Just DMN_String` column only `FNullary (VS _)`
cells, and `:153-155` only `FAnything`. **No `FSection`/`FInRange`/`FFunction` can appear on a
candidate column**, so "what does `< 18` become under an enum" cannot arise.

Untyped markdown columns are in scope — inference settles them to `Just DMN_String`.

Numeric domains are excluded **by l4, not by choice**: `DECLARE Bucket IS ONE OF 1, 2, 3` →
`CHECK=1`, `unexpected 1 / expecting … identifier`.

## A.3 Constructors are always backticked — and this is what saves the ditto grid

`ctor s = "`" ++ s ++ "`"`, unconditionally, never `quoteVar`.

**The load-bearing evidence.** `"Dining"` and `` `Dining` `` are both 8 display columns; `"kids"`
and `` `kids` `` are both 6. Bare `kids` is 4 and *would* shift the grid. The reviewer emitted
today's arms with the real binary, took the enum arms, mapped `` ` `` → `"`, stripped `JUST `,
and diffed:

```
$ diff str_arms.txt enum_arms.txt
IDENTICAL — backticking preserves the ditto grid byte-for-byte
```

Repeated on a two-table file: also identical. This matters because `renderDittoGrid` resolves `^`
by **absolute source column**, and a one-column drift makes a caret silently copy the wrong token.

Backticking also handles what I had guessed would be the hard part — `README.md` Example 3
declares `LEVEL 2, LEVEL 1, NONE`, and `` DECLARE `Review level` IS ONE OF `LEVEL 2`, … `` →
`CHECK=0`, 6 assertions satisfied. Members containing `"` and `'` also work
(`` `Say "hi"` ``, `` `Woman's World` `` — despite `'s` being l4's field-access operator).

## A.4 Name capture — the hazard neither I nor the design's own probing found first

**Backticking does NOT prevent capture.** A constructor and a same-typed parameter with the same
spelling silently resolve to the parameter:

```
DECLARE Cat IS ONE OF `cat`, `dog`
GIVEN cat IS A Cat
     IF cat EQUALS `cat` THEN 1
     OTHERWISE 2

l4 check          -> exit 0            <-- invisible to the typechecker
#EVAL f `dog`     -> 1                 <-- 2 is correct
#ASSERT …EQUALS 2 -> assertion failed
```

When the types differ l4 *does* catch it. Same-type capture is silent — and same-type is exactly
what dmnmd would create.

**Fix: rename the parameter, not the constructor.** The constructor is the lawyer's word; the
parameter is a binder we invented. Append `_` until unique. The avoid-set must be **file-wide**
(l4 scoping is top-level and order-independent) and compared on *emitted tokens*, so a column
named `Cold Storage` and a member `Cold Storage` collide — both render `` `Cold Storage` ``.

Confirmed **not** needed in the avoid-set: record `fieldName`s (separate namespace — verified),
and function names *when arities differ*. See §A.7 for when that last one is false.

> ### MEASURED 2026-07-27 — two claims above are wrong, and the fix is under-specified
>
> **1. "When the types differ l4 *does* catch it" is REFUTED in guard position.** `NUMBER`,
> `STRING` and an unrelated sum type all typecheck clean and give the wrong answer. After capture
> *both operands are the same variable*, so there is nothing left for the typechecker to disagree
> about. l4 only objects when the captured name lands in a **type-forced output slot** — which is
> not the shape a guard has. So the mitigation this section leans on does not exist:
>
> ```
> DECLARE Route IS ONE OF `Route`, `Dining`
> GIVEN Route IS A Route
> GIVETH A STRING
> Categorize Route MEANS
>   BRANCH
>     IF Route EQUALS `Route` THEN "matched Route"
>     OTHERWISE "no match"
>
> #EVAL Categorize `Dining`   ->  "matched Route"      <-- wrong; `Dining` is not `Route`
> l4 check                    ->  exit 0, "Check succeeded."
> ```
>
> The guard is a **tautology**: both sides resolve to the parameter, so arm 1 fires for every
> input. No warning, no ambiguity error, no diagnostic of any kind. Proven directly by writing a
> function whose entire body is the literal `` `Route` `` and watching it return its argument.
>
> **2. "Append `_` until unique" is not enough as literally written.** A domain member named
> `cat_` re-creates the capture, and `cat__` defeats two underscores. The emitter needs a
> **freshness loop against the constructor set**, not a fixed suffix. Members are the lawyer's
> words and can be anything.
>
> Still true: record `fieldName`s are a separate namespace and need no mangling; the underscore
> rename does work once it is fresh, bare or backticked, including inside multi-word names.
> Newly measured: `WHERE`- and `LET`-bound names capture identically and silently, and a
> parenthesised `OR` chain behaves no differently. Adjacent collisions that are at least *loud*
> (exit 1): a duplicate parameter, and a parameter spelled like a function.

## A.5 The `OTHERWISE` problem → `MAYBE T` / `NOTHING`

There is no natural zero, and `OTHERWISE` cannot be omitted. Measured:

```
BRANCH with no OTHERWISE                     -> parser error
OTHERWISE ""  against a sum-typed GIVETH     -> check error   <-- today's emission
GIVETH A MAYBE Card + OTHERWISE NOTHING      -> Check succeeded
```

Three alternatives were considered and rejected:

- **First constructor as fallback** — fabricates a domain member for uncovered input. Forbidden
  by `CLAUDE.md`.
- **A sentinel added to the enum** — *disqualified with a transcript.* Put the sentinel in a
  domain that is table A's output and table B's input (the miles-card `Category` shape) and
  `l4 check` passes while `#EVAL CardToUse \`NO RULE MATCHED\`` succeeds. dmnmd would have
  silently widened **a different table's declared input domain** — reintroducing, inside the
  emitted L4, the exact defect `domainErrors` exists to refuse.
- **A partial `CONSIDER`** — honest but leaves a worse diagnostic.

Members named `NOTHING`, `JUST` or `TRUE` do not shadow the builtins (verified).

## A.6 The payoff: one shared type across tables

Two tables sharing a domain emit **one** `DECLARE`, so `CardToUse (Categorize "foodpanda")`
typechecks with the two sides related rather than being unrelated `STRING`s. Verified end to end:
`CHECK=0`, `JUST OF PRVI` / `NOTHING`, 2/2 assertions satisfied.

That requires a **file-level** entry point, because two `DECLARE Category` in one file is
`check=1`, *multiple definitions for the identifier*:

```haskell
toL4File :: L4Opts -> [DecisionTable] -> ([Diagnostic], String)
toL4     :: L4Opts -> DecisionTable  -> String   -- kept; TranslateL4Spec calls it in 14 places
```

`Diagnostic`/`Severity`/`renderDiagnostic`/`isError` currently live in
`src/DMN/XML/XmlToDmnmd.hs:42-67`. A transpiler importing the XML reader is backwards — move them
to their own module and re-export.

> ### MEASURED 2026-07-27 — the payoff is real, and three things about it were not known
>
> The shared-type composition works end to end: one `DECLARE Category` serving `Categorize`'s
> `GIVETH` and `CardToUse`'s `GIVEN` typechecks clean, every assertion passes, and the ditto `^`
> copies a typed constructor without incident.
>
> **The `MAYBE` mismatch is real and loud.** `Categorize` returns `MAYBE Category` while
> `CardToUse` wants `Category`, so the composition is exit 1. The shortest fix is **zero glue**
> when the upstream table has a catch-all row — its `GIVETH` is then plain `Category` and the two
> compose directly. Otherwise it needs a two-arm `CONSIDER`, which must be multi-line and needs no
> prelude.
>
> **`CONSIDER` is not exhaustiveness-checked.** A missing arm passes `l4 check` with zero
> diagnostics and fails at *runtime* — with **exit 0**. So if dmnmd ever emits a `CONSIDER`, l4's
> typechecker is not a safety net for it and dmnmd must guarantee totality itself.
>
> **`EQUALS` is the only comparison l4 permits on a sum-typed value** — every ordering operator is
> exit 1. That matches §A.2's claim that no `FSection`/`FInRange` can reach a candidate column, but
> now from l4's side as well as dmnmd's.
>
> **Typing inputs is a strict gain beyond composition.** It converts `EQUALS "Dining"` — a string
> literal tested against a domained column — from silently compiling into a compile error. Today
> that mistake is invisible.

## A.7 What the adversarial review broke

**BLOCKER — zero-input tables.** The design claimed *"l4 disambiguates by arity"*. It disambiguates
**only when the arities differ**. A table with zero input columns emits a zero-arity function, and
a same-named constructor of the same type is then ambiguous. Reachable from plain markdown, and
it *typechecks today*:

```
| U | card (out)  |
|   | Route, PRVI |
| 1 | Route       |          table named `Route`, member named `Route`

today          -> CHECK=0
design A       -> CHECK=1  multiple definitions for the identifier Route
```

Fix: the avoid-set must include the **table/function name**, unconditionally — mangling a type is
cheaper than reasoning about arity. Note `isCatchAll` is *vacuously true* on a zero-input row
(`all (all (== FAnything)) []`, `L4.hs:126`), so such a table gets no `MAYBE` to disambiguate it
either.

This is exactly the "sharpened past its evidence" failure the repo keeps hitting: a true claim
("l4 disambiguates by arity") narrowed into a false one by dropping its precondition.

> ### MEASURED 2026-07-27 — the mechanism is wrong, and so is the fix
>
> **l4 does not disambiguate by arity at all.** It disambiguates by the **expected type at the
> use site**. The paragraph above is wrong in both directions:
>
> - *Differing arities do not save you.* An arity-1 function `Route` and a nullary constructor
>   `Route` collide fatally in any position that supplies no expected type — a bare `#EVAL Route`
>   gives `multiple definitions for the identifier Route`, exit 1. The message lists the candidates
>   **by type**, which is l4 telling you what the rule actually is.
> - *Equal arities are not fatal.* Two arity-0 definitions coexist happily when their **types**
>   differ — and `GIVETH A MAYBE Category` against a constructor of type `Category` is exactly that
>   pair. So the zero-input table this section calls a BLOCKER **is not one in the shape dmnmd
>   actually emits**. It is ambiguous only when the arity-0 function's return type *equals* the
>   constructor's type.
>
> **And mangling the type name does not fix it.** The ambiguity is on the *constructor*
> identifier, not the type identifier; renaming `Card` changes nothing about two `Route`s. The
> section's own recommendation would have left the bug in place.
>
> **Backtick quoting is purely lexical and disambiguates nothing.** All four
> DECLARE-site/use-site quoting combinations behave identically. Backticks buy the ditto grid
> (§A.3) and non-identifier members — not scope.
>
> So the real hazard for step 5 is **not** the zero-input table. It is §A.4's silent parameter
> shadowing, which this section does not mention and which no amount of type mangling touches.
> Only renaming the parameter, renaming the constructor, or declining to emit the sum type fixes
> it. Since a constructor is the lawyer's word and a table name is the public API, the honest
> option when a constructor collides with the **function** name is to leave that column a `STRING`
> and warn — degrading to exactly today's behaviour rather than emitting something that passes
> `l4 check` and ambushes the caller later. (Duplicate declarations with no use site do pass at
> exit 0; l4 complains where the name is *used*.)

**`needsMaybe` must subsume the existing `wrapMaybe`, not stack with it.** `L4Opts.wrapMaybe`
(`L4.hs:30`, consumed at `:79`, `:334-335`, `:341-342`) does the same job. Composed, they emit
`MAYBE MAYBE T` → `CHECK=1`, *arities do not match*. Latent today only because `Main.hs:186`
always passes `defaultL4Opts`.

**`HP_Priority` would always get `MAYBE`, even when total.** `L4.hs:94-95` hardwires
`catchAll = Nothing` for Priority, so `needsMaybe` fires unconditionally — but
`policy/l4-priority-reorders-arms` shows the all-wildcard row became a vacuously-true *arm*
(`IF TRUE THEN "Stew"`), so the table is total and the `OTHERWISE` is dead. Test
`any isCatchAll armRows` too.

**`feelValL4` (`L4.hs:227-232`) is missing from the threading list**, as is `typeDefaultL4`
(`:378-383`). Reached under `useElem`.

**Separately, and pre-existing:** `useElem` output does not typecheck at all today — `elem` is
not a defined identifier in l4 (`CHECK=1`). That deserves its own `symptom` case and is not
caused by this work.

**Multi-table + a refusing table is unprotected.** Today a good table's output reaches stdout
*before* a later table's `error` fires (exit 1 with partial stdout). `toL4File` makes that a
function of Haskell strictness. Both existing refusal cases are single-table, so neither can
catch it. A multi-table-with-refusal case is required.

## A.8 Commit order

1. Move `Diagnostic` out of `XmlToDmnmd` into its own module. Pure refactor.
2. `toL4File` + `outputToAll`, still emitting exactly today's output. Behaviour-preserving; add
   the multi-table-with-refusal case here, **before** it can regress.
3. Constructor backticking and `DECLARE` emission for outputs only, catch-all tables only (no
   `MAYBE` yet).
4. `MAYBE`/`JUST`/`NOTHING` for tables needing a synthesized `OTHERWISE`, subsuming `wrapMaybe`.
5. Input columns.
6. File-level dedup and the avoid-set (including the table name, per §A.7).

---

# Part B — stop silently discarding `<itemDefinition>`

## B.1 The defect

`xsd/DMN13.xsd:50` puts `itemDefinition*` on `<definitions>`; `tItemDefinition` carries
`allowedValues` (`tUnaryTests` — an enum domain, ~238), recursive `itemComponent*` (~240),
`typeLanguage` (~243) and `isCollection` (~244).

`ParseDMN.hs:852` is `xpIgnoredElems "itemDefinition"`, and `xpIgnoredElems` returns `()`. **The
whole DMN data model is discarded without a word** — the rule `XmlToDmnmd`'s own module header
exists to enforce, broken one module upstream.

## B.2 The premise the design corrected

The brief framed this as binary: keep today's total permissiveness, or model it and make every
unmodelled attribute a hard document-level rejection. **There is a third option**, compiled and
run: `xpFilterAttr`/`xpFilterCont` take an **arrow**, not a boolean. Passing a *name filter*
deletes everything except the handful of children and attributes modelled, so `id`, `label`,
`typeLanguage`, `<description>` and foreign attributes are dropped exactly as today.

Strictness increases in **one** place: inside `<allowedValues>`, where it becomes identical to
what `<inputValues>` has imposed since E4 — both are `tUnaryTests`.

## B.3 Tiers

| tier | what | verdict |
|---|---|---|
| **0** | a discarded `<itemDefinition>` emits a Warning naming it | land first; needs only a two-line "peek", not the full pickler |
| **1** | `allowedValues` on a top-level itemDefinition becomes a named domain a column's `typeRef` inherits | the direct answer to Meng's question; composes with Part A |
| **2** | `isCollection` → `DMN_List` | **blocked — see below.** The "small" verdict was wrong |
| **3** | `itemComponent`, structured types | **out of scope**; recursive, and warns as unusable in tier 1 |

> ### MEASURED 2026-07-27 — tier 2 is not small, and doing it would make things worse
>
> `DMN_List` exists in `DMN.Types`, `ParseTable` accepts a `[Number]` column header, and all three
> backends have a `type2*` case for it. **The cell layer does not honour any of it.**
> `DecisionTable.hs:157` sends a list type straight to `baseType`, so every cell is read as a
> scalar while the emitted signature still says list:
>
> ```
> --to=l4   GIVEN tags IS A LIST OF NUMBER … IF tags EQUALS 5    ->  l4 check exit 1
> --to=ts   function Scores ( tags : number[] ) … tags === 5.0   ->  never true
> ```
>
> The L4 is caught by l4's typechecker. The TypeScript is worse: `tsc` would reject
> `number[] === number`, but dmnmd does not compile what it emits, so what ships is a comparison
> that is silently always false. Recorded as `symptom/md-list-column-cells-are-scalars`.
>
> So the one-line `isCollection → DMN_List` change would import that into the XML reader and
> **trade a loud refusal for a silent wrong answer** — the one trade this project does not make.
> Tier 1 therefore *refuses* an `isCollection` type, citing the case.
>
> Real tier 2 is: make a list column's cells be **list tests**. In DMN an `inputEntry` on a
> collection is a membership or quantified test, not an equality — so this belongs with the S-FEEL
> grammar work, not here.

> ### LANDED 2026-07-28 — what tier 2 turned out to be
>
> The block above is right that tier 2 is not one line, and wrong about where it belongs. It does
> **not** need S-FEEL grammar work: S-FEEL has no list type, no `in`, no function invocation and
> no quantifier, and `SFeelGrammar.hs` is not wired into the pipeline at all. What it needed was a
> decision about **meaning**, and then plumbing.
>
> **The meaning: a plain cell in a collection column is MEMBERSHIP, and every other shape is
> refused.** Not an existential lift, and not an explicit `some`/`every` keyword. Both were
> designed in full and both were rejected:
>
> * An existential lift has to pick ∃ over ∀ for `> 3` with nothing in the table to justify it,
>   and pays for the choice four times over — a hardcoded lambda binder that captures user
>   identifiers in three backends with no freshening, and Python silently substring-matching a
>   scalar string through `any(… for x in …)`.
> * A mandatory quantifier keyword cannot be met from the XML side at all: a DMN `<inputEntry>` is
>   `tUnaryTests`, one `<text>`, with no quantifier slot. Requiring the keyword in markdown would
>   make `--from=xml` able to accept nothing but `-`.
>
> Refusing the ambiguous shapes designs both problems out. There is no lambda to bind and no
> quantifier to guess, and `.includes` / ` in ` / `` `dmnmd list contains` `` are all simple calls.
>
> **Why refusing is not a cop-out here.** The cell being refused is not merely unimplemented; DMN
> gives it no meaning. §10.3.2.10 defines input-entry satisfaction by reduction to `FEEL(e in (t))`
> and Table 54 defines `<`/`>` over scalars, so a conformant engine yields null and never matches.
> And what it replaced was measurably worse than never-matching: `tags > 3` against a `number[]`
> emitted `if (tags > 3.0)`, and in node `[5] > 3` is **true**, `[10] > 3` is **true**, `[1,2] > 3`
> is **false** — JS stringifies the array and coerces. A test suite over single-element lists would
> have passed.
>
> **What shipped**, over seven commits:
>
> | | |
> |---|---|
> | `-Werror=incomplete-patterns` | it found a missing `FFunction` arm in `feel2pyIn` within minutes, and a missing `DMNVal` arm in code written later the same day |
> | `elemType`, replacing `baseType` | one level, no flattening, no `Nothing -> String` coercion |
> | `structuralErrors` | eight refusals + warnings, at TABLE level walking `allrows` — **not** in `mkFEither`, which cannot tell an input cell from an output cell from a sub-header domain member |
> | membership in four backends | `.includes` / ` in ` / a self-contained L4 helper, **no `IMPORT prelude`** |
> | `DMNVal`'s `VL`, and `mkInputValue` | a cell is a TEST, a runtime argument is a VALUE — conflating them is why `-q` used to accept `>= 5` as an input |
> | `isCollectionOf` in `resolveTypeRef` | the wrap happens after the recursive resolve, so a collection of a named base composes |
>
> Verified by execution, not by reading: the emitted JS in node, the emitted Python in python3, the
> emitted L4 through `l4 run`, and the `-q` interpreter, all agree on the same six inputs.
>
> The one thing the block above got exactly right is the warning that a one-line
> `isCollection → DMN_List` would trade a loud refusal for a silent wrong answer. That refusal is
> preserved, moved from the type down to the cell: R3 refuses a collection cell that is not a plain
> member, per rule id, so `list contains(?, "RED")` from a DMN file is still refused by name.

## B.4 The blocker the review found

`xpFilterCont (hasName "typeRef" <+> …)` is **wrong**. HXT's `hasName` compares the **qualified**
name, prefix included. Against `<semantic:typeRef>` it does not match, so the filter deletes the
`typeRef` and the `allowedValues`, and the itemDefinition parses as empty.

This is not exotic — **the DMN specification's own Chapter 11 example is prefix-qualified**, and
it is already checked into `test/examples/`. Worse, the resulting diagnostic is *false about the
document*: it says the itemDefinition "declares neither `<typeRef>`, `<itemComponent>` nor
`<functionItem>`" while the file has `<semantic:typeRef>string</semantic:typeRef>` on the next
line. That is the exact failure mode tier 1 exists to remove, reintroduced one level up.

Proven by fixing it — one substitution:

```haskell
. xpFilterCont (hasNameWith ((`elem` ["typeRef","allowedValues","itemComponent","functionItem"]) . localPart))
```

after which the same file emits correctly and the inherited domain is enforced.

**Every `hasName` in a new pickler must be `hasNameWith (… . localPart)`.** Audit the existing
ones too.

## B.5 Commit order

1. Tier 0 — the Warning. Smallest landable unit; makes the silence visible immediately.
2. Tier 1 — the filtered pickler (**with `localPart`**), the type environment, `typeRef`
   resolution, and the extended "unknown typeRef" diagnostic that says whether the name *was*
   declared and dropped.
3. Tier 2 — `isCollection`.

Fixtures: the DMN spec's Chapter 11 example, re-namespaced to 1.3, is the realistic test and is
already in the repo. A prefix-qualified fixture is **mandatory**, not optional — its absence is
what let B.4 through.

---

## C. Open questions

1. **Does the L4 sum type want the DMN type's name** (Part B tier 1) rather than the column's?
   Strictly better, and the reason both halves are in one document — but it makes A depend on B.
   **Recommendation: build A named after the column, and rename in a follow-up once B lands.**
2. **Should input columns get a sum type at all**, or only outputs? Inputs make the composition
   payoff (§A.6) work, but widen the blast radius. **Recommendation: outputs first, per §A.8.**
3. `useElem` emits L4 that does not typecheck today, independently of all of this. Record as a
   symptom now, fix separately.

   > **MEASURED 2026-07-27 — the diagnosis was wrong, and so is the remedy.** The claim repeated
   > in §A.7, "`elem` is not a defined identifier in l4", is false. `elem` **is** defined, in l4's
   > own prelude (`jl4-core/libraries/prelude.l4:448`), as `elem x list` over a `LIST OF a` —
   > exactly the shape dmnmd emits. What the emission omits is the `IMPORT prelude` line that
   > brings it into scope. Same measurement, opposite conclusion: not "l4 lacks this", but "we
   > forgot the import".
   >
   > This is the "sharpened past its evidence" pattern again — `CHECK=1 / could not find a
   > definition for elem` supports "unbound *in this file*", and was written up as a statement
   > about the language.
   >
   > Two things block a fix rather than a note. `useElem` is **not reachable from the CLI** — no
   > flag sets it, only `TranslateL4Spec` — so the behavioural corpus, which shells out to the
   > binary, cannot hold a case for it at all. And the fix could not be verified here: the
   > `prelude.l4` in the l4-ide checkout uses `@infixl`, which the installed `l4` binary's lexer
   > rejects, and `~/.local/share/jl4/libraries/` does not exist on this machine. So the emitter
   > change (`IMPORT prelude` whenever `useElem` is set) is **stated, not made**.
