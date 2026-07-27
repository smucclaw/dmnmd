# BUILD SPEC — L4 sum types for domained columns, and the DMN data model

> ## Status: **PROPOSED — not landed. No code written for either half.**
>
> Produced 2026-07-27 against trunk `3d880d4`, answering `BUILD-SPEC-dmnmd-e4.md` §12 q2 and q3.
> Everything in the present tense describes the tree **as it is today**; everything proposed is
> written as "would". A sentence reading as though this shipped is a bug in this document.
>
> **Every L4 fragment here was run through the real `l4`** at `/Users/mengwong/.local/bin/l4`.
> Claims carry their transcripts. Two independent designs were produced and each was attacked by
> a reviewer who built a working prototype in-tree, measured it, and reverted. **Both designs had
> a landing blocker found only by that attack** — §A.7 and §B.6. Neither would have survived
> first contact without it.
>
> **What would make it true:** the commit sequences in §A.8 and §B.5, each ending with
> `make corpus` green and `cabal test` green.

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
| **2** | `isCollection` → `DMN_List` | small, once tier 1 exists |
| **3** | `itemComponent`, structured types | **out of scope**; recursive, and warns as unusable in tier 1 |

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
