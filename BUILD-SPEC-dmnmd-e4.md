# BUILD SPEC — E4: honour declared domains, and read quoted strings

> ## Status: **PROPOSED — not landed. No code has been written.**
>
> This is a design, produced 2026-07-27 against trunk `6edea2c`. Everything below in the
> present tense describes the tree **as it is today**; everything E4 would change is written
> as "would" or under an explicit *Change* heading. If you find a sentence that reads as
> though E4 has shipped, it is a bug in this document — see `CLAUDE.md`, "Never write a
> planned state in the present tense".
>
> **What would make it true:** the commit sequence in §10, ending with `make corpus` showing
> the four named cases moved and the remaining 123 unchanged, and `cabal test` green.
>
> **Provenance.** Three independent designs were produced from different angles, each attacked
> by two adversarial reviewers, and this synthesises them. Where a claim is load-bearing I
> re-ran it myself rather than trusting the agent that reported it; those are marked
> **[verified here]**. Two of the reviewers' findings changed the design and are recorded in
> §11 rather than quietly absorbed.

---

## 1. What E4 is, and what it is not

The extensions spec (§3, "E4 — `DMN_Enum` and `DMN_FEEL`") proposes two things, adopting a
proposal the repo's author wrote by hand at `src/DMN/Types.hs:38-49`. This design **accepts the
first, rejects the second, and adds no markdown syntax for either.**

| | proposed | this design |
|---|---|---|
| **E4(a)** enum domains | new header syntax `requirement : {certified, reviewed, audited}` | **no new syntax.** The domain sub-row already exists, is already documented, and is already parsed. Start *honouring* it. |
| **E4(b)** `DMN_FEEL` | a `: FEEL` column switches to FEEL quoting conventions | **do not build.** Fix double-quoted string literals unconditionally instead. |

Neither half adds a `DMNType` constructor. That is the single most load-bearing decision here
and §3 explains why.

### The motivating defect for E4(a)

A column whose permitted values are **declared** accepts values outside them, silently, exit 0.
**[verified here]**

```
| F | Risk Category     | Routing (out)          |
|---|-------------------|------------------------|
|   | LOW, MEDIUM, HIGH | DECLINE, REFER, ACCEPT |
| 1 | HIHG              | DECLINE                |
| 2 | LOW               | ACCEPTT                |
```

```
$ dmnmd typo.md --to=ts
export function Routing ( Risk_Category : string ) {
  if (Risk_Category === "HIHG") { // 1
    return {"Routing":"DECLINE"};
  }
  else if (Risk_Category === "LOW") { // 2
    return {"Routing":"ACCEPTT"};
  }
}
$ echo $?
0
```

`HIHG` is a typo, not a fourth risk category; `ACCEPTT` is a typo, not a fourth routing. The
table is parsed, the domain is read, and the mismatch is discarded — the exact thing `CLAUDE.md`
forbids: *"Anything we parse but do not yet honour must produce a loud, located diagnostic."*

### The motivating defect for E4(b)

A double-quoted cell compiles to a comparison against the quote characters. **[verified here]**

```
$ dmnmd quoted.md --to=js          # cell reads:  "Fall"
  if (Season === "\"Fall\"") { // 1
$ echo $?
0
```

This matters far beyond hand-written markdown: **DMN XML writes every string that way**, so
every string imported through `-f xml` acquires a cell the markdown grammar reads wrongly.
Pinned by `symptom/md-quoted-string-cell-literal`.

### Not in scope

- **Gap analysis.** Declared domains are what make it *possible*; running it needs
  wildcard-coverage reasoning dmnmd does not have. Separate work.
- **`{…}` header syntax.** §11.
- **FEEL evaluation**, per the extensions spec §6.
- **E3.** E4 must not block it, and does not: nothing here narrows what a cell may contain.

---

## 2. The surface syntax

**Total new markdown syntax: none.** Two existing constructs start meaning what they already
look like they mean.

### 2.1 The domain sub-row

Already documented at `README.md:140-152` ("The column enums are giving in a subhead row
between the top row and body data row 1"), already used by DMN 1.3 figure 8.19, already parsed
into `ColHeader.enums` by `parseContinuationRows` (`src/DMN/ParseTable.hs:173-179`).

**The domain row goes *below* the `|---|` delimiter.** This is not a stylistic point and it is
the one thing an implementer is most likely to get wrong — two of the three designs wrote their
examples the other way round. GFM requires the delimiter immediately after the header row;
putting the domain row second means **there is no table at all**. **[verified here]**

```
$ pandoc -f gfm -t html below.md | grep -c '<table'     # delimiter 2nd, domain 3rd
1
$ pandoc -f gfm -t html above.md | grep -c '<table'     # domain 2nd, delimiter 3rd
0
```

and through GitHub's own renderer, the correct layout produces the domain row as the first
`<tbody>` row, cells intact:

```
$ gh api /markdown -X POST --input payload.json
<markdown-accessiblity-table><table role="table">
<thead><tr><th>F</th><th>Risk Category</th><th>Routing (out)</th></tr></thead>
<tbody><tr><td></td><td>LOW, MEDIUM, HIGH</td><td>DECLINE, REFER, ACCEPT</td></tr>
...
```

So the canonical form, which is exactly `README.md` Example 3's form:

```
| F | Risk Category     | Routing (out)          |
|---|-------------------|------------------------|
|   | LOW, MEDIUM, HIGH | DECLINE, REFER, ACCEPT |
| 1 | HIGH              | DECLINE                |
| 2 | MEDIUM            | REFER                  |
| 3 | LOW               | ACCEPT                 |
```

Grammar, in the register of `parseColHeader`'s doc comment at `ParseTable.hs:52`:

```
domainRow   ::= "|" hspace* "|" domainCell ("|" domainCell)* "|" eol
domainCell  ::= hspace* ( "-" | "" | value ("," value)* ) hspace*
value       ::= cell text that mkFEither accepts at the column's settled type
```

recognised, as today, by its **blank first cell** appearing before the first numbered data row.

> **Multiple sub-rows.** `parseContinuationRows` is `many` + `trim . unwords <$> transpose`
> (`ParseTable.hs:174-177`), so N sub-rows are joined **with a space** before `mkFs` splits on
> commas. A domain spread over two rows therefore silently becomes one domain with a
> space-joined value in the middle. This design does **not** change that; it inherits it, and
> §9 requires a new symptom case recording it so the next person does not discover it by
> accident. Found by an adversarial reviewer, not by the design.

### 2.2 The double-quoted cell

> A cell wrapped in double quotes is a string **literal**: `"Fall"` denotes the four-character
> value `Fall`, and the quotes are not part of it. This is how DMN XML writes strings, so a
> table imported from XML reads the same as one written by hand. A cell that is not a
> well-formed string literal is kept exactly as written — `Non-Participating` and `5' 10"` are
> unaffected.

No annotation gates this. Requiring `: FEEL` on every column imported from XML would be the
opposite of legible.

> **All or nothing per cell — do not make this per-fragment.** `mkFsEither` splits on commas
> before anything looks at what a cell means, so `not("Fall", "Winter", "Spring", "Summer")`
> arrives already shredded into four fragments, of which the middle two happen to be
> well-formed literals. Unquoting each on its own merits gives
> `not("Fall` / `Winter` / `Spring` / `"Summer")` — neither the source text nor a parse of it.
>
> **This was tried and reverted once before**, and the warning is recorded in
> `test/DmnXmlSpec.hs` above the frozen expectation for that cell. It was found only by reading
> that comment after a first implementation attempt broke the test. So a cell is unquoted only
> when **every** fragment is a well-formed literal, which keeps `"Fall"` and a genuine
> multi-value `"Fall", "Winter"` while leaving a shredded cell verbatim.

### 2.3 Worked examples

All four render as tables on GitHub; all four keep the delimiter in position 2.

**A — a declared domain.** Accepted today. Under E4, a `HIHG` in place of `HIGH` would be
refused (§8) instead of emitted.

```
| F | Risk Category     | Routing (out)          |
|---|-------------------|------------------------|
|   | LOW, MEDIUM, HIGH | DECLINE, REFER, ACCEPT |
| 1 | HIGH              | DECLINE                |
| 2 | MEDIUM            | REFER                  |
| 3 | LOW               | ACCEPT                 |
```

**B — a domain plus tests.** A *test* is not a member of the domain, it selects a subset, so
`< 18` and `-` are not checked against the value list.

```
| O | Age : Number | Risk Category     | Routing (out)          |
|---|--------------|-------------------|------------------------|
|   | [0..150]     | LOW, MEDIUM, HIGH | DECLINE, REFER, ACCEPT |
| 1 | < 18         | -                 | DECLINE                |
| 2 | -            | HIGH              | REFER                  |
| 3 | -            | LOW               | ACCEPT                 |
```

**C — quoted strings, as DMN XML writes them.**

```
| U | Season   | Dish (out)  |
|---|----------|-------------|
| 1 | "Fall"   | "Spareribs" |
| 2 | "Winter" | "Roastbeef" |
```

**D — a numeric domain driving output order.** `O` returns results in domain order (30, 10, 20),
not row order.

```
| O | Age : Number | Score (out) : Number |
|---|--------------|----------------------|
|   | [0..150]     | 30, 10, 20           |
| 1 | < 18         | 10                   |
| 2 | [18..65]     | 30                   |
| 3 | > 65         | 20                   |
```

---

## 3. The type algebra — what `DMNType` becomes

**Unchanged.**

```haskell
data DMNType = DMN_String | DMN_Number | DMN_Boolean | DMN_List DMNType
```

A declared domain is a **refinement on an existing type**, not a fifth type. `Risk Category`
with a domain of three strings is a `DMN_String` column restricted to three strings; `Score`
with `30, 10, 20` is a `DMN_Number` column restricted to three numbers.

This is the load-bearing decision, and the reason is not aesthetic:

> `dmnmd.cabal` sets `ghc-options: -Wno-unused-matches -fwrite-ide-info -hiedir=.hie`. There
> is **no `-Wall` and no `-Wincomplete-patterns`.** Adding a `DMNType` constructor would make
> `mkFEither` (`DecisionTable.hs:152-189`), `type2js` (`JS.hs:74-78`) and `type2l4`
> (`L4.hs:147-152`) non-exhaustive **with no warning at all**, and the failure would arrive as
> a runtime `Non-exhaustive patterns` in front of a user.
>
> That is not hypothetical. `app/Main.hs:175-177`'s `showToJSON` is non-exhaustive **today**,
> and crashes whenever `-q` is used — recorded as `symptom/cli-showtojson-unknown-format`, and
> visible as the reason the README's whole `## Evaluation` transcript does not work.

The domain therefore lives where it already lives — `ColHeader.enums :: Maybe [FEELexp]` — and
`baseType` (`Types.hs:56-59`) needs no case.

**What becomes unrepresentable:** nothing that is representable today. **What stays
unrepresentable:** a domain whose members are not expressible as cells (there is no such thing),
and a *named* enum type reusable across columns (out of scope, and no markdown surface would
survive §6's lawyer test).

---

## 4. Parser changes

The critical structural fact, and the thing that makes this more than a one-line check:

> **The domain is merged into the header *after* the cell signatures are taken, and the data
> rows are parsed with the pre-merge signatures.** `ParseTable.hs`:
>
> ```
> 150:  let columnSignatures = columnSigs headerRow_1     -- pre-subhead
> 151:  subHeadRow <- parseContinuationRows
> 153:  let headerRow = ... zipWith ... enums ...          -- merge happens here
> 158:  dataRows <- parseDataRows columnSignatures         -- still the pre-merge sigs
> ```
>
> So a domain can never inform pass-1 cell parsing, and `mkDTable`'s inference pass
> (`reprocessRows`, `DecisionTable.hs:247-251`) rebuilds only the `DTrow`s — **never the
> enums**. That is precisely `symptom/struct-outputorder-enum-untyped`, whose `case.conf`
> already diagnoses it.

**Change 1 — `ParseTable.hs:151-157`: stop typing the domain in pass 1.** Do not build `enums`
here. Keep the raw text and hand it to pass 2.

**Change 2 — `ParseTable.hs:160-162`: pass the domain text into `mkDTable`.** `mkDTable` has
exactly one call site, so the signature change is contained:

```haskell
mkDTable :: String -> HitPolicy -> [ColHeader] -> [Maybe String] -> [DTrow] -> DecisionTable
```

**Change 3 — `DecisionTable.hs:237-251`: `mkDTable` gains the domain, in this order.** Each step
depends on the one before:

1. build the domain untyped, `mkFs Nothing`;
2. run `inferTypes` with the domain **as additional evidence** — the domain is the stronger
   signal of the two, since it is declared rather than observed;
3. **retype the domain** at the settled type, then `reprocessRows` as today;
4. `validateDomain` (§8) over every cell, collecting diagnostics.

Step 3 is what fixes `struct-outputorder-enum-untyped`: today the domain `30, 10, 20` is built
as strings while the cells become numbers, so `elemIndex` never matches and `O` degrades to row
order silently.

**Change 4 — `reprocessRows` must include `DMN_String`.** `DecisionTable.hs:258` reads
`if notElem (vartype ch) [Nothing, Just DMN_String] && …` — it skips `DMN_String` by name, which is why
the quoted-string fix needs this: `inferType`'s quote rule (`DecisionTable.hs:312`) already
returns `Just DMN_String` for `"Fall"`, but nothing re-runs the cell at that type.

**Change 5 — a string-literal parser** in the `Just DMN_String` arm of `mkFEither`: a cell that
is a well-formed double-quoted literal yields the unquoted value; anything else is unchanged.

**No new `: Type` name is introduced, so no new parse ambiguity is possible.** Worth recording
why, because it is stronger than it looks: `parseTypeDecl = Mega.optional $ lexeme ":" *>
parseType` (`ParseTable.hs:72`) — `lexeme ":"` **consumes**, so `Mega.optional`'s `<|>` cannot
backtrack, and any failure after a colon is unrecoverable by construction. Both rejected
syntaxes (`: {Fall, Winter}` and `: FEEL`) are hard errors today, exit 1, and cannot become
silent misparses.

---

## 5. Per-backend rendering

**Three of the four backends require no code change at all**, because E4 adds no `DMNType`
constructor. `type2js`, `type2py` and `type2l4` are untouched. What changes in their output is
only what the cell layer now produces.

| backend | change | why |
|---|---|---|
| `JS.hs` (serves `--to=js` **and** `--to=ts`) | none | a `type Risk = "LOW" \| "MEDIUM" \| "HIGH"` union would be new emission machinery for no semantic gain — the values are already checked at read time |
| `PY.hs` | none | `type2py` is **dead code** (only its own definition matches a grep); `mkArgument` discards the type. Python output is untyped today and stays so |
| `L4.hs` | none | see below |
| XML reader | yes, §7 | it is the one place a domain is genuinely *declared* by the input format |

The only output that changes is the quoted-string one, and it changes toward correctness:

```typescript
// today:  if (Season === "\"Fall\"") { return {"Dish":"\"Spareribs\""}; }
// after:  if (Season === "Fall")     { return {"Dish":"Spareribs"};     }
```

**L4 deserves a note.** `enums` already drives `outputOrder` → `mySort` → `sortCol` → `sortCell`,
used by `evalTable` for `HP_Priority` and `HP_OutputOrder` and by `L4.hs:96`. Step 3 above makes
that ordering correct for untyped columns without changing a line of `L4.hs`. The ditto grid is
untouched: E4 changes cell *values*, never their column widths, and `renderDittoGrid` measures
whatever it is handed.

---

## 6. `evalTable`

**No change, deliberately.** `evalTable` (`DecisionTable.hs:26-58`) does not pattern-match
`DMNType` at all, and E4 adds no constructor.

The domain would be enforced at **table-construction time**, in `mkDTable`, not at evaluation
time. By the time `evalTable` saw a `DecisionTable`, every cell would already be known to lie in
its column's domain, so the interpreter needs no membership test and gains no new failure mode.

One consequence worth stating: `sortCell` is `compare (elemIndex a enums) (elemIndex b enums)`,
so a value absent from the domain sorts as `Nothing` — i.e. **before every present value**,
silently. E4 would close both routes into that path (a value outside the domain; a type mismatch
between domain and cells), which is why `HP_Priority` and `HP_OutputOrder` would get more correct
without `evalTable` changing.

---

## 7. The DMN XML reader

`XmlToDmnmd.hs`'s `Col` record **already carries the domain text** — `colValuesText`, populated
for inputs at `:200` and outputs at `:222` from `<inputValues>`/`<outputValues>`, and converted
to `T.enums` in `resolveColumn` at `:334-337`.

So the reader already parses a declared domain and hands it to a field that
`DecisionTable.hs:76` reads only for output columns. `policy/xml-output-values` demonstrates it
exactly: an `<inputValues><text>[0..150]</text></inputValues>` produces no output and no
diagnostic — `expected/stderr` is empty, `expected/exit` is `0`. **Parsed, not honoured,
silent — the `CLAUDE.md` rule broken inside the reader written to enforce it.**

**Change:** `resolveColumn` runs the same `validateDomain` as the markdown path and reports
through `errorAt`, which is already located — `where_` builds `table "X": input column "Y": `
and `atRule` adds the rule id:

```
error: table "Band": input column "age": Rule_4: value outside the column's
  declared domain {"minor","adult","senior"} — the cell reads "middling"
```

`convTable`'s existing `| anyErrors cellDiags = (structural ++ cellDiags, [])` at `:118` then
refuses the table, which is the established contract.

---

## 8. Diagnostics — every new way to be wrong

One rule, proposed: **a value outside a declared domain would be an error, and the table
refused.** A table whose rules have been silently widened is a wrong answer that exits 0.

None of the messages below exists yet; they are the wording to implement.

| condition | severity | message |
|---|---|---|
| plain cell not in the declared domain | **Error**, table refused, exit 1 | `table "Routing": column "Risk Category": row 1: value outside the column's declared domain {LOW, MEDIUM, HIGH} — the cell reads "HIHG"` |
| plain numeric cell outside a declared numeric range | **Error** | as above, `domain [0..150]`, `the cell reads "200"` |
| domain member not parseable at the column's settled type | **Error** | `table "T": column "C": declared domain member "ten" is not a Number` |
| domain declared on a column whose cells settle to a different type | **Error** | names both types |
| XML `<inputValues>`/`<outputValues>` violated | **Error**, table not emitted | §7 |

**Not** errors, deliberately:

- a *test* (`< 18`, `[18..65]`, `-`, an arithmetic expression) in a domained column. A test is
  not a member of the domain; it selects a subset of it.
- a domain declared but never exercised. That is gap analysis, and it is out of scope (§1).
- an empty or `-` domain cell — the column declares no domain, as today.

---

## 9. Corpus impact

Baseline: **127 cases, 62 policy / 65 symptom, 127 unchanged.**

### symptom → policy (progress; re-record, then `git mv`)

| case | why it moves |
|---|---|
| `md-quoted-string-cell-literal` | emits `Season === "Fall"`. Correct, not merely different. Renamed `md-quoted-string-literal-unwrapped` on promotion, since the old slug named the defect. |
| `struct-outputorder-enum-untyped` | `O` returns domain order, not row order. Confirmed reachable today by hand-declaring the types. |

### symptom → symptom (re-record, **stays** symptom)

| case | why it stays |
|---|---|
| `struct-blank-rownum-swallowed` | a data row with a blank rule number is still swallowed as the domain row. E4 does not fix this and must not appear to. |

### New cases E4 must add

| class | slug | pins |
|---|---|---|
| policy | `enum-domain-violation-refused` | `HIHG` → exit 1, located message, nothing on stdout |
| policy | `enum-domain-test-not-checked` | `< 18` in a domained column is accepted |
| policy | `enum-domain-numeric-order` | `O` with `30, 10, 20` returns domain order |
| policy | `xml-input-values-honoured` | the §7 gap: an `<inputValues>` violation is refused |
| symptom | `enum-domain-multirow-joined` | two sub-rows are `unwords`-joined (§2.1). **Must be recorded before the implementation, so it is visible as inherited rather than introduced.** |

### Must not move

The other 123, in particular the three policy recordings that embed megaparsec/HXT error text
byte-for-byte — `md-parse-failure-exits-nonzero`, `md-partial-failure-emits-nothing`,
`xml-malformed-rejected`.

---

## 10. Implementation order

Each commit leaves the tree green (`cabal test` + `make corpus`).

1. **Record `enum-domain-multirow-joined` as a symptom.** Before touching code, so the
   `unwords` behaviour is on record as pre-existing.
2. **Quoted string literals** — parser changes 4 and 5 only. Moves
   `md-quoted-string-cell-literal` to policy. Self-contained, and the highest-value single fix
   in the cell layer.
3. **Plumb the domain into pass 2** — parser changes 1–3, no validation yet. Moves
   `struct-outputorder-enum-untyped` to policy. Behaviour-preserving otherwise.
4. **`validateDomain` + diagnostics** (§8), markdown path. Adds the three new policy cases.
5. **The XML path** (§7). Adds `xml-input-values-honoured`.
6. **Document it in `README.md`** — the domain-row paragraph at `README.md:144`, and a
   sentence on quoted cells. The README is a live fixture, so verify `--to=ts` over the old
   and new files still parses to the same tables.

Steps 2 and 3 are independently useful and independently revertable. If E4 is abandoned after
step 2, the tree is strictly better than it is now.

---

## 11. Rejected alternatives

**`requirement : {certified, reviewed, audited}` header syntax.** Rejected: the sub-row already
exists, `README.md` already documents it, DMN 1.3 figure 8.19 already uses it, and
`ColHeader.enums` already stores it. A second spelling of one concept, in a format whose entire
virtue is having little syntax. A reviewer confirmed braces are safe in GFM — they render
verbatim — so this is a design choice, not a rendering constraint.

**A `DMN_Enum` constructor.** Rejected on the exhaustiveness argument in §3. This is the
decision most likely to be revisited by someone who has not read that section.

**`DMN_FEEL` and a `: FEEL` column type.** Rejected for three reasons, the first of which is
decisive: **it does not fix its own motivating example.** The spec's illustration is
`Number Of Guests * Cost Per Head + 100`; that fails at the `+` because `parseFNF3`
(`ParseFEEL.hs:42-52`) is non-associative — `A * B` parses, `(A * B) + 100` does not. A type
annotation cannot repair an associativity bug in the expression parser. Second, its genuinely
useful half is string literals, and those must **not** be gated behind an annotation, because
DMN XML writes every string quoted. Third, `Season : FEEL` fails the lawyer test worse than
`Season : Number` does.

> What is worth doing instead is **fixing `parseFNumFunction` to fold left with precedence**.
> That is a cell-parser fix belonging in the extensions spec's "X hygiene" bucket, next to
> `parseFNumFunction`'s `error` that `symptom/num-function-call-crash` already records. It is
> not a type, and it should not wait on E4.

**Emitting a TypeScript string-union type for domained columns.** Rejected: new emission
machinery for no semantic gain, since values would already be checked at read time.

**Checking tests against the domain.** Rejected: a test selects a subset of the domain rather
than being a member of it, so `< 18` in a domained column is correct input.

### Two reviewer findings that changed this design

1. **The delimiter-position error.** Two of three designs put the domain row above the `|---|`,
   which GFM rejects outright — their examples were not tables. Caught by a legibility reviewer
   rendering every example through GitHub's actual renderer, and re-verified here. §2.1 now
   states the constraint explicitly because it is invisible until someone looks at the rendered
   page.
2. **The `unwords` multi-row join.** The design asserted its grammar was "exactly the row
   `parseContinuationRows` already matches"; it is not, because that function is `many` +
   `unwords`. §2.1 now records it and §9 requires a symptom case for it.

---

## 12. Open questions needing a human decision

1. **Should a domain violation be an Error or a Warning?** §8 says Error, following the
   `CLAUDE.md` rule. The counter-argument is real: `README.md`'s own Example 3 has domains on
   four columns, and if any existing table in the wild violates its declared domain, E4 turns a
   working table into a refused one. Mitigation: this is exactly what `make corpus` measures,
   and no case in the current 127 violates a domain. **Recommendation: Error.**
2. **Does the L4 backend want the domain?** L4 has `DECLARE … IS ONE OF`, so a domained column
   could emit a real sum type rather than `STRING`. That is strictly more faithful and strictly
   more work, and it interacts with the `GIVEN`/`GIVETH` shape. **Recommendation: not in E4**,
   but it is the obvious follow-on and worth deciding before §10 step 4 fixes the diagnostics
   wording.
3. **`DMN_List` and domains.** A `[String]` column with a domain — does the domain constrain
   the elements or the list? Nothing in the corpus or the README exercises it.
   **Recommendation: refuse the combination with a diagnostic until someone has a use case.**
