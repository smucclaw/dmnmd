# BUILD SPEC — dmnmd extensions for L4 interop

> ## Status: **PARTLY DISCHARGED — live plan, but §1 is a dated snapshot**
>
> Against the sequencing table in §4:
>
> | step | state |
> |---|---|
> | **E0a**, **E0b** | **shipped** — PR #17. The DMN 1.3 reader is conformant, a vendor `xmlns` and an `<inputData><variable/></inputData>` are accepted, and failures are visible instead of silent. Pinned by `test/corpus/cases/policy/xml-*`. |
> | **X** (hygiene) | **partly.** PR #19 added `test/corpus/`, a 105-case behavioural record, which is the "stop the next six-year gap" half. The error-position bug and `parseFNumFunction`'s `error` are **recorded as symptom cases, deliberately not fixed** — see `symptom/md-error-position-misreported` and `symptom/num-function-call-crash`. |
> | **E4** (`DMN_Enum` + `DMN_FEEL`) | **shipped, but not as specified.** Declared domains are now checked and quoted strings are unwrapped, with **no new markdown syntax and no new `DMNType` constructor** — the sub-header row already was the domain syntax. `DMN_FEEL` was **rejected**: it does not fix its own motivating example, because `parseFNF3` is non-associative. See `BUILD-SPEC-dmnmd-e4.md`, whose §13 records where its own design was wrong. **E8 shipped with it.** |
> | **E2, E3, E6, E1** | open, in that dependency order. E3 still must not ship before E4. |
> | **E7** | open. It **crashes, it does not misparse** — §2.1 predicted the wrong failure mode; see the erratum there. Its real fix is folding `parseFNumFunction` left with precedence, which belongs in row X, not in a type. |
> | **E8** | **shipped with E4.** A declared enum domain now has a home, and is checked. |
>
> **Correction, 2026-07-27.** An earlier version of this table called E4 "temporal types" and
> pointed at `policy/xml-temporal-typeref-refused`. That was wrong, and wrong in the way this
> repo keeps getting caught by: it conflated two separate items. **E4 is `DMN_Enum` and
> `DMN_FEEL`** (§3, and the author's own proposal at `src/DMN/Types.hs:38-49`). Temporal
> `typeRef`s — `date`, `time`, `date and time`, the two durations — are the **tail of E0a**,
> which §3 asks to "map the temporal types too, or degrade them to `DMN_String` with a warning".
> That half of E0a did not ship: an unmodelled `typeRef` is still refused outright, which is
> what `policy/xml-temporal-typeref-refused` actually pins, and which is safe and loud rather
> than wrong. It remains open, and is **not** E4.
>
> **§1 "Verified current state" was true on 2026-07-25 and is now partly false.** PR #17 moved
> several of the behaviours it reports. Do not quote §1 as evidence of how dmnmd behaves —
> run the binary, or read `test/corpus/`, which is a machine-checked record rather than prose.
> The specific trap already sprung once: an earlier draft claimed "fixing E0a alone changes
> nothing observable", which was false, and cost a wasted pass.
>
> §§2–7 (the gaps, the designs, the sequencing, acceptance, non-goals, and the exporter
> contract) remain current and are the reason this file is still here.
>
> **When the remaining steps land, delete this file** rather than archiving it. Its durable
> content belongs in three places that cannot go stale the same way: decisions in `CLAUDE.md`,
> verified behaviour in `test/corpus/`, and rationale in commit messages. The one exception is
> §6 Non-goals, which is a standing constraint and should move to `CLAUDE.md` first — it is
> already quoted there.

_Scoped 2026-07-25. Requirements source: the L4 DMN exporter (Track D1 of the Lexipedia-superset
programme, `legalese/l4-ide`). Companion to [`BUILD-SPEC-dmnmd-to-l4.md`](./BUILD-SPEC-dmnmd-to-l4.md),
which specifies the opposite direction (`dmnmd --to=l4`)._

---

## 0. Why this exists

`l4 export --to=dmn` lowers a typechecked L4 module to one IR (`L4.Dmn.IR`) and emits it through
two backends: **DMN 1.3 XML** for Camunda import, and **dmnmd markdown** for human review. The
markdown carrier is the one people will actually read — an XML diff in a pull request tells a
reviewer nothing, whereas a pipe table is the whole decision at a glance.

But dmnmd's grammar is **narrower than the IR**. Several things the exporter can say in XML it
cannot say in markdown, so it drops them and records a fidelity note naming the loss. This
document is the list of things it would stop having to drop.

**Nothing here blocks the L4 exporter.** It emits strictly within today's grammar, by design —
see §7. This spec raises the ceiling of what the markdown carrier can round-trip; it is not a
prerequisite for anything already in flight.

---

## 1. Verified current state

Probed 2026-07-25 against commit `3724e61` (branch `feat/translate-l4`), using the cabal-built
binary. **Note:** the shim at `~/.local/bin/dmnmd` is broken on this machine — it dies with
`Library not loaded: /usr/local/opt/pcre/lib/libpcre.1.dylib`. Use the build output directly:

> **Superseded.** That dyld failure can no longer happen: `regex-pcre` was retired with the
> `num-*` cell-layer work and the binary links no C library. Preferring the build output over
> the on-PATH copy is still good advice, for the unrelated reason that a stale `cabal install`
> silently tests a different binary — see the corpus runner's `corpus: using …` line.

```
languages/haskell/dist-newstyle/build/aarch64-osx/ghc-9.10.3/dmnmd-0.1.0.2/x/dmnmd/build/dmnmd/dmnmd
```

| probe                                                       | result                                                                                                                         |
| ----------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| `-t l4` on a single-table `.dmn.md`                         | ✅ works — emits well-formed L4 with `BRANCH` and ditto carets                                                                     |
| `-f xml` on a hand-built **DMN 1.3** table                  | ✅ **works** — `* imported 1 tables.`                                                                                              |
| `-f xml -t l4` on that same table                           | ❌ dies: `Unknown type: "number"` (`XmlToDmnmd.hs:85`)                                                                             |
| `-f xml` on `test/Traffic Violation.dmn`                    | ❌ namespace mismatch — that fixture is DMN **1.2** (`.../20180521/MODEL/`)                                                        |
| `-f xml` on `test/dish-decision.dmn11.xml`                  | ❌ namespace mismatch — DMN **1.1** (`.../20151101/dmn.xsd`)                                                                       |
| `-f xml` on `test/simple.dmn`                               | ⚠️ parses (it is 1.3) but the file genuinely contains **no decisions**, so `* imported 0 tables.` is correct, not a failure       |
| `-t xml`                                                    | ❌ **unsupported**: _"Supported output formats are 'ts', 'js', 'py' and 'l4'"_                                                     |
| `-t md`                                                     | ❌ **also unsupported** — same message. dmnmd cannot _write_ markdown, only read it                                                |
| `-f md` on `test/safe.md` (13 tables)                       | ❌ **parse failure at 75:1** — but the fixture is malformed, not the parser. See §1.1                                              |
| `-f md` on two hand-built tables in one file                | ✅ **works** — multi-table markdown is fine                                                                                        |
| `FileFormat`                                                | `Ts \| Js \| Py \| Xml \| Md \| L4 \| Unknown` (`app/Options.hs:58`) — but `Xml`/`Md` are **input-only**                          |

So the actual shape of the tool is:

> **in:** markdown (single-table), DMN 1.3 XML  **out:** ts, js, py, l4

Four consequences worth stating plainly, because the first two correct a natural misreading:

1. **The XML reader works.** `DMN.XML.ParseDMN` is not a stub. It is **pinned to the DMN 1.3
   namespace exactly** (`https://www.omg.org/spec/DMN/20191111/MODEL/`), and every XML fixture
   checked into `test/` is 1.1 or 1.2, so all of them fail — which makes the reader look broken when
   it is merely strict. **This matters for L4 interop: the L4 exporter emits DMN 1.3, so dmnmd can
   already read our output.**
2. **But the reader requires namespace declarations it never uses.** A DMN 1.3 file declaring only
   the MODEL namespace is rejected with
   `no namespace declaration found for namespace "https://www.omg.org/spec/DMN/20191111/DMNDI/"`.
   Adding `dmndi`, `dc` and `di` declarations to the root element — with no DI content at all — makes
   the same file import cleanly. Producers must therefore declare all four namespaces unconditionally.
3. **`convertType` is written against XSD type names, not FEEL ones.** It maps `string`, `boolean`
   and `integer`, and `error`s on anything else — including **`number`, which is *the* FEEL numeric
   type** in DMN 1.3 (`integer` is not a FEEL type at all). This is a two-line fix and it is the only
   thing standing between us and a working `-f xml -t l4`. See E0.
4. **Multi-table markdown is fine.** Two tables in one file parse cleanly; tables are separated by
   nothing more than a line not starting with `|`, and both blank lines and `##` headings are
   `irrelevantLine`. The `test/safe.md` failure is the fixture's fault, not the parser's — see §1.1,
   which is worth reading before you trust any parse error this tool reports.

### 1.1 `test/safe.md`, and why its error message lies

Diagnosed empirically. The headline: **the reported error position is wrong by eight lines**, and
the fixture has two independent defects at two different stages.

**Defect 1 — the file does not end with a newline.** `safe.md`'s last byte is `|`, not `\n`
(`wc -l` reports 82 for an 83-line file). In `app/ParseMarkdown.hs`,
`getTableLine = char '|' >> manyTill anyChar endOfLine`, and `endOfLine` accepts only `\n` / `\r\n`.
So the unterminated final row fails **after consuming its `|`**; `many1` therefore fails outright,
`try` backtracks the whole final `grepTable`, and the error surfaces from the fallback
`many irrelevantLine >> eof` — parked at that table's **header** row, eight lines earlier than the
actual problem. Hence `75:1: unexpected '|'` for a defect at EOF on line 83.

Minimal reproduction — three lines, **no trailing newline**:

```
| F | in | out |
| --- | --- | --- |
| 1 | true | 10 |
```

→ `** parser failure in grepMarkdown: 3:18: unexpected end of input`. Add one `\n` and it parses.
With two or more tables the message mutates into the misleading `unexpected '|'` form. It is not
specific to tables: a file ending in unterminated prose fails the same way.

> **Producer rule: every `.dmn.md` file must end with a newline.** Cheap to honour, and the
> diagnostic you get for violating it points at the wrong line.

**Defect 2 — `(out )` with a stray space**, real but masked by Defect 1. Isolated:
`| U | cond | Result (out ) |` → `1:21: unexpected '('`, because the post-label parser admits no
whitespace inside the parens. `(out)` parses.

**And beyond those two:** fixing only the newline gets all 13 chunks to stage 2, where **10 of 13
still fail** in `parseTable`. Eight are **single-column formula tables** with no hit-policy column
(`Safe Price`, `Discount Price`, `Conversion Amount`, …), failing with
`expecting Horizontal space or hitPolicy`; one uses `<` in a header
(`Event Date < Safe Termination Date`), which is not a legal identifier character.

So `safe.md` is not a fixture with a typo — it is substantially **unsupported by the current
parser**, and the trailing newline is only the first gate. Two observations for whoever picks this
up:

- The **error-position bug is worth fixing on its own** (`try` + fallback discarding the real
  failure site). A parser that reports a location eight lines from the fault will cost every future
  user the hour it cost here.
- **Single-column formula tables** look like a legitimate authoring idiom that the format simply
  does not admit — a named quantity defined by one expression, rather than a decision over cases.
  That is essentially E3 (§3) arriving from a different direction, and the two designs should be
  reconciled rather than solved twice.

---

## 2. The gaps

Each row is a thing the L4 IR can express, the markdown carrier cannot, and the exporter therefore
drops with a named fidelity note.

| #      | What L4 wants to say                                             | What dmnmd offers today                    | What the exporter emits instead                         |
| ------ | ---------------------------------------------------------------- | ------------------------------------------ | -------------------------------------------------------- |
| **E1** | A decision's input comes from **another decision's** output (DRG) | nothing — a table is an island             | N independent tables, dependencies invisible (`D-MD-NODRG`) |
| **E2** | Hit policy `U` **plus** a catch-all default output               | no `defaultOutputEntry`                    | downgrades the table to `F` (`D-MD-NODEFAULT`)             |
| **E3** | A decision that is **not a table** — a bare FEEL expression      | nothing — every decision is a table        | the decision is skipped entirely (`D-MD-NOLITERAL`)        |
| **E4** | Enum and record types                                            | `String \| Number \| Boolean \| List`      | collapses to `String` (`D-MD-TYPE`)                        |
| **E5** | Provenance (`@ref`) and rule-version / as-of date                | free-text `#` annotation columns           | provenance into a `#` column; as-of dropped               |
| **E6** | An input **expression** distinct from its human-readable label   | one header slot, must be an identifier     | the expression, with the label discarded                   |
| **E7** | A **negative number** in a numeric cell (`>= -5`)                | `mkF` reads any `+ - * /` as arithmetic    | **misparsed, not rejected** — see §2.1                      |
| **E8** | An **enum domain** on a column (`accredited \| retail \| …`)     | no column-values construct                 | collapsed to a bare `String` column                        |

### 2.1 The one that is worse than the rest: silent misreading

Every other row in that table is an *omission* — dmnmd cannot say it, we do not say it, and a
fidelity note records the loss. **E7 is different: dmnmd reads the cell and reads it wrongly.**
`mkF` treats the presence of any of `+ - * /` in a cell as a signal that the cell is an arithmetic
expression, so `>= -5` in a Number column is not a comparison against negative five.

That is the worst failure mode a format can have. An omission is visible to anyone comparing the
two documents; a misparse produces a table that looks right, parses clean, and means something
else. Whoever picks this up should treat "does any construct we accept get *misread*" as a
higher-priority question than "what can we not express", and audit `mkF` and `ParseFEEL.hs` for
others of the same kind.

_(E7 and E8 were found by the L4 exporter's author reading dmnmd's parser source, not by exercising
it — so E7 is inferred from the code path and has not been demonstrated end to end.)_

> **Erratum, 2026-07-27 — E7 is not a misparse.** The hedge above was right to be there. Running
> it: a negative number in a numeric cell **aborts with exit 1**, it does not produce a wrong
> answer. Three corpus cases already pin this and were recorded before this section was re-read:
> `symptom/num-negative-threshold-crash` (`>= -5`), `symptom/num-negative-range-crash`
> (`[-5..5]`), `symptom/num-function-call-crash`.
>
> The *diagnosis* holds — `mkF` does route any cell containing `+ - * /` to the arithmetic
> parser — but that parser then rejects a leading unary minus outright
> (`error: parsing suspected function expression >= -5 … expecting … digit, or letter`), so the
> run dies rather than proceeding on a wrong reading. E7 belongs in the same class as row X's
> `parseFNumFunction` crash: loud, badly-worded, and safe.
>
> **The paragraph's advice survives its example.** "Audit for constructs we accept and misread"
> is exactly right, and the audit finds real ones — just not this one. Two confirmed silent
> misparses, both exit 0:
>
> | cell | emits | pinned by | why it matters |
> |---|---|---|---|
> | `"Fall"` | `Season === "\"Fall\""` — compares against the quote characters | `symptom/md-quoted-string-cell-literal` | **DMN XML writes every string this way**, so the XML reader walks into it |
> | `not("Fall", "Winter")` | `Season === "not(\"Fall\"" \|\| Season === "\"Winter\")"` | `symptom/xml-comma-split-negation` | `mkFs` splits on commas *before* typing, so a function call is torn in half |
>
> Those are the ones that "look right, parse clean, and mean something else". The second is also
> frozen as an *expectation* in `test/DmnXmlSpec.hs`, which is why the hspec suite cannot certify
> a change to the cell layer — see `CLAUDE.md`. Both are E4's problem: a design that adds a
> `: FEEL` parse mode without fixing string quoting would leave the untyped default doing this.

E2 is the one that is actively dangerous rather than merely lossy: a `U` table written with a
catch-all row is a table dmnmd would **read back with different semantics**, so the exporter
downgrades to `F` rather than emit it. Fixing E2 removes a real correctness hazard, not just a
capability gap.

---

## 3. Proposed designs

Ordered by value-per-unit-effort. All are markdown-native and introduce as little new syntax as
possible — the format's whole virtue is that it is legible to someone who has never read a spec.

### 3.1 The evidence — what `safe.md` was trying to say

**Read this before the designs.** Two of them (E1, E3) were originally invented here; both turn out
to be documented by an author already, and the corpus corrects the details.

`test/safe.md` (Jason Morris, 2020-09-14, commit `9e8c479`) is a **transcription of a real DMN
model** — its sibling `test/safe2.dmn`, in the same directory, is the same SAFE agreement in DMN
XML. Comparing them shows exactly where the markdown format ran out.

**The eight "single-column formula tables" are DMN literal expressions.** Verbatim:

```
## Safe Price

| Safe Price                                                    |
| ------------------------------------------------------------- |
| decimal(Post-Money Valuation Cap / Company Capitalization, 4) |
```

```
## Cash Out Amount

| Cash Out Amount |
| --------------- |
| Purchase Amount |
```

The convention is consistent across all eight: three rows, the header cell is **the name of the
quantity being defined** and equals the `##` heading, and there is exactly one body row. No
hit-policy column, no inputs. `Cash Out Amount` is the tell — its body is a bare `Purchase Amount`
with no arithmetic at all, so the shape is not "a table with one computed column". It is
**`name ::= expression`**.

And in `safe2.dmn` those same seven quantities exist as **`<literalExpression>` decisions**:
`safe price ::= post money valuation cap / company capitalization`,
`conversion price ::= min(safe price, discount price)`, and so on. So **E3 is not a new feature —
it is a construct DMN already has and the markdown carrier lacks.** The author needed it, found no
notation for it, and improvised the one-column table.

**The tables chain by name — E1's design, authored.** Formula bodies reference other headings
(`Conversion Price` → `Safe Price`, `Discount Price`; `Liquidity Price` → `Liquidity
Capitalization`), and the decision tables chain through columns: a table named `X` emits `X (out)`
and a later table consumes a bare input column `X`. `Dissolution Event` takes `Liquidity Event`;
`Safe Event Type` takes both. `safe2.dmn` carries the same graph explicitly as
`<informationRequirement>`. This is what corrects E1's matching rule.

**Input headers are expressions, not names.** `| U | Safe Terminated | Event Date < Safe Termination
Date | … |` looks like a column name containing an operator. The XML settles it: DMN gives an input
column **two slots**, `label` and `inputExpression`, and there they are
`'date of event before date of termination?'` and `'date of event < date of SAFE termination'`.
Markdown has one slot, and the author put the **expression** in it. That is a different problem from
"allow `<` in identifiers" — see **E6**.

**Two hard blockers found by measuring how far the failures actually are:**

- `Event Prior to Termination` is **one character** from parsing — the `<` is its only defect.
- `Result of Termination` is three deep: `(out )`, then the hyphen in `Non-Participating` (not a
  legal identifier character), and then it **crashes** — `floor(Purchase Amount / Conversion Price)`
  hits an uncaught `error` at `src/DMN/DecisionTable.hs:151`, _"parsing suspected function
  expression"_. Any numeric cell containing `+ - * /` routes to `parseFNumFunction`, which handles
  arithmetic over variable names but **not function calls**.

That crash is load-bearing for sequencing: the eight formula bodies are all `decimal(…)`,
`floor(…)`, `if…then…else`. **Accepting the one-column shape without FEEL-capable cell parsing just
moves the failure one stage later**, and into a hard crash rather than a diagnostic. E3 and E4 are
therefore coupled, and `parseFNumFunction`'s `error` should become a diagnostic regardless.

**Provenance: the format outran the parser and nothing noticed for six years.** `safe.md` landed
2020-09-14; `src/DMN/ParseTable.hs` was last touched ten days earlier and then not again until
2022-12-29. `README.md` — which doubles as the format's spec — documents only hit-policy tables. And
**`safe.md` is referenced by no test**. There is no commit at which it parsed: this is not a
regression, it is a design someone wrote down, checked in, and never wired to anything that would
have complained. Worth naming, because the fix for that is a test, not a parser.

### E0 — **do this first.** Four bug fixes that unlock a differential test

Not features; bugs and a papercut. Highest value per line of anything in this document.

> **Scope correction, 2026-07-25.** An earlier version of this section said E0a was the only thing
> standing between us and a working `-f xml -t l4`. That was wrong. The L4 exporter drove its own
> DMN 1.3 output through dmnmd and stripped it one construct at a time until a minimal numeric
> table got through. **There are four blockers, and E0a is the last of them** — the first three are
> `tDefinitions` unpickling failures that fire *before* any type conversion runs, so fixing E0a
> alone changes nothing observable.

| #   | symptom (verbatim from dmnmd)                                                                                                                                  | cause                                                                                             |
| --- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| 1   | `xpCheckEmptyContents: unprocessed XML content detected`, `context: element "{…MODEL/}inputData"`, `contents: <variable id="…" name="class" typeRef="string"/>` | `<variable>` is not modelled as a child of `<inputData>` — and `<variable>` is how DMN names an element's value |
| 2   | `no attribute value found for "label"`, `context: element "{…MODEL/}output"`                                                                                    | dmnmd **requires** `label` on `<output>`; DMN 1.3 makes it **optional**                              |
| 3   | `xpCheckEmptyContents…`, `contents: <defaultOutputEntry id="…"><text>false<…`                                                                                   | `<defaultOutputEntry>` unmodelled                                                                    |
| 4   | `dmnmd: Unknown type: "number"` (`src/DMN/XML/XmlToDmnmd.hs:85:31`)                                                                                             | **E0a** below                                                                                        |

Blockers 1–3 are conformance gaps against DMN 1.3 rather than exotic usage: every one of those
constructs appears in output a standard producer emits. Fix all four or the reader stays unusable
for real files.

**E0a — `convertType` must cover FEEL's type names** (`src/DMN/XML/XmlToDmnmd.hs:81-85`). Today:

```haskell
convertType (TypeRef "string")  = T.DMN_String
convertType (TypeRef "boolean") = T.DMN_Boolean
convertType (TypeRef "integer") = T.DMN_Number
convertType (TypeRef tname)     = error $ "Unknown type: " ++ show tname
```

`integer` is an XSD name, not a FEEL one. DMN 1.3's built-in FEEL types are `number`, `string`,
`boolean`, `date`, `time`, `date and time`, `days and time duration`, `years and months duration`.
At minimum add **`number`** (which is what every real 1.3 producer emits, ourselves included);
ideally map the temporal types too, or degrade them to `DMN_String` with a warning rather than
`error`. **Do not leave an `error` call on the default branch** — an unrecognised `typeRef` should
be a diagnostic, not a crash, since it takes down the whole conversion for one unknown column.

**E0b — accept DMN 1.3 files that do not declare unused namespaces.** A file declaring only the
MODEL namespace is currently rejected because the unpickler wants a `DMNDI` declaration present
even when there is no DI content. That is stricter than the standard requires. Either relax it, or —
if the unpickler's structure makes that hard — say so in the error message, because the current
text sends the reader hunting for a missing element rather than a missing `xmlns:` attribute.

**What E0 buys, immediately.** The L4 exporter emits *both* DMN 1.3 XML and dmnmd markdown from one
IR. With E0a fixed, both can be pushed through dmnmd to L4 and the results compared:

```
our XML ──► dmnmd -f xml -t l4 ──┐
                                  ├──► same L4?
our MD  ──► dmnmd -f md  -t l4 ──┘
```

Any disagreement is a bug in exactly one of our two emitters, localised for free — and dmnmd is the
neutral referee, since it has no stake in either. That is a strong consistency check available for
the cost of adding one line to a case expression. **It does not require `-t xml` or `-t md` to
exist**, which is why E0 is scoped to the reader only.

**Separately: decide what to do about `-t xml` / `-t md`.** Both are accepted by the option parser
and rejected at runtime. Either implement them or remove them from `FileFormat`; advertising a
format that fails at the last step is the worst of the three states. Writing DMN XML is a real
project (the `xsd/` directory already carries DMN13.xsd, DMNDI13.xsd, DC.xsd and DI.xsd, so schema
validation is available to whoever attempts it); writing markdown is much smaller and arguably more
useful, since it would make dmnmd a normaliser for its own format.

### E1 — DRG by name, no new syntax

A `DecisionTable` already has a `tableName`. Proposal:

> **An input column whose name matches another table's name in the same document is an information
> requirement**, resolved to that table's output.

```
| U | offering size : Number | financial statements required (out) : String |
...
| F | financial statements required : String | assurance level (out) : Number |
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ names the table above ⇒ an edge in the DRG
```

**This design is confirmed by an author, not invented here** — see §3.1. `test/safe.md` already
chains its tables exactly this way, and its sibling `test/safe2.dmn` carries the corresponding
`<informationRequirement>` graph explicitly. Two corrections and three notes follow from that
evidence.

- **Match against output-column names, not table headings** — this reverses what this spec
  originally said. The authored convention is that a table named `X` emits a column `X (out)`, and a
  consumer takes a bare input column `X`. Keying off the output column is what the corpus actually
  does; keying off the heading breaks on case (`## Safe Event Type` vs the column `SAFE Event Type`).
  So: **match case-insensitively against output column names**, and report ambiguity rather than
  silently picking one.
- **The `(out)` marker is not reliably present.** In `safe.md`, `Liquidity Event` and
  `Dissolution Event` mark their output `(out)`; `Event Prior to Termination` and `Safe Event Type`
  leave the final column unmarked; `Result of Termination` marks all seven. Resolution must cope
  with the unmarked-final-column convention, which the parser already accepts.
- **v1 restriction:** only single-output tables can be referenced this way. Multi-output tables need
  a column selector, and that is a second design.
- Requires cycle detection and a topological evaluation order in `evalTable`'s caller.
- Report resolved edges under `-v`. Implicit linkage is only acceptable if it is observable.
- **Fallback if implicit proves too magical in practice:** an explicit per-table `# requires:`
  annotation. Prefer implicit first; it is strictly less syntax and easy to retreat from.

### E2 — a default-output row

> **A final row whose rule-number cell is `-` (or empty) is the default output, not a rule.**

```
| U | age : Number | band (out) : String |
|---|---|---|
| 1 | < 18         | minor               |
| 2 | >= 65        | senior              |
| - | -            | adult               |   ← default output, not a rule
```

This is legal under `U`, `A` and `P` — precisely the hit policies where a catch-all rule would
otherwise create an overlap and make the table invalid. Under `F`/`O`/`R` the marker should be a
parse error (a default is meaningless when order already decides), which keeps the feature from
being used to paper over a hit-policy mistake.

`DTrow.row_number` is already `Maybe Int`, so the AST needs no change — only a flag distinguishing
"unnumbered row" from "the default". Suggest a `row_default :: Bool` or promoting `row_number` to a
three-way sum.

### E3 — literal-expression decisions

DMN's own name for this is a **literal expression**: a decision whose logic is one FEEL expression
rather than a table. `safe2.dmn` has seven of them; `safe.md` needed all seven and had no notation,
so it improvised (§3.1). So this is not a feature request against DMN — it is the markdown carrier
catching up with a construct its source notation has had all along.

**Adopt the authored form.** It is already in the repo, it is what a transcriber reached for
unprompted, and it keeps the document uniformly tabular:

```
## Safe Price

| Safe Price                                                    |
| ------------------------------------------------------------- |
| decimal(Post-Money Valuation Cap / Company Capitalization, 4) |
```

> **A table with exactly one column and exactly one body row is a literal-expression decision:
> the header cell names it, the body cell is its FEEL expression.**

No new syntax at all — it is a degenerate case of the existing grammar, currently rejected because
`parseTable` demands a hit-policy column. That is why this is preferable to the fenced-code-block
form this spec originally proposed: a fenced block is new syntax that solves a problem the corpus
had already solved.

A literal-expression decision is also a perfectly good DRG node, which is how the two designs meet:
`Conversion Price` is a literal expression that references `Safe Price` and `Discount Price`, both
literal expressions themselves.

**Sequencing warning, from §3.1:** accepting the shape is necessary but not sufficient. All eight
authored bodies are `decimal(…)`, `floor(…)` or `if…then…else`, and `parseFNumFunction` **crashes**
on function calls (`src/DMN/DecisionTable.hs:151`, uncaught `error`). Land E3 alone and every real
example fails one stage later, harder. Either do E4 with it, or gate E3 on cell contents it can
actually parse — and turn that `error` into a diagnostic either way.

### E6 — an input header is an expression, not a name

Found in the corpus, not anticipated by this spec. DMN gives an input column **two** slots:

```
label           = 'date of event before date of termination?'
inputExpression = 'date of event < date of SAFE termination'
```

A markdown header has **one**, and `safe.md`'s author put the expression in it —
`| U | Safe Terminated | Event Date < Safe Termination Date | … |` — discarding the human-readable
label. The parser rejects it because `<` is not a legal identifier character.

The reframing matters more than the fix: **every input header is an expression**, and ordinary ones
like `Safe Terminated` are degenerate expressions that happen to be bare identifiers. "Allow `<` in
names" would be the wrong repair — it treats the symptom and leaves `Non-Participating` (a hyphen in
a *cell* value) and `floor(…)` still broken for the same underlying reason.

Two directions, and the choice is a genuine design decision this spec does not make:

1. **Widen headers to expressions**, with the type annotation still available
   (`Event Date < Safe Termination Date : Boolean`). Simplest, matches what was authored, loses the
   label.
2. **Give the header both slots**, e.g. `label ⟨expression⟩` or a `#`-annotation row carrying labels.
   Faithful to DMN, more syntax, and the format's virtue is having little.

Whoever takes this should look at how much of `README.md`'s existing grammar assumes a header is an
identifier before choosing.

### E4 — `DMN_Enum` and `DMN_FEEL`

**This is already proposed in this repo, in the author's own hand.** `src/DMN/Types.hs` lines 38–49
carry a comment block proposing exactly `DMN_Enum` and `DMN_FEEL`, including the rule that a column
typed `: FEEL` switches to FEEL quoting conventions (literal strings double-quoted, bare identifiers
read as variables, so `Number Of Guests * Cost Per Head + 100` parses). This spec adopts that
proposal rather than inventing a rival.

Two additions from the L4 side:

- **Enum domains in the header**, so the type carries its values:
  `requirement : {certified, reviewed, audited}`. `ColHeader` already has an `enums` field
  (populated today by observation, for `HP_OutputOrder`); this would let it be **declared** instead
  of inferred, which is what makes a *gap* analysis possible — you cannot detect a missing case
  without knowing the domain.
- Enum-typed columns should be checked: a cell value outside the declared domain is an error, not a
  string.

The gap-analysis point is the reason to prefer E4 over E1 if only one gets built. Declared enum
domains are the difference between a table you can *read* and a table you can *check*.

### E5 — provenance by convention, not syntax

> **Qualified 2026-07-25** by the L4 exporter's empirical pass. Provenance is expressible, but
> more narrowly than stated below: a `#` column's **header must itself be a legal varname**
> (`parseVarname`: a letter, then `[alnum | space | tab | _]`). So `# ref` works as a header and the
> citation lives in the cells — but you cannot name a column after the citation, and a header
> carrying `§`, `(`, `)` or `.` is rejected. The L4 exporter currently emits no provenance at all
> and reports no note for it; that is an exporter gap, not a format gap.

Provenance is **already expressible**: annotation (`#`) columns carry free text. So this needs no
format change, only a convention — recommend `# ref` for a citation column, which the L4 exporter
will populate from `@ref`.

Rule-version / as-of date has **no home** in the format and no obvious markdown-native one.
Recommend leaving it out of scope: it is a property of a whole document, not of a table, and
front-matter is a bigger decision than this spec should make.

---

## 4. Sequencing and effort

| step | depends on | rough shape                                                                                       |
| ---- | ---------- | --------------------------------------------------------------------------------------------------- |
| E0a  | —          | **hours.** One case-expression arm plus removing an `error`. Unlocks the differential test            |
| E0b  | —          | small — relax or explain the namespace requirement                                                    |
| X    | —          | **hygiene, small each.** Wire the fixtures into the test suite; fix the eight-line error-position bug; turn `parseFNumFunction`'s `error` into a diagnostic |
| E2   | —          | smallest real feature; parser flag + `evalTable` case. Removes a correctness hazard                   |
| E4   | —          | `DMNType` gains two constructors; touches parser, all four translate backends, `evalTable`            |
| E3   | **E4**     | accept the authored one-column form. Alone it just moves the failure one stage later — see §3.1       |
| E6   | —          | headers become expressions; a real design choice, not a mechanical change                             |
| E1   | E3 helps   | largest — needs name resolution, cycle detection, evaluation ordering                                 |
| —    | —          | _optional, separately sized:_ `-t xml` and/or `-t md` writers, or removing them from `FileFormat`     |

**Start with E0a.** It is the smallest change here and the only one that immediately improves
something outside this repo: it turns dmnmd into a neutral referee for the L4 exporter's two
backends.

**Then do X, and do not skip it.** §3.1 establishes that `safe.md` has never parsed at any commit,
is referenced by no test, and describes a format `README.md` does not document. The parser did not
regress — nothing was watching. Wiring the fixtures into the suite is what prevents the next
six-year gap, and it costs less than any feature in this document. The misreported error position
and the `parseFNumFunction` crash belong in the same pass: both make every downstream failure harder
to diagnose than it needs to be, and both will otherwise tax whoever does E3.

After that, E2 is independent and self-contained. **E3 must not ship before E4** — its authored
bodies are all function calls, which is precisely what the cell parser crashes on. E1 is the one
that changes dmnmd's model of the world — from "a table" to "a graph of tables" — and should be
last.

---

## 5. Acceptance

For each extension:

1. A fixture under `test/` exercising it, parsed and round-tripped.
2. `-t l4` output that **typechecks** against the L4 in `legalese/l4-ide` — the existing
   `BUILD-SPEC-dmnmd-to-l4.md` §7 golden strategy applies unchanged.
3. `evalTable` semantics preserved: the reference interpreter is the oracle, per that spec's §0.
4. For E2 specifically: a `U` table with a default row and a `U` table with a catch-all *rule* must
   be distinguishable, and the second must be rejected.

The integration-level acceptance, once E1 and E2 are in: the L4 exporter's Reg CF output —
`financial statements required` (4 tiers, `F`) feeding `assurance level` (3-way `CONSIDER`, `U`
with a default) — round-trips as **two linked tables**, and `dmnmd -t l4` reproduces L4 that
typechecks and evaluates to the same answers as the original corpus.

---

## 6. Non-goals

- **Not** a general DMN implementation. dmnmd's value is that it is a markdown table; every
  extension must survive the test "would a lawyer still recognise this as a table?"
- **Not** FEEL evaluation for E3. Carrying the text is v1.
- **Not** rule-version / temporal validity (see E5).
- **Not** BPMN. The process side of the L4 programme targets BPMN 2.0 XML directly and has no
  markdown carrier.

---

## 7. Interaction with the L4 exporter — the contract

The L4 side has been instructed to:

- emit **strictly within today's grammar**, inventing no syntax;
- emit **one table per file**, given §1's multi-table finding;
- record every gap as a structured fidelity note (`D-MD-*`) rather than approximating;
- treat dmnmd as **local validation, never a CI dependency** — `legalese/l4-ide` must not acquire a
  build dependency on this repo.

So the two sides can move independently. When an extension here lands, the exporter drops the
corresponding note and starts emitting the richer form; until then it degrades honestly. Neither
repo waits on the other.
