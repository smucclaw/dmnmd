# Handoff — starting a dmnmd session

_Written 2026-07-25 from a `legalese/l4-ide` session that was building the L4 → DMN exporter and
kept running into this repo's edges. Everything below was verified by running it, not inferred._

**How to use this file:** open a new Claude Code session with `~/src/smucclaw/dmnmd` as the working
directory and point it at this document. It is written to be read first and to make the rest of the
repo make sense.

---

## 1. What this repo is

`dmnmd` reads decision tables written as **GitHub-flavoured markdown pipe tables** and compiles them
to executable code. It is a small, opinionated Haskell tool whose thesis is that a decision table is
more useful as a table a human can read than as XML a human cannot.

```
| F | merchant : String | mcc : Number | Category (out) : String |
|---|---|---|---|
| 1 | Cold Storage, CS Fresh | -    | GroceriesPhysical       |
| 2 | -                      | 5814 | FastFood                |
```

Actual capability, verified (§3) — narrower than the CLI advertises:

> **in:** markdown (single-table), DMN 1.3 XML   **out:** ts, js, py, l4

---

## 2. Why a session is being started now

`legalese/l4-ide` is building **`l4 export --to=dmn`**: a compiler from typechecked L4 to decision
tables, emitting **two** carriers from one IR — DMN 1.3 XML for Camunda import, and **dmnmd markdown
for human review**. The markdown carrier is the one people will actually read; an XML diff in a pull
request tells a reviewer nothing.

That work is done and does **not** wait on this repo — it emits strictly within today's dmnmd
grammar and records every gap as a structured fidelity note. See §7 for the contract.

Two documents define the work here:

| file                                                                   | what it is                                                                   |
| ---------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| [`BUILD-SPEC-dmnmd-extensions.md`](./BUILD-SPEC-dmnmd-extensions.md)   | **the spec.** Verified current state, five gaps (E0–E5), proposed designs, sequencing |
| [`BUILD-SPEC-dmnmd-to-l4.md`](./BUILD-SPEC-dmnmd-to-l4.md)             | the existing `--to=l4` backend, already built. Its mapping tables invert usefully |

**Read `BUILD-SPEC-dmnmd-extensions.md` §1 first.** It is the verified-state section and it corrects
several things a reasonable person would otherwise conclude by reading the source.

---

## 3. Environment — verified, with the traps

### The binary

Build with cabal from `languages/haskell/`. (There is also a `stack.yaml`; cabal is what has been
exercised recently.)

```
cd ~/src/smucclaw/dmnmd/languages/haskell
cabal build
cabal test        # hspec; test/Spec.hs, includes DmnXmlSpec, ParseFEELSpec, TranslateL4Spec
```

The built executable lands at:

```
languages/haskell/dist-newstyle/build/aarch64-osx/ghc-9.10.3/dmnmd-0.1.0.2/x/dmnmd/build/dmnmd/dmnmd
```

**Check `which dmnmd` before trusting the on-PATH copy.** As of this handoff a stale
`~/.local/bin/dmnmd` was being repaired or removed — it was an old Intel-Homebrew build that died
with `Library not loaded: /usr/local/opt/pcre/lib/libpcre.1.dylib`. If `dmnmd --help` prints usage,
it is fine; if it dies with a dyld error, reinstall with:

```
cabal install exe:dmnmd --overwrite-policy=always --install-method=copy \
  --installdir=$HOME/.local/bin
```

### Three traps that will cost you an hour each

1. **Every XML fixture in `test/` is the wrong DMN version.** `Traffic Violation.dmn` is 1.2,
   `dish-decision.dmn11.xml` is 1.1. The reader is pinned to **1.3**
   (`https://www.omg.org/spec/DMN/20191111/MODEL/`) and rejects them on the root element. The reader
   is *not* broken; it is strict and under-fixtured. Add a 1.3 fixture before concluding anything.
2. **`test/simple.dmn` imports 0 tables and that is correct** — it is 1.3, it parses, and it
   genuinely contains no decisions. Do not use it to test the decision-table path.
3. **A DMN 1.3 file must declare `dmndi`, `dc` and `di` namespaces even with no DI content**, or the
   unpickler rejects it with a message that points at the wrong thing (`no namespace declaration
   found for …DMNDI/`). Note DC and DI keep their **2018-05-21** dates in 1.3 — correct, not a typo.
4. **The markdown parser reports the wrong line.** `test/safe.md` fails with `75:1: unexpected '|'`;
   the actual defect is a **missing final newline at line 83**. `try` backtracks the failed table and
   the error resurfaces from the fallback parser, parked eight lines earlier at that table's header.
   Do not trust a `grepMarkdown` position without checking EOF first. Full diagnosis in the spec
   §1.1 — **and fixing this error-reporting bug is arguably worth more than any feature in §3**,
   because it will mislead every future user exactly as it misled us.

### A known-good 1.3 fixture

This one imports (`* imported 1 tables.`) and is worth checking into `test/`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
             xmlns:dmndi="https://www.omg.org/spec/DMN/20191111/DMNDI/"
             xmlns:dc="http://www.omg.org/spec/DMN/20180521/DC/"
             xmlns:di="http://www.omg.org/spec/DMN/20180521/DI/"
             id="defs_1" name="Demo" namespace="http://example.com/demo">
  <decision id="d_band" name="band">
    <decisionTable id="dt_band" hitPolicy="FIRST">
      <input id="i_age" label="age">
        <inputExpression id="ie_age" typeRef="number"><text>age</text></inputExpression>
      </input>
      <output id="o_band" label="band" name="band" typeRef="string"/>
      <rule id="r1">
        <inputEntry id="ie1"><text>&lt; 18</text></inputEntry>
        <outputEntry id="oe1"><text>"minor"</text></outputEntry>
      </rule>
      <rule id="r2">
        <inputEntry id="ie2"><text>&gt;= 18</text></inputEntry>
        <outputEntry id="oe2"><text>"adult"</text></outputEntry>
      </rule>
    </decisionTable>
  </decision>
</definitions>
```

`-f xml -t l4` on it currently dies with `Unknown type: "number"` — which is E0a, the first task.

### Schemas are already vendored

`xsd/` carries `DMN13.xsd`, `DMNDI13.xsd`, `DC.xsd`, `DI.xsd` (and 1.1/1.2); `xrelaxng/` carries the
RelaxNG equivalents. So `xmllint --schema` validation is available to anyone implementing an XML
writer, without fetching anything from OMG.

---

## 4. The work, in order

Full detail in `BUILD-SPEC-dmnmd-extensions.md` §3. In priority order:

| #       | task                                                                                     | size       |
| ------- | ------------------------------------------------------------------------------------------ | ---------- |
| **E0a** | `convertType` covers FEEL's type names — add `number`, stop `error`ing on the default      | hours      |
| **E0b** | accept 1.3 files that omit unused namespace declarations, or improve the error message     | small      |
| **X**   | **wire the fixtures into the test suite**, fix the eight-line error-position bug, turn `parseFNumFunction`'s `error` into a diagnostic | small each |
| **E2**  | a default-output row (`-` in the rule-number column) — removes a correctness hazard        | small      |
| **E4**  | `DMN_Enum` + `DMN_FEEL` — **already proposed in `src/DMN/Types.hs:38–49`, in your own hand**     | medium     |
| **E3**  | literal-expression decisions — the one-column form `safe.md` already uses. **After E4**    | medium     |
| **E6**  | input headers are expressions, not identifiers — a genuine design choice                   | medium     |
| **E1**  | a decision requirement graph, by output-column-name matching, no new syntax                | large      |

**Read spec §3.1 before touching E1 or E3.** Both were originally invented in this spec; both turn
out to be documented by an author already. `test/safe.md` is a transcription of `test/safe2.dmn`
(the same SAFE agreement in DMN XML), and comparing them shows the eight "broken" one-column tables
are DMN **literal expressions** — a construct the standard has and the markdown carrier lacks — while
the tables' name-chaining reproduces the XML's `<informationRequirement>` graph. The corpus also
corrects E1's matching rule and turns up E6, which nobody had anticipated.

**Do E0a first.** It is the smallest change in the list and the only one that immediately improves
something outside this repo: it turns dmnmd into a **neutral referee** for the L4 exporter's two
emitters —

```
L4 ─┬─► our DMN 1.3 XML ──► dmnmd -f xml -t l4 ──┐
    │                                             ├──► should be the same L4
    └─► our dmnmd markdown ──► dmnmd -f md -t l4 ─┘
```

Any disagreement is a bug in exactly one of our two emitters, localised for free, by a tool with no
stake in either. The markdown leg works today; only the XML leg is blocked, on one missing case
alternative.

---

## 5. Conventions

- Branch off whatever the current working branch is (`feat/translate-l4` at time of writing); the
  tree was clean. Remote is `git@github.com:smucclaw/dmnmd.git`.
- Do **not** copy the git trailers out of the l4-ide specs — those carry a session URL that will not
  be yours. Use whatever your own session prescribes.
- Acceptance for every extension is in `BUILD-SPEC-dmnmd-extensions.md` §5. The short version:
  a fixture under `test/`, `-t l4` output that typechecks against real L4, and `evalTable` preserved
  as the semantic oracle.

---

## 6. Non-goals — please respect these

- **Do not make `legalese/l4-ide` depend on this repo.** dmnmd is local validation for that project,
  never a build or CI dependency. This is a hard constraint from the l4-ide side.
- **Do not extend the markdown format speculatively.** Every extension must survive the test *"would
  a lawyer still recognise this as a table?"* That legibility is the whole product.
- Not a general DMN implementation. Not FEEL evaluation for E3. Not BPMN — the process side of the
  L4 programme targets BPMN 2.0 XML directly and has no markdown carrier.

---

## 7. The contract with the L4 exporter

The l4-ide side has been instructed to:

- emit **strictly within today's grammar**, inventing no syntax;
- emit **one file per module**, all its tables together under `##` headings (multi-table markdown
  works — the `safe.md` failure was that fixture's own defects, see spec §1.1);
- **always end the file with a newline**, and never emit `(out )` with a space — the two producer
  rules that fall out of §1.1;
- declare all four DMN namespaces on `<definitions>`;
- record every gap as a structured fidelity note (`D-MD-*`) rather than approximating;
- treat dmnmd as local validation only.

So the two repos move independently. When an extension lands here, the exporter drops the
corresponding note and starts emitting the richer form. Until then it degrades honestly. **Neither
side waits on the other** — which is the point of writing the contract down.

---

## 8. The spec is complete — the empirical pass has landed

An earlier version of this file said §2 was awaiting an empirical gap list from the L4 exporter.
**That list is in and folded through the spec.** It was produced by actually driving L4 → DMN 1.3
XML and L4 → dmnmd markdown and recording every construct that would not go through, so §2 is now
measured rather than derived from reading the grammar. Three consequences worth knowing before you
plan:

- **E0 grew from one bug to four.** The exporter stripped its own DMN 1.3 output one construct at a
  time until a minimal numeric table imported. `convertType` lacking `number` is the **last** of
  four blockers; the first three are `tDefinitions` unpickling failures — `<variable>` unmodelled
  under `<inputData>`, `label` required on `<output>` where DMN 1.3 makes it optional, and
  `<defaultOutputEntry>` unmodelled — which fire **before** any type conversion runs. Fixing
  `convertType` alone changes nothing observable.
- **E7 is new and is the worst kind of defect in the document.** A negative number in a numeric cell
  (`>= -5`) is **misparsed, not rejected**, because `mkF` reads any `+ - * /` as arithmetic. Every
  other gap here is an omission a reader can spot by comparing two documents; a misparse yields a
  table that looks right and means something else. Treat "does anything we accept get *misread*" as
  a higher-priority question than "what can we not express", and audit `mkF` and `ParseFEEL.hs` for
  siblings.
- **E8 is new**: an enum column's domain has no home, so a three-constructor type becomes a bare
  `String` column and a completeness checker cannot know the domain is three.

Nothing here is blocking. Start with E0a/E0b and the row-X hygiene work regardless — they are bug
fixes, independent of anything the exporter wants.
