# A neutral DMN core on Hackage — findings for the `legalese/l4-ide` side

**Canonical copy: `smucclaw/dmnmd/DMN-CORE-HACKAGE-FINDINGS.md`.** If you are reading this in
another repo, it is a copy; corrections land here first. Written 2026-07-30 from a dmnmd session.

**Status: findings and a recommendation, not a decision, and nothing here has been built.** No
package exists, no `cabal.project` anywhere has been edited, and the recommendation below is
explicitly *do not build this yet*. Read it as input to a conversation, not as a plan you are
being handed.

**What is being proposed** — Meng's words, in a dmnmd session on 2026-07-30:

> Maybe we should just contribute a neutral DMN core implementation in Haskell to Hackage and
> then build dmnmd's next version on top of that. Just a thought.

Two multi-agent workflows examined it from different sides: one on repo alignment (which had it
as "shape 6" among six), one on DMN TCK conformance (which reached it independently and called it
"the best idea in the thread"). This is what they found, with the stale bits corrected against
the tree as of today.

**Every reference here now resolves on `trunk`.** An earlier version of this paragraph warned that
commit `837800b` — cited throughout as the point where dmnmd's last system dependency was retired —
was on an unpushed branch. That branch landed as PR #39 on 2026-07-31, and the rebase renamed the
commit to `36df5a9`. Both facts are recorded because a SHA that silently stops resolving is exactly
the kind of claim this document is trying not to make.

---

## 0. The one-paragraph version

Publishing a neutral `dmn-core` to Hackage is the only alignment shape that costs **zero l4-ide
document amendments**, because `CLAUDE.md` §1.2 — *"`legalese/l4-ide` must never depend on
`smucclaw/dmnmd`"* — is a rule about depending on **a repo in another org whose branch can
move**, and every harm it names is a property of that. An immutable, version-bounded Hackage
release is none of those things. §1.2 does not reach it. But the *timing* is wrong right now:
`L4.Dmn.IR` grew 45% in the four days after it was created, under a spec whose own header says it
is superseded and which is three of six phases from done. Freezing that type behind a cross-repo
package boundary is the single worst thing either repo could do this quarter. **The
recommendation is: declare it the destination, build none of it, and revisit when
`DMN-EXPORT-PROGRAM-MODEL-SPEC.md`'s status header stops saying "not yet implemented".**

---

## 1. Why §1.2 does not reach a Hackage dependency

This is the finding that changes the shape of the question, so it goes first.

`l4-ide/CLAUDE.md` §1.2 is the only rule in that file with no **Why** attached, in a file whose
preamble says every rule carries one. Its Why lives in *this* repo, written two days earlier —
`BUILD-SPEC-dmnmd-extensions.md` §7: *"treat dmnmd as local validation, never a CI dependency…
Neither repo waits on the other."*

What it actually protects, each verified:

| harm §1.2 names | true of a **SHA pin** | true of a **Hackage release** |
|---|---|---|
| dmnmd is unpublished, 404 on Hackage, **0 git tags** — any edge is a SHA pin against another org's branch | yes | **no** — `build-depends: dmn-core >= 0.1 && < 0.2` cannot be broken by a push to `smucclaw/dmnmd` |
| `nix/jl4-lsp/hs-overlay.nix:9` uses `callCabal2nix`, which reads `jl4-core.cabal` and **not** `cabal.project` — so a `source-repository-package` is invisible to it and the pin must be hand-added with its own `sha256` | yes — **the SHA then lives in three files, one needing a content hash**, in a repo carrying three CLAUDE.md rules about duplicated facts drifting | **no** — resolved from nixpkgs, no overlay entry, no hash |
| cadence: dmnmd CI is GHC 9.10.3 on `trunk` only; l4-ide is 9.10.2, plus a WASM job, plus nixpkgs `ghc910` — four toolchains, and a green dmnmd PR can redden three of l4-ide's four builds with no signal | yes | **no** — a release is a deliberate act with a version number on it |
| WASM: `cabal-wasm.project` builds `jl4-core`, so anything jl4-core reaches must cross-build for wasm32, and **dmnmd linked `libpcre`** | *was* yes | **no, and no longer true of dmnmd at all** — see §2 |

**So shape 6 makes §1.2 inapplicable rather than requiring it to be amended.** Of the shapes
considered, it is the only one costing zero edits to l4-ide documents. That is worth saying out
loud because it inverts the intuition that publishing is the *most* committing option.

**What it does not dispose of.** Immutability settles "who breaks whose build" completely — better
than a SHA pin does. It does not settle "who decides the API". It relocates that from a merge
queue into a semver contract, which is *slower* and *harder* to change unilaterally. That is the
real trade, and it is the right one only if the API is worth committing to. It is not yet (§3).

### The two document notes this implies — and they are the whole ask

Neither is an amendment to §1.2. Both are additions to `specs/todo/INLINE-DMNMD-SPEC.md`, whose
#173 merged 2026-07-29 (`087d9ae8`) and is now landed doctrine rather than a draft:

1. **§4.1 forecloses the Hackage option by accident.** Its absolutism —

   > an inline-table reader is an in-tree parser for the format … Never a shell-out to `dmnmd`,
   > never a library import, never a submodule, never a dev-dependency that CI happens to have.

   was written against a **repo** dependency and does not answer a **Hackage** dependency. That
   distinction is now load-bearing. Record it; do not weaken the rule.

2. **§4.2's argument should be marked a HAZARD, not a benefit.** It reads: *"the exporter already
   owns a serialiser, so a parser gets a fixture corpus free."* But `L4/Dmn/Markdown.hs:189`'s
   `mdValue (VStr t) = t` writes a string literal with **no escaping, no comma check and no `-`
   check**, while `Lower.hs`'s XML path calls `quoteFeelString` on the same literal. So an L4
   string `"active, pending"` becomes a **disjunction** in markdown, and `"-"` becomes an
   unconditional row that shadows everything below it — both at exit 0. A parser fixtured from
   that exporter inherits both defects. **This is verified**: `jl4/examples/dmn/expected/regcf-corpus.dmn.md:9`
   has a comma inside a string value today; dmnmd splits it into a branch that can never match,
   exit 0, no fidelity note. Quoting does not save it — `"c, d"` splits into `"c` OR `d"`.

The comma bug is l4-ide's to fix and has not been fixed; it was left alone because this session's
merge permission was dmnmd-only.

---

## 2. Correction: the WASM blocker is gone, on both sides

Both workflows treated `regex-pcre` as a live blocker on dmnmd's side. **It was retired on
2026-07-30** — dmnmd `36df5a9`, *"build: retire regex-pcre, and with it dmnmd's last system
dependency"*.

What made it removable was reading the patterns rather than replacing them. Unescaped, eight of
the nine were literal substrings (`".."`, `">"`, `"<"`, `"="`, `" * "`, `" + "`, `" - "`, `" / "`,
`" ** "`) — i.e. `isInfixOf`. Only `^\d+(\.\d+)?$` was a real pattern, and it is five lines of
`span isDigit`. A C-library binding was being linked for one anchored digit test. Proven
behaviour-preserving by dmnmd's behavioural corpus: **168 of 168 cases unchanged**, including all
58 that record known-wrong behaviour.

**dmnmd now has zero system dependencies.** `shell.nix` is an empty shell kept only as the
machine-readable record that the list is empty on purpose.

And l4-ide had already made the identical move from the other side: `specs/done/WASM-LSP-SPEC.md`
records dropping `pcre2` from `jl4-core.cabal` and replacing `Text.Regex.Pcre2` with plain string
matching, stated reason *"not WASM-compatible"*. **Two repos independently hit the same wall and
resolved it the same way.** That is the last *technical* objection to `build-depends: dmn-core`
gone. What remains is packaging and timing, which is why the recommendation is still "not yet".

One inherited claim not re-executed: a second reported WASM blocker, `makePrisms` failing in
`GHC.ByteCode.Linker.lookupCE`. Nobody ran `wasm32-wasi-ghc` in this session. It sits in modules
l4-ide would never import, which is what would make a sub-library split load-bearing rather than
tidy.

---

## 3. Why not yet: `L4.Dmn.IR` is the fastest-moving type in either tree

`specs/todo/DMN-EXPORT-PROGRAM-MODEL-SPEC.md:3-5`, verbatim, still the header at
`origin/unstable`:

> _Status: **design, not yet implemented.** Supersedes the program model currently in
> `jl4-core/src/L4/Dmn/{IR,Lower,Emit,Markdown}.hs`._

And #175 (`8235b2e0`, Phase 3 of six) moved those files hard:

| module | at birth `20992358` (2026-07-25) | at `origin/unstable` | Δ |
|---|---|---|---|
| `IR.hs` | 607 | **877** | **+45%** |
| `Lower.hs` | 1962 | **2895** | **+48%** |
| `Emit.hs` | 334 | 409 | |
| `Markdown.hs` | 316 | 325 | |

**The IR grew by 45% four days after it was created, under a spec that says it is superseded and
is three phases from done.** Any shape that freezes it behind a cross-repo package boundary
proposes to freeze that, now.

**The trigger is mechanical and checkable**, which is why it is worth writing down rather than
revisiting by feel:

```
git show origin/unstable:specs/todo/DMN-EXPORT-PROGRAM-MODEL-SPEC.md | head -5
```

When that no longer contains *"not yet implemented"*, the packaging question is worth reopening.
Until then, extracting an IR taxes the most active work in either repo to buy an alignment that
nothing is currently breaking — see §4.

---

## 4. Nothing is on fire, and that is load-bearing

**Does `L4.Dmn.Markdown`'s output parse in dmnmd today? Yes, on every committed golden, exit 0.**
Measured with dmnmd `db9c9ed` against `origin/unstable` fixtures:

| fixture | exit | `MEANS` emitted |
|---|---|---|
| `jl4/examples/dmn/expected/reg-cf.dmn.md` | 0 | 2 |
| `jl4/examples/dmn/expected/regcf-corpus.dmn.md` | 0 | 1 |
| `jl4/examples/dmn/expected/sumtype.dmn.md` (new in #175) | 0 | 3 |

**Not one capability is duplicated between the repos today.** The overlap is adjacency at a seam,
not contest:

| capability | dmnmd | l4-ide |
|---|---|---|
| md → IR | **yes** | no (but #173 is merged doctrine, so a second parser is now sanctioned) |
| DMN XML → IR | **yes**, XSD-shaped, strict | **no. Nothing in l4-ide reads DMN.** |
| IR → DMN XML | no (issue #13) | **yes**, engine-checked against KIE 8.44 and Camunda 8.7.6 |
| IR → dmnmd markdown | no | **yes** |
| IR → TS/JS/Py/L4 | **yes** | no |
| L4 → IR | no | **yes** |

**Every measured divergence between the two is a dmnmd-side bug in dmnmd's cell layer, and zero
of them need a line of l4-ide code to fix.** That is the strongest argument against urgency: there
is no shared-code shape that fixes any of them, and no shared-code shape that is *safe* to attempt
while they are open, because they are exactly the semantics a shared IR would have to encode.

Three of the divergences named at the time have since been fixed in dmnmd (the interval forms, the
suffix comparisons, the negative-number misparse); `VN Float` and the `label`-vs-`name` attribute
have not. See §6.

---

## 5. Neither IR is a subset. A merge would be a union.

This is the thing to know before anyone designs a shared type.

**Where l4-ide is richer** — `TestRange !Bool !FeelValue !FeelValue !Bool` expresses all four DMN
interval forms and `TestNot` expresses `not(...)`; `VNum !Rational` with **one** `renderNumber`
shared by both emitters.

**Where dmnmd is richer** — collections, N output columns, sub-header declared domains, annotation
columns, and **all seven hit policies** implemented in `evalTable`, against l4-ide's
`HitPolicy = HitUnique | HitFirst` (`IR.hs:536`, still two after #175) and singular
`dtOutput :: !OutputColumn`.

Two consequences:

- **Publishing today would commit `HitPolicy = HitUnique | HitFirst` and a singular output column
  to a public API**, and both are known-wrong against DMN §8.2.11 and §8.2.5. That alone answers
  "why not publish early to shorten the SHA-pin interval".
- dmnmd has since closed part of its own half: `FInRange` gained bracket flags (`FInRange Bound
  Float Float Bound`), so all nine S-FEEL interval spellings now parse and collapse to the correct
  four semantics. It still lacks `FNot`.

**`L4.Dmn.IR` is not DMN-generic and should not be mistaken for a DMN IR.** `IR.hs:159-160`
`L4Verbatim` — *"the text is L4 source"*; `:346` `icLabel` — *"the L4 source text of the column's
subject"*; `:316` — *"The choice is forced by `L4.Viz.GuardedRows`' `grDisjoint`"*; `:5` derived
from *"a typechecked `Module Resolved`"*. It is L4's DMN-**export** IR with a DMN-primitive core
inside it, and only that core would move. Separating them is the actual design work, and nobody
has done it.

**One coupling that surprised everyone:** `L4.Interchange.Fidelity` is imported by
`jl4-core/src/L4/Bpmn/IR.hs`, `L4/Bpmn/Lower.hs` and `jl4/tests/BpmnExport.hs`. Parameterising
`FidelityNote` over its location type changes all three. "BPMN is unaffected" is wrong.

---

## 6. Two things worth knowing regardless of the packaging decision

**One is a bug on the path we are about to recommend publicly.** The two-binary pipeline

```
dmnmd -f md -t l4 table.md > table.l4
l4 export table.l4 --to dmn --fail-on lossy -o table.dmn
```

works today — verified, 8,132 bytes of DMN 1.3 out of `reg-cf.dmn.md`, `@id` on every element,
`<informationRequirement>` wiring, a DMNDI layout block, exit 0, zero new code. It is a better
answer to `smucclaw/dmnmd#13` than the feature that issue asked for.

But **it silently downgrades hit policy**. A `| U |` markdown table lands as `hitPolicy="FIRST"`,
and `--fail-on lossy` cannot catch it, because the `U` is gone before `l4` ever sees the input:
dmnmd's L4 backend emits a first-match `BRANCH` and issues no diagnostic. That is dmnmd's own
constitution violated (*"anything we parse but do not yet honour must produce a loud, located
diagnostic"*) — a dmnmd bug, filed as such.

Further testing found it goes **both ways**, and the other direction is worse: a `| F |` table
whose guards are provably disjoint comes back as `UNIQUE`, because l4 re-derives hit policy from
its own disjointness analysis. An upgrade asserts something the author never claimed.

**The other is silent corruption in dmnmd's XML reader.** `reg-cf.dmn:189` is
`<text>max(2500, annual_limit_basis)</text>`; the XML leg emits `THEN "max(2500"`, exit 0, no
diagnostic, and `l4 check` on the result reports success. l4-ide's exporter *correctly refuses*
the same table (`[D-MD-CELLSYNTAX] blocking`). The comma-split is the same defect class as §1's
`mdValue` hazard, arriving from the opposite direction.

---

## 7. The number that decides how big this is

`reg-cf.dmn`: 5 `<decision>`, 3 `<decisionTable>`, 2 `<literalExpression>` → **2 tables in
markdown**.

`regcf-corpus.dmn`: **102 `<decision>`, 11 `<decisionTable>`, 91 `<literalExpression>`**. Its
committed markdown export is **504 bytes — one table**, with **126 blocking fidelity notes**:
91 `D-MD-NOLITERAL`, 30 `D-MD-NONIDENTCOLUMN`, 4 `D-MD-CELLSYNTAX`, 1 `D-MD-NODRG` (223
information requirements with no markdown form). Identical census before and after #175.

**dmnmd's markdown format can see 1 of 102 decisions in l4-ide's flagship formalisation, and 91 of
the 126 blockers are "this is a formula, not a table".** Boxed literal expressions are the gap,
and they are 72% of it.

So the packaging decision has a prior: **is dmnmd becoming a general DMN implementation, or staying
a markdown-table tool?** Meng has said the former is now the intent — the original scope limit was
economic (a DMN implementation once needed a Camunda-sized startup) and he judges that no longer
true. A separate workflow measured what that costs:

- **Yes, conceivable.** `DecisionToolkit/dsntk` is a Rust DMN engine, **one contributor**, first
  TCK submission 3.5 months after repo creation, scoring 99.6%.
- **But the cost is FEEL, and FEEL is ~97% of it.** Of the TCK's 3,493 compliance testCases, only
  **102** live in models containing a `<decisionTable>` — dmnmd's entire subject matter.
- **dmnmd scores 0/3,493 today, and the reason is a namespace string, not an engineering gap** —
  all 151 TCK models declare DMN 1.5 and `xmlns_dmn` is pinned to 1.3 at `ParseDMN.hs:36`. With
  only the namespaces rewritten: **45/116 Level-2 cases, 45/51 (88%) of the Level-2
  decision-table subset**.
- Calibration: the field's bar is 99–100%, and **Camunda — the engine l4-ide exports *to* — fails
  26 of 116 Level-2 cases**, three of them on space-bearing FEEL names. That is independent
  external confirmation of the `annual income` → `annual in come` defect
  `DMN-EXPORT-PROGRAM-MODEL-SPEC.md` §13.2 measured on its own.
- A caveat worth carrying into any scoreboard: **43.0% of the TCK is passed by a program that
  returns null** (1,502 of 3,492 testCases have every `resultNode` marked `errorResult` or
  `xsi:nil`, and the reference runner's comparator treats null-equals-null as a pass).
  `0100-arithmetic` alone contributes 819.

**If the conformance answer is yes, the arrow inverts on the merits rather than by amendment** —
dmnmd's IR *is* the DMN IR and `L4.Dmn.IR` is the export-shaped subset of it. If it is no, then
1-of-102 is the answer: the repos are diverging in capability, not converging, and the shared
artefact shrinks to a ~150-line `dmn-lexical` (`renderNumber`, `feelIdentText`, `quoteFeelString`,
`displayWidth`) — which is, notably, where every divergence anyone actually measured lives.

---

## 8. Recommendation

**Declare a neutral Hackage core the destination. Build none of it yet.**

Deciding it now is free and disciplines the intermediate work in exactly the useful way: keep the
core dependency-light (done — zero system deps as of `36df5a9`), keep markdown-grammar and
L4-export concerns out of the IR, keep C bindings out. The split is already almost clean on
dmnmd's side: `grepMarkdown` lives in `app/ParseMarkdown.hs`, **not in the library**, and
`XmlToDmnmd` imports `DecisionTable`, never `ParseTable`.

**For l4-ide, this week: nothing.** Specifically —

- do **not** amend `CLAUDE.md` §1.2 (it is not in the way);
- do **not** extract a package or touch `L4.Dmn.IR`;
- do add the two `INLINE-DMNMD-SPEC.md` notes in §1 above;
- do fix `Markdown.hs:189`'s unescaped `mdValue` — it is a live exit-0 wrong answer in a committed
  fixture, independent of everything else here;
- do **not** flip `RUN_XML_LEG` expecting a test: `etc/validate-dmn.mjs:214-232`'s guarded branch
  is a `console.log` of an exit code with **no comparison in the file**, so "the differential
  harness is already written" is false. Either write the comparison or delete the branch — and do
  delete the four stale blockers its header names at `:37-56`, all fixed on 2026-07-27.

**One thing that would change the answer immediately:** if someone lands a #173 inline-parser
implementation before dmnmd's cell layer is finished. That is the moment the org owns two markdown
grammars, and the second one will be mirroring the first one's bugs — `Markdown.hs:21-35` is a
hand-maintained prose model of dmnmd's parser **with no version attached**, and `Markdown.hs:154`
hardcodes a copy of dmnmd's own range regex. A mirror inherits defects rather than diverging from
them, which is strictly worse than divergence because the bug then has two homes. If that lands,
stop and decide.

---

## 9. What is verified, and what is not

Verified by execution in this session: the pcre removal and its 168/168 corpus result; the
two-binary pipeline; the hit-policy downgrade *and* upgrade; the comma split in
`regcf-corpus.dmn.md`; dmnmd's current dependency set; the TCK namespace finding and the 45/116
score; the Hackage-niche emptiness (search API queried directly, `dmn`/`dmn-core`/`feel`/
`feel-parser`/`decision-table`/`sfeel` all empty).

**Not** verified, and flagged because parts of the above lean on it:

- **Nobody built l4-ide.** Module line counts, IR field shapes and importer sets were read out of
  `origin/unstable` via `git show`, not compiled. The local checkout was two merged PRs behind.
- **The `makePrisms` WASM blocker is inherited**, not re-executed. No `wasm32-wasi-ghc` was run.
- **TCK licence is unresolved** — three different answers in that repo (`LICENSE-ASL-2.0.txt`; a
  README saying "creative commons Share-Alike-With-Attribution"; GitHub API reporting
  `license: null`). Pin a clone, do not vendor, ask before committing any `.dmn` into either repo.
- **Level-3 dmnmd behaviour was not measured**; all Level-3 statements are structural inference.
- **The dsntk 3.5-month calibration** assumes its author had no non-public prior work. The public
  record is clean but nobody asked.
