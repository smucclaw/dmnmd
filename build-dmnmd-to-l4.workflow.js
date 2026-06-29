// build-dmnmd-to-l4.workflow.js
//
// Claude Code Workflow that EXECUTES the BUILD-SPEC-dmnmd-to-l4.md plan:
//   Part A  dmnmd `--to=l4` backend (BRANCH + ditto)
//   Part B  l4-ide ditto codegen tweak (L4.Print.Columnar)
//   Gate    golden round-trip (Option A: semantic / AST-equivalence) wired into `stack test`, until green.
//
// Primitives assumed from the workflow runtime:
//   agent({ name, cwd, model?, allowedTools?, prompt })  -> a Step (one subagent run)
//   pipeline(...steps)   -> run steps sequentially, short-circuit on failure
//   parallel(...steps)   -> run steps concurrently, join
//   loopUntil(step, { check, maxIters })                 -> rerun step until check() passes
//
// Nothing here runs eagerly; the runner imports `meta` + the default workflow().

export const meta = {
  id: "dmnmd-to-l4",
  title: "dmnmd --to=l4 (BRANCH + ditto) + l4-ide ditto codegen",
  spec: "BUILD-SPEC-dmnmd-to-l4.md",
  repos: {
    dmnmd:  "/Users/mengwong/src/smucclaw/dmnmd/languages/haskell",
    l4ide:  "/Users/mengwong/src/legalese/l4wt/dmnmd-to-l4",
  },
  golden: {
    input:    "/Users/mengwong/src/mengwong/homelab/docs/miles-card-dmn.md",
    expected: "/Users/mengwong/src/mengwong/homelab/docs/miles-card.l4",
  },
  phases: [
    "0-preflight",
    "1-scaffold",
    "2-printer-core",
    "3-ditto",
    "4-l4ide-ditto",   // runs in parallel with phase 3
    "5-golden-wire",
    "6-validate",
    "7-finalize",
  ],
  // No commits/pushes unless the operator explicitly asks (repo policy).
  policy: { commit: false, push: false },
};

const DMNMD = meta.repos.dmnmd;
const L4IDE = meta.repos.l4ide;
const HS = { tools: ["Bash", "Read", "Edit", "Write"] };

// ---------------------------------------------------------------------------
// Phase 0 — preflight: both trees build green before we touch anything.
// ---------------------------------------------------------------------------
const preflight = parallel(
  agent({
    name: "preflight-dmnmd",
    cwd: DMNMD,
    allowedTools: HS.tools,
    prompt: `Confirm baseline. Run \`stack build\` then \`stack test\` in ${DMNMD}.
Report pass/fail and the current FileFormat constructors in app/Options.hs.
Do NOT change anything. Fail loudly if the baseline is already red.`,
  }),
  agent({
    name: "preflight-l4ide",
    cwd: L4IDE,
    allowedTools: HS.tools,
    prompt: `Confirm baseline in ${L4IDE}. Run \`cabal build all\` (GHC 9.10.2, index-state 2025-03-31).
Then \`cabal run l4 -- --help\` to confirm the l4 CLI is runnable. Report pass/fail.
Do NOT change anything.`,
  }),
);

// ---------------------------------------------------------------------------
// Phase 1 — scaffold dmnmd: enum + wiring + empty backend stub, build green.
// ---------------------------------------------------------------------------
const scaffold = agent({
  name: "scaffold-dmnmd-l4-backend",
  cwd: DMNMD,
  allowedTools: HS.tools,
  prompt: `Implement BUILD-SPEC §4.2–§4.4 (wiring only; backend may be a stub).
1. app/Options.hs: add \`L4\` to data FileFormat (line ~58); add "l4" -> return L4 in
   parseFileFormat (~67) and fix its error string; add (".l4", L4) to fileExtensionMappings (~76).
2. app/Main.hs: extend the FileFormat(..) import with L4; import DMN.Translate.L4 (toL4, L4Opts(..));
   add the L4 arm of outputTo dispatching to toL4. Define defaultL4Opts.
3. Create src/DMN/Translate/L4.hs (module DMN.Translate.L4) modeled on src/DMN/Translate/PY.hs,
   with the L4Opts record and a STUB \`toL4 :: L4Opts -> DecisionTable -> String\` that emits a
   minimal GIVEN/GIVETH + BRANCH/OTHERWISE skeleton (no ditto yet).
4. Add DMN.Translate.L4 to dmnmd.cabal library exposed-modules; keep package.yaml in sync (hpack if available).
Then \`stack build\` until it compiles. Report the diff summary.`,
});

// ---------------------------------------------------------------------------
// Phase 2 — DMN->L4 printer core (BRANCH arms, guards, values, arithmetic),
//           ditto OFF. Must produce valid, column-aligned, spelled-out L4.
// ---------------------------------------------------------------------------
const printerCore = agent({
  name: "dmnmd-l4-printer-core",
  cwd: DMNMD,
  allowedTools: HS.tools,
  prompt: `Implement the DMN->L4 mapping from BUILD-SPEC §1 and §2 in src/DMN/Translate/L4.hs,
with emitDitto = False for now (fully spelled-out arms, but column-aligned via the grid layout §3.1-3.3).
Cover:
- givenBlock + type2l4 (§1.1); GIVETH a scalar for one output column, or a DECLAREd record for
  multiple output columns via a mk<Name> constructor helper (one-line arms; inline one-line record
  literals do NOT parse -- §1.4).
- feel2l4In (§1.2): FSection ops as single-token operators; EQUALS for Feq/FNullary; FInRange ->
  two AND-ed conjuncts; FAnything -> omit conjunct (Nothing cell).
- multi-value inner lists -> OR-of-EQUALS expansion (\`field\` EQUALS v1 OR \`field\` EQUALS v2) by
  default; elem \`field\` (LIST ...) only under the useElem opt (§1.3).
- showFeelL4 / fnf2l4 for FFunction arithmetic; showValL4 + showNumL4 (9.0 -> "9").
- HP_First/HP_Unique/HP_Priority -> first-match BRANCH + synthesized OTHERWISE (§1.5);
  HP_Collect/others -> error stub (§1.6).
- row_comments -> trailing -- comments.
Use getInputHeaders/getOutputHeaders/var_name/underscore. Keep helpers self-contained (do NOT touch
FEELhelpers.hs). \`stack build\`. Add a quick TranslateL4Spec smoke test over dmn1 (Unique) and
confirm \`stack test\` builds the test module.`,
});

// ---------------------------------------------------------------------------
// Phase 3 — ditto grid in dmnmd (the column-collapse pass).
// Phase 4 — l4-ide ditto codegen helper. Independent => run in parallel.
// ---------------------------------------------------------------------------
const dmnmdDitto = agent({
  name: "dmnmd-ditto-grid",
  cwd: DMNMD,
  allowedTools: HS.tools,
  prompt: `Implement renderDittoGrid in src/DMN/Translate/L4.hs per BUILD-SPEC §3:
build [[Maybe Cell]] for the BRANCH arm block; colWidth = max cell length per column; left-align +
fixed single-space gutter so every column starts at an identical absolute source column; ditto pass
replaces cell[i][j] with ^ when it equals the Just cell directly above (transitive ok), emits spaces
for Nothing cells (copy nothing). Keep the arm block contiguous (no blank/comment-only lines inside).
Ditto guard cells only; keep IF/THEN/result literal (§3.1). Wire emitDitto = True into defaultL4Opts.
Trim trailing whitespace per line for clean output (byte-exactness is no longer the gate -- Option A
checks AST/behavioural equivalence). \`stack build\` + extend TranslateL4Spec with
an emitDitto-on vs emitDitto-off pair on a small First-hit table and assert the ditto output shape.`,
});

const l4ideDitto = agent({
  name: "l4ide-ditto-codegen",
  cwd: L4IDE,
  allowedTools: HS.tools,
  prompt: `Implement BUILD-SPEC §5 in the jl4 toolchain.
1. Create jl4-core/src/L4/Print/Columnar.hs (module L4.Print.Columnar) with the SAME ditto grid
   algorithm as §3: type Cell = Maybe Text; renderDittoGrid :: DittoOpts -> [[Cell]] -> Text.
2. Add a ditto-aware emission path for MultiWayIf (prettyLayoutDitto or equivalent) that renders the
   BRANCH header structurally and delegates the arm block to renderDittoGrid; leave the default
   prettyLayout untouched (do NOT add a ditto AST node).
3. Add L4.Print.Columnar to jl4-core/jl4-core.cabal exposed-modules. \`cabal build all\`.
4. Add a round-trip test: a hand-written .l4 with ^ -> \`cabal run l4 -- format\` preserves the ^
   (ExactPrint round-trips RealTCopy), and parse(ditto) AST == parse(spelled-out) AST.
Report files changed and the cabal build result.`,
});

const dittoPhase = parallel(dmnmdDitto, l4ideDitto);

// ---------------------------------------------------------------------------
// Phase 5 — wire the golden round-trip into `stack test`.
// ---------------------------------------------------------------------------
const goldenWire = agent({
  name: "golden-roundtrip-wire",
  cwd: DMNMD,
  allowedTools: HS.tools,
  prompt: `Wire the golden round-trip from BUILD-SPEC §7.1 (Option A: semantic, NOT byte-exact).
1. Copy the homelab golden pair into committed fixtures:
   ${meta.golden.input}  -> test/golden/miles-card-dmn.md
   ${meta.golden.expected}-> test/golden/miles-card.l4
   (add test/golden/README.md noting provenance + a Makefile \`sync-golden\` target).
2. Create test/TranslateL4Spec.hs exporting l4Spec: read the md fixture, parse with
   parseOnly (parseTable "miles per dollar"), emit toL4 milesOpts dt, write it to a temp file, and
   assert SEMANTIC equivalence to the golden -- do NOT exact-string compare. Either (a) shell out to
   \`~/.local/bin/l4 run\` on the emitted file and require all #ASSERTs to pass (emitAsserts carries
   the golden's behavioural assertions), and/or (b) parse the emitter output and the committed golden
   and compare ASTs ignoring layout / whitespace / helper naming. The golden stays the readable
   hand-authored reference; it factors groups into named predicates the emitter need not reproduce.
   milesOpts uses the resolved default convention (bare-typed OTHERWISE via the mk<Name> constructor
   helper; useElem = False; wrapMaybe = False).
3. Register l4Spec in test/Spec.hs forM_ list; add TranslateL4Spec to dmnmd.cabal test other-modules.
4. Add a Makefile \`golden-homelab\` target running the built binary against the absolute homelab
   paths, then validating the output via \`~/.local/bin/l4 check\` + \`l4 run\` (BUILD-SPEC §7.2); a
   raw diff against the golden is informational only, never the gate.
Run \`stack test\`. The golden is SEMANTICALLY authoritative: never regenerate it to match the
emitter byte-for-byte -- reconcile the emitter / L4Opts to be AST-/behaviourally-equivalent instead.`,
});

// ---------------------------------------------------------------------------
// Phase 5b — converge: iterate emitter vs golden until `stack test` is green.
// The golden is SEMANTICALLY authoritative (Option A); reconcile defaultResult/
// constructor-helper output, number/quote formatting, and ditto alignment in the
// emitter until AST-/behaviourally-equivalent — never regenerate the golden.
// ---------------------------------------------------------------------------
const converge = loopUntil(
  agent({
    name: "converge-emitter-to-golden",
    cwd: DMNMD,
    allowedTools: HS.tools,
    prompt: `Run \`stack test\`. If the miles-card golden test fails, diff toL4 output vs
test/golden/miles-card.l4 for DIAGNOSIS ONLY, then reconcile the EMITTER (src/DMN/Translate/L4.hs)
and L4Opts to be SEMANTICALLY equivalent to the golden per BUILD-SPEC §9 risks 1,5,6,8 (bare-typed
OTHERWISE via the constructor helper, showNumL4 9.0->9, backtick quoting of multi-word fields,
EQUALS-vs-OR-of-EQUALS heterogeneity). The check is AST-equivalence + \`l4 run\` of the #ASSERTs, NOT
byte-exact strings (risk 2 is moot). Never regenerate the golden. Report remaining divergence (empty
when green).`,
  }),
  { check: (out) => /0 failures|All .* tests passed|miles-card.*PASS/i.test(out), maxIters: 6 },
);

// ---------------------------------------------------------------------------
// Phase 6 — validate emitted L4 with the jl4 toolchain (typecheck + AST equiv).
// ---------------------------------------------------------------------------
const validate = agent({
  name: "validate-emitted-l4",
  cwd: L4IDE,
  allowedTools: HS.tools,
  prompt: `Validation per BUILD-SPEC §7.3 (Option A — semantic equivalence is the contract).
1. Take dmnmd's emitted miles-card.l4 (run \`stack run -- --to=l4 ${meta.golden.input}\` in ${DMNMD}).
2. \`cabal run l4 -- check\` (or \`~/.local/bin/l4 check\`) it here -> must typecheck.
3. AST-equivalence (two checks): (a) also emit the spelled-out (emitDitto=False) variant and assert
   its AST == the ditto variant's AST (catches a ^ that typechecks but copied the wrong token); and
   (b) parse BOTH the emitter output and the committed golden (${meta.golden.expected}) and assert
   their ASTs are equivalent ignoring layout / whitespace / helper naming (the golden factors groups
   into named predicates the emitter inlines).
4. \`cabal run l4 -- run\` (or \`~/.local/bin/l4 run\`) the emitted file AND the golden; the golden's
   #ASSERTs are the behavioural spec and must pass on the emitter output too. Sanity-check rows
   against DMN evalTable expectations.
Do NOT regenerate the golden to match the emitter byte-for-byte; it stays the readable, semantically
authoritative reference. Report any divergence with the exact failing row/column.`,
});

// ---------------------------------------------------------------------------
// Phase 7 — finalize: summarize; do NOT commit/push unless operator asked.
// ---------------------------------------------------------------------------
const finalize = agent({
  name: "finalize-summary",
  cwd: DMNMD,
  allowedTools: ["Bash", "Read"],
  prompt: `Summarize: files created/modified in both repos, \`stack test\` result (must be green incl.
the Option A golden gate), \`l4 check\` + AST-equivalence + \`l4 run\` #ASSERT result, and any §9 risks
still open (hit-policy fidelity; algorithm duplication; number formatting — note risks 1 (OTHERWISE)
and 2 (byte-exact) are RESOLVED). List git status in both repos. Do NOT commit or push (meta.policy).`,
});

// ---------------------------------------------------------------------------
// Body.
// ---------------------------------------------------------------------------
export default async function workflow() {
  return pipeline(
    preflight,      // 0
    scaffold,       // 1
    printerCore,    // 2
    dittoPhase,     // 3 + 4 (parallel)
    goldenWire,     // 5
    converge,       // 5b (loop to green)
    validate,       // 6
    finalize,       // 7
  );
}
