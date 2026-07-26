# `test/golden/` — DMN→L4 golden fixtures

Committed copies used by `TranslateL4Spec` (BUILD-SPEC §7.1, Option A — semantic
round-trip, **not** byte-exact).

| file | role | provenance |
|------|------|------------|
| `miles-card-dmn.md` | test **input** — two DMN decision tables (`Categorize`, `CardToUse`) | copied from `/Users/mengwong/src/mengwong/homelab/docs/miles-card-dmn.md` |
| `miles-card.l4` | hand-written **reference** output | copied from `/Users/mengwong/src/mengwong/homelab/docs/miles-card.l4` |
| `.out/` | transient — emitter output written by the test run (git-ignored) | generated |

Committed copies are used because the homelab repo is not guaranteed present in
dmnmd CI. Refresh from the canonical sources with `make sync-golden` (see the
package `Makefile`).

## Why semantic, not byte-exact (Option A)

`miles-card.l4` is the **readable** reference, not a byte-for-byte target. It
factors the multi-value groups into hand-named membership predicates
(`` `is a yuu grocer` ``, `` `is online four-mpd` ``, …) and uses `DECLARE … IS
ONE OF` enum domains plus word-form operators (`GREATER THAN`, `AT MOST`) — none
of which the mechanical emitter is required to reproduce (BUILD-SPEC §1.3, §1.5).
The emitter instead inlines `OR`-of-`EQUALS`, types category/card columns as
`STRING`, and uses symbolic operators (`>`, `<=`).

The test therefore checks **behaviour**, not text:

1. parse the two tables out of `miles-card-dmn.md`,
2. `toL4 milesOpts` each (ditto on, OR-of-EQUALS, bare `OTHERWISE` via `mkCardToUse`),
3. write the concatenation to `.out/miles-card.l4`,
4. `l4 check` it (must typecheck — catches ditto/column misalignment and type errors), and
5. `l4 run` it with the golden's behavioural `#ASSERT` block adapted to the
   emitter's STRING-world output (must report no `assertion failed`).

`l4 run` exits 0 even when an assertion fails, so the gate inspects stdout for
`assertion satisfied` / `assertion failed` rather than the process exit code.
