#!/usr/bin/env bash
#
# run-roundtrip.sh — the semantic gate on the `--to=xml` backend (DECISIONS.md D-8).
#
# The claim a hand-written expectation cannot make, and this can: dmnmd ALREADY
# READS DMN 1.3/1.4/1.5. So for every markdown fixture in the tree we can compare
#
#     F.md --to=ts                          (direct)
#     F.md --to=xml | --from=xml --to=ts    (round trip)
#
# and if those differ, either the emitter or the reader is wrong. TypeScript is
# the comparison surface rather than the XML, because the point is that the
# MEANING survived, not that the bytes did; `--to=ts` is a total function of the
# DecisionTable IR, so equal TS means equal IR.
#
# This does NOT build. `cabal build` first.
#
# Usage:
#   ./run-roundtrip.sh                 every fixture
#   ./run-roundtrip.sh --only 'num-*'  fixtures whose slug matches a glob
#   ./run-roundtrip.sh --list          list the fixtures and stop
#   ./run-roundtrip.sh --verbose       print each command and each diff in full
#   ./run-roundtrip.sh --keep DIR      keep the intermediate .dmn/.ts in DIR
#   ./run-roundtrip.sh --xsd           additionally validate each emitted
#                                      document with xmllint against xsd/DMN13.xsd
#
# Exit status:
#   0  every eligible fixture round-tripped, except those on the XFAIL list
#   1  at least one eligible fixture diverged, or an XFAIL unexpectedly passed
#
# Eligibility. A fixture is ELIGIBLE only if the DIRECT `--to=ts` run succeeds
# (exit 0). Everything else is SKIPPED and counted separately: about half the
# corpus inputs exist precisely to record a refusal, and a fixture dmnmd refuses
# to read cannot tell us anything about an emitter. Skips are printed, never
# hidden — a fixture that silently stops being eligible is how coverage rots.
#
# XFAIL. Constructs dmnmd accepts that DMN has no spelling for round-trip to a
# DIFFERENT but CORRECT document; D-11's suffix comparison is the named example.
# Those cases belong on the XFAIL list below with a reason, and the comparison
# stays strict. A weakened comparison that passes is worse than a strict one
# that fails with a known list.

set -u

RT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd -- "$RT_DIR/../.." && pwd)"              # languages/haskell
REPO_ROOT="$(cd -- "$PKG_ROOT/../.." && pwd)"           # repo root

# ---------------------------------------------------------------- options

MODE=check
FILTER_SLUG='*'
VERBOSE=0
KEEP_DIR=''
DO_XSD=0

while [ $# -gt 0 ]; do
  case "$1" in
    --list)       MODE=list ;;
    --check)      MODE=check ;;
    --verbose|-v) VERBOSE=1 ;;
    --xsd)        DO_XSD=1 ;;
    --keep)       KEEP_DIR="${2:-}"; shift ;;
    --keep=*)     KEEP_DIR="${1#--keep=}" ;;
    --only)       FILTER_SLUG="${2:-}"; shift ;;
    --only=*)     FILTER_SLUG="${1#--only=}" ;;
    -h|--help)    sed -n '2,45p' "${BASH_SOURCE[0]}"; exit 0 ;;
    *) echo "run-roundtrip.sh: unknown argument: $1" >&2; exit 1 ;;
  esac
  shift
done

# ---------------------------------------------------------------- the binary
#
# Same search order as run-corpus.sh, and the same warning when it falls back to
# PATH: a stale `cabal install`ed binary produces a green run against the wrong
# code.

if [ -n "${DMNMD:-}" ]; then
  :
elif DMNMD="$(cd "$PKG_ROOT" && cabal list-bin exe:dmnmd 2>/dev/null)" && [ -x "$DMNMD" ]; then
  :
else
  DMNMD="$(command -v dmnmd || true)"
  if [ -n "$DMNMD" ]; then
    echo "roundtrip: WARNING falling back to dmnmd on PATH: $DMNMD" >&2
    echo "roundtrip:   this may not be the code you just changed." >&2
  fi
fi

if [ -z "${DMNMD:-}" ] || [ ! -x "$DMNMD" ]; then
  echo "roundtrip: no dmnmd binary found. Run 'cabal build' first." >&2
  exit 1
fi
echo "roundtrip: using $DMNMD"

# ---------------------------------------------------------------- XSD

XSD="$PKG_ROOT/xsd/DMN13.xsd"
if [ "$DO_XSD" = 1 ]; then
  if ! command -v xmllint >/dev/null; then
    echo "roundtrip: --xsd requested but xmllint is not on PATH" >&2
    exit 1
  fi
  if [ ! -f "$XSD" ]; then
    echo "roundtrip: --xsd requested but $XSD does not exist" >&2
    exit 1
  fi
  echo "roundtrip: xsd $XSD ($(xmllint --version 2>&1 | head -1))"
fi

# ---------------------------------------------------------------- XFAIL list
#
# slug<TAB>reason. A fixture here is EXPECTED to diverge; the run fails if it
# stops diverging (re-read it, then delete the line) as loudly as if an ordinary
# fixture starts to.
#
# Ten slugs in five groups, every one written AFTER measuring the emitter.
# Eight are refusals (exit 1, with the reason on stderr) and two are documented
# renumberings; none is a loosened comparison, and each remains a strict FAIL if
# the divergence changes shape.
#
# (This paragraph said "six entries, four refusals and two renumberings" while
# the list held nine slugs and the runner reported 9 xfail. Counting slugs is
# what the reported number counts, so that is what it says now — and the number
# is why it should not have been written down without being counted.)
#
# Four of the five groups are a construct dmnmd accepts that DMN genuinely
# cannot express. The fifth, symptom/md-output-comparison-emits-lambda, is the
# reverse: DMN is right and dmnmd's markdown reader is wrong.
#
# Two things are NOT here, and their absence is the measurement:
#
#  * DECISIONS.md D-11's suffix comparison. D-11 says --to=xml "must emit the
#    mirrored form and record a fidelity note", as if the emitter could tell.
#    It cannot: DMN.ParseCell.suffixCmp mirrors `5 <=` into FSection Fgte (VN 5)
#    at PARSE time and FEELexp has no provenance slot, so by the time any
#    backend runs, `5 <=` and `>= 5` are the same value. The four
#    policy/num-suffix-* fixtures therefore round-trip CLEAN, which is the
#    empirical proof that the note has nowhere to come from. See the amended
#    D-11 entry.
#  * multi-value cells, all four interval spellings, arithmetic in an output
#    cell, and declared sub-header domains. All four were on the suspect list
#    and all four turn out to have exact DMN spellings; they pass.

# A leg that fails outright is a divergence like any other, so the XFAIL list has
# to be consulted here too and not only at the final diff. Eight of the ten
# slugs below are REFUSALS — `--to=xml` exits 1 with a located error — and
# treating those as hard failures would have left the only honest answer
# (refusing a construct DMN cannot express) permanently red.
#
# Returns 0 if the divergence was expected, having counted it.
expected_divergence() {
  local slug="$1" what="$2" reason
  if reason="$(xfail_reason "$slug")"; then
    n_xfail=$((n_xfail + 1))
    [ "$VERBOSE" = 1 ] && echo "  xfail ($what): $reason"
    return 0
  fi
  return 1
}

xfail_reason() {
  case "$1" in
    # DMN's tDecisionTable is `output+`. A markdown table with no output column
    # parses (all three of these are symptom/ fixtures, i.e. dmnmd itself calls
    # them defects) and no DMN document expresses it.
    symptom/struct-onecol-no-output|\
    symptom/struct-hitpolicy-only-table-accepted|\
    symptom/l4-zero-output-dangling-giveth|\
    policy/xml-no-output-column-refused)
      echo "no output column: DMN's tDecisionTable requires output+; refused with a located error" ;;
    # A short markdown row has no DMN spelling: tDecisionRule wants one entry per
    # column, and padding with "-" would widen the rule in silence.
    symptom/struct-short-row-truncated|\
    policy/xml-short-row-refused)
      echo "short row: DMN requires one entry per column; refused rather than padded with \"-\"" ;;
    # Markdown rule numbers are AUTHORED (gaps and repeats are meaningful and
    # reach diagnostics); DMN identifies a <rule> by position and has no field
    # for one. The emitter warns; the TS comment `// 3` comes back as `// 2`.
    symptom/struct-dash-rownum-dropped|\
    symptom/struct-midtable-continuation)
      echo "authored rule numbers are not 1..n: DMN has no field for one, so rows renumber (warned)" ;;
    # `= v` against a declared domain. Unlike the three classes above this is NOT
    # a construct DMN cannot express — the emitted document is correct DMN and
    # the value survives. It diverges because of an asymmetry inside dmnmd:
    # §9.2 rule 5 has no "=" operator so `= 9` must emit as the bare `9`, and
    # DecisionTable.domainErrors EXEMPTS a test from the declared domain while
    # CHECKING a plain value against it. So dmnmd refuses the document it just
    # wrote. Warned on emission by eqDomainWarns, which uses the same fEval
    # membership test, so the warning cannot drift from the refusal.
    policy/xml-eq-test-domain-warned)
      echo "\`= v\` outside a declared domain: DMN has no \"=\" so it emits as a bare value, which dmnmd's own domain check then refuses (warned)" ;;
    # A unary test in an OUTPUT cell. DMN's <outputEntry> is a literal
    # expression — a value — and §9.2 admits a test only in an <inputEntry>
    # (rule 12 vs rule 3), so `--to=xml` refuses it with a located error. That
    # refusal is CORRECT and is the point of the fixture: ts/js/py accept the
    # same cell at exit 0 and emit an arrow function as the output value, which
    # JSON.stringify drops. So the two backends disagree about the same cell,
    # and the XML one is right. The divergence goes away when the markdown
    # reader refuses it too — which is what progress on this symptom means.
    symptom/md-output-comparison-emits-lambda)
      echo "unary test in an output cell: DMN's <outputEntry> is a value, not a test; refused with a located error (ts/js/py wrongly accept it — see the case's WHY)" ;;
    *) return 1 ;;
  esac
}

# ---------------------------------------------------------------- fixtures
#
# Every .md that is or contains a decision-table fixture:
#   - the root README.md, which is the spec AND a live fixture
#   - test/safe.md, test/golden/*.md, test/examples/**.md
#   - every corpus case input.md (161 of them, one per verified behaviour)
# Deliberately NOT the design documents at the repo root (CLAUDE.md, DECISIONS.md,
# BUILD-SPEC-*.md): their pipe tables are prose, and dmnmd says so on stderr.

collect_fixtures() {
  echo "$REPO_ROOT/README.md"
  find "$PKG_ROOT/test" -name '*.md' ! -name 'README.md' -print | sort
  find "$PKG_ROOT/test/corpus/cases" -name 'input.md' -print | sort
  # test/golden/README.md is itself a fixture: prose-only, exits 0, no tables.
  echo "$PKG_ROOT/test/golden/README.md"
}

slug_for() {
  local f="$1" rel
  rel="${f#$REPO_ROOT/}"
  rel="${rel#languages/haskell/}"
  rel="${rel#test/}"
  rel="${rel#corpus/cases/}"
  rel="${rel%/input.md}"
  echo "$rel"
}

FIXTURES=()
while IFS= read -r f; do
  [ -f "$f" ] || continue
  s="$(slug_for "$f")"
  # shellcheck disable=SC2254
  case "$s" in $FILTER_SLUG) FIXTURES+=("$f") ;; esac
done < <(collect_fixtures | awk '!seen[$0]++')

if [ "${#FIXTURES[@]}" -eq 0 ]; then
  echo "roundtrip: --only '$FILTER_SLUG' matched no fixture. Refusing to report success." >&2
  exit 1
fi

if [ "$MODE" = list ]; then
  for f in "${FIXTURES[@]}"; do printf '%s\t%s\n' "$(slug_for "$f")" "$f"; done
  echo "roundtrip: ${#FIXTURES[@]} fixture(s)"
  exit 0
fi

# ---------------------------------------------------------------- run

if [ -n "$KEEP_DIR" ]; then
  mkdir -p "$KEEP_DIR"
  WORK="$KEEP_DIR"
else
  WORK="$(mktemp -d "${TMPDIR:-/tmp}/dmnmd-roundtrip.XXXXXX")"
  trap 'rm -rf "$WORK"' EXIT
fi

n_total=0; n_pass=0; n_fail=0; n_skip=0; n_xfail=0; n_xpass=0; n_xsdbad=0
FAILED=(); SKIPPED=(); XPASSED=()

for f in "${FIXTURES[@]}"; do
  n_total=$((n_total + 1))
  slug="$(slug_for "$f")"
  safe="${slug//\//__}"; safe="${safe// /_}"
  d="$WORK/$safe"; mkdir -p "$d"

  [ "$VERBOSE" = 1 ] && echo "--- $slug"

  # 1. direct: is this fixture eligible at all?
  "$DMNMD" --to=ts "$f" >"$d/direct.ts" 2>"$d/direct.err"; rc_direct=$?
  if [ "$rc_direct" -ne 0 ]; then
    n_skip=$((n_skip + 1))
    SKIPPED+=("$slug	direct --to=ts exited $rc_direct (fixture records a refusal)")
    continue
  fi
  if [ ! -s "$d/direct.ts" ]; then
    n_skip=$((n_skip + 1))
    SKIPPED+=("$slug	direct --to=ts emitted nothing (no decision table)")
    continue
  fi

  # 2. emit XML
  "$DMNMD" --to=xml -o "$d/out.dmn" "$f" >"$d/xml.out" 2>"$d/xml.err"; rc_xml=$?
  if [ "$rc_xml" -ne 0 ]; then
    if expected_divergence "$slug" "--to=xml refused"; then continue; fi
    n_fail=$((n_fail + 1))
    FAILED+=("$slug	--to=xml exited $rc_xml: $(head -1 "$d/xml.err")")
    continue
  fi

  # 3. optional XSD check. A document that fails the schema is a failure even if
  #    it round-trips: dmnmd's reader is more permissive than the XSD in places
  #    (test/dmn13/bad-rule-arity.dmn XSD-validates and the reader refuses it),
  #    so neither check subsumes the other.
  if [ "$DO_XSD" = 1 ]; then
    if ! xmllint --noout --schema "$XSD" "$d/out.dmn" >"$d/xsd.out" 2>&1; then
      n_xsdbad=$((n_xsdbad + 1))
      FAILED+=("$slug	emitted document fails XSD: $(head -1 "$d/xsd.out")")
      [ "$VERBOSE" = 1 ] && sed 's/^/    /' "$d/xsd.out"
      continue
    fi
  fi

  # 4. read it back and re-emit TS
  "$DMNMD" --from=xml --to=ts "$d/out.dmn" >"$d/rt.ts" 2>"$d/rt.err"; rc_rt=$?
  if [ "$rc_rt" -ne 0 ]; then
    if expected_divergence "$slug" "read-back refused"; then continue; fi
    n_fail=$((n_fail + 1))
    FAILED+=("$slug	--from=xml --to=ts exited $rc_rt: $(head -1 "$d/rt.err")")
    continue
  fi

  # 4b. second comparison surface: L4.
  #
  # TS ALONE IS NOT ENOUGH, and this was measured, not assumed. `--to=ts`
  # collapses every hit policy into two classes: {U,P,F} emit `else if` chains
  # and {A,O,R,C,C+,C<,C>,C#} emit independent `if`s, so a TS-equal round trip
  # cannot see an emitter that writes ANY where the source said RULE ORDER.
  # L4 separates O, R and C from the rest — though it still collapses C, C+,
  # C<, C> and C# onto one another, because it refuses all five with the same
  # message. That residual hole (the COLLECT aggregation attribute) is NOT
  # covered here and needs its own targeted test.
  #
  # L4 legitimately refuses tables the other backends accept (list-valued hit
  # policies), so a direct L4 failure means "not a surface for this fixture",
  # not a failure.
  l4_note=''
  if "$DMNMD" --to=l4 "$f" >"$d/direct.l4" 2>"$d/direct.l4.err"; then
    if "$DMNMD" --from=xml --to=l4 "$d/out.dmn" >"$d/rt.l4" 2>"$d/rt.l4.err"; then
      diff -u "$d/direct.l4" "$d/rt.l4" >"$d/diff.l4" 2>&1 || l4_note='; L4 also differs'
    else
      l4_note='; --from=xml --to=l4 failed on the emitted document'
    fi
  fi

  # 5. compare
  if [ -z "$l4_note" ] && diff -u "$d/direct.ts" "$d/rt.ts" >"$d/diff" 2>&1; then
    if xfail_reason "$slug" >/dev/null; then
      n_xpass=$((n_xpass + 1))
      XPASSED+=("$slug	on the XFAIL list but round-tripped clean — re-read it and remove the line")
    else
      n_pass=$((n_pass + 1))
    fi
  else
    if reason="$(xfail_reason "$slug")"; then
      n_xfail=$((n_xfail + 1))
      [ "$VERBOSE" = 1 ] && echo "  xfail: $reason"
    else
      n_fail=$((n_fail + 1))
      FAILED+=("$slug	differs after round trip ($(grep -c '^[+-]' "$d/diff" 2>/dev/null || echo 0) changed TS line(s))$l4_note")
      [ "$VERBOSE" = 1 ] && { sed 's/^/    /' "$d/diff" 2>/dev/null; sed 's/^/  L4  /' "$d/diff.l4" 2>/dev/null; }
    fi
  fi
done

# ---------------------------------------------------------------- report

echo
if [ "${#SKIPPED[@]}" -gt 0 ]; then
  echo "roundtrip: skipped (not eligible — direct --to=ts does not succeed):"
  printf '  %s\n' "${SKIPPED[@]}"
  echo
fi
if [ "${#FAILED[@]}" -gt 0 ]; then
  echo "roundtrip: FAILURES:"
  printf '  %s\n' "${FAILED[@]}"
  echo
fi
if [ "${#XPASSED[@]}" -gt 0 ]; then
  echo "roundtrip: UNEXPECTED PASSES:"
  printf '  %s\n' "${XPASSED[@]}"
  echo
fi

[ -n "$KEEP_DIR" ] && echo "roundtrip: intermediates kept in $KEEP_DIR"

echo "roundtrip: $n_total fixture(s): $n_pass pass, $n_fail FAIL, $n_xfail xfail, $n_xpass xpass, $n_skip skipped$([ "$DO_XSD" = 1 ] && echo ", $n_xsdbad XSD-invalid")"

# n_xsdbad is part of the gate. It was not, and the omission made --xsd a
# reporting flag rather than a check: an XSD-invalid document printed under
# FAILURES and the script still exited 0, so a future commit could break
# validity without failing anything. Verified by stubbing xmllint to `false` —
# "1 XSD-invalid" printed, exit status 0.
if [ "$n_fail" -gt 0 ] || [ "$n_xpass" -gt 0 ] || [ "$n_xsdbad" -gt 0 ]; then exit 1; fi
exit 0
