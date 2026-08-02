#!/usr/bin/env bash
#
# run-corpus.sh — re-execute the behavioural corpus and diff against the recording.
#
# See README.md in this directory for what the corpus is and how to use it.
#
# Usage:
#   ./run-corpus.sh                    check every case
#   ./run-corpus.sh --class policy     check only the policy cases
#   ./run-corpus.sh --only 'num-*'     check cases whose slug matches a glob
#   ./run-corpus.sh --list             list cases (class, slug, title) and stop
#   ./run-corpus.sh --record           re-record expected/ for the selected cases
#   ./run-corpus.sh --verbose          also print each case's command line
#
# Exit status:
#   0  every selected policy case matched its recording
#   1  at least one policy case diverged, or a case is unrunnable
#
# Symptom divergences are reported loudly but do NOT fail the run: a symptom
# diff during the cell-language rewrite is the expected, desired outcome.

set -u

CORPUS_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd -- "$CORPUS_DIR/../.." && pwd)"          # languages/haskell
REPO_ROOT="$(cd -- "$PKG_ROOT/../.." && pwd)"           # repo root

# ---------------------------------------------------------------- options

MODE=check
FILTER_CLASS=''
FILTER_SLUG='*'
VERBOSE=0

while [ $# -gt 0 ]; do
  case "$1" in
    --record)  MODE=record ;;
    --list)    MODE=list ;;
    --check)   MODE=check ;;
    --verbose|-v) VERBOSE=1 ;;
    --class)   FILTER_CLASS="${2:-}"; shift ;;
    --class=*) FILTER_CLASS="${1#--class=}" ;;
    --only)    FILTER_SLUG="${2:-}"; shift ;;
    --only=*)  FILTER_SLUG="${1#--only=}" ;;
    -h|--help) sed -n '2,25p' "${BASH_SOURCE[0]}"; exit 0 ;;
    *) echo "run-corpus.sh: unknown argument: $1" >&2; exit 1 ;;
  esac
  shift
done

if [ -n "$FILTER_CLASS" ] && [ "$FILTER_CLASS" != symptom ] && [ "$FILTER_CLASS" != policy ]; then
  echo "run-corpus.sh: --class must be 'symptom' or 'policy', got '$FILTER_CLASS'" >&2
  exit 1
fi

# ---------------------------------------------------------------- the binary
#
# Deliberately NOT a hardcoded absolute path. Search order:
#   1. $DMNMD, if the caller set it
#   2. `cabal list-bin` — cheap, does not build
#   3. any dist-newstyle build product, newest first
#   4. dmnmd on PATH
#
# There used to be a fourth entry ahead of PATH, `stack path --local-install-root`.
# It went when the package became cabal-only.

BIN_SOURCE=''

find_binary() {
  if [ -n "${DMNMD:-}" ]; then BIN_SOURCE='$DMNMD'; printf '%s\n' "$DMNMD"; return; fi

  local b
  b="$( cd "$PKG_ROOT" && cabal list-bin exe:dmnmd 2>/dev/null )"
  if [ -n "$b" ] && [ -x "$b" ]; then BIN_SOURCE='cabal list-bin'; printf '%s\n' "$b"; return; fi

  b="$( ls -t "$PKG_ROOT"/dist-newstyle/build/*/ghc-*/dmnmd-*/x/dmnmd/build/dmnmd/dmnmd 2>/dev/null | head -1 )"
  if [ -n "$b" ] && [ -x "$b" ]; then BIN_SOURCE='dist-newstyle'; printf '%s\n' "$b"; return; fi

  b="$(command -v dmnmd 2>/dev/null)"
  if [ -n "$b" ]; then BIN_SOURCE='PATH'; printf '%s\n' "$b"; return; fi
}

# ---------------------------------------------------------------- normalisation
#
# Two GHC-internal artefacts are stripped because they are properties of the
# compiler and of package hashes, not of dmnmd:
#   - the `HasCallStack backtrace:` header
#   - the frames pointing into `libraries/…` (ghc-internal, haskeline, …),
#     which embed a build-specific package hash such as `haskeline-0.8.2.1-bd70`
# The `CallStack (from HasCallStack): error, called at src/DMN/…` lines are kept
# verbatim: those are repo-relative, meaningful, and exactly what we want to
# notice changing. Trailing whitespace is stripped so an editor cannot cause a
# spurious diff.
# The unit-id character class must include the hyphen. A cabal project has one unit
# per component, and only the library's is the bare `dmnmd-0.1.0.2-inplace` — the
# executable is `dmnmd-0.1.0.2-inplace-dmnmd` and the test-suite
# `dmnmd-0.1.0.2-inplace-dmnmd-test` (see dist-newstyle/cache/plan.json). Without the
# hyphen the class stops at the first one and those two never match. No recording
# contains one today, because the only app/ frame in the corpus is a GHC
# "Non-exhaustive patterns" message, which carries a source span but no unit id — so
# this is a latent gap, not a live failure. It is one character to close and it would
# otherwise surface as an unreproducible recording on somebody else's toolchain.
normalize() {
  sed -e '/^HasCallStack backtrace:$/d' \
      -e '\#libraries/#d' \
      -e 's/ in dmnmd-[0-9.]*-[A-Za-z0-9-]*:/ in dmnmd:/' \
      -e 's/[[:space:]]*$//'
}

# Source positions — `Foo.hs:12:34` and `Foo.hs:(12,3)-(14,5)` — move whenever an
# unrelated edit shifts a line, and a recording that goes red for that is not
# reporting a behaviour change.
#
# They are NOT stripped by normalize(): they are repo-relative, they cost nothing
# to keep, and a reader auditing a diff would rather see them than not. No line
# numbers are quoted here on purpose -- the numbers are the part that goes stale.
#
# This comment used to claim more: that the positions were load-bearing, because
# DecisionTable.mkFsAt and .mkFAt produce byte-identical text and only the
# position says which raised. That is true of the FILES and false as a
# justification, because scrub_positions below runs BEFORE the cosmetic check --
# so a swap from one wrapper to the other is already reported as cosmetic and
# already exits 0. Measured, by editing a recording to cite the other wrapper.
# The real discriminator is in test/Spec.hs, which calls mkFsAt and mkFAt by
# name. See test/corpus/README.md for the full retraction.
#
# A diff that consists of NOTHING BUT moved positions is reported as cosmetic:
# printed in full, but it does not fail the run. The alternative — a policy case
# going red every time somebody inserts a line in L4.hs — trains people to
# re-record without reading the diff, and that habit would make this whole
# directory worthless.
scrub_positions() {
  sed -e 's/\.hs:[0-9][0-9]*:[0-9][0-9]*/.hs:LINE:COL/g' \
      -e 's/\.hs:([0-9][0-9]*,[0-9][0-9]*)-([0-9][0-9]*,[0-9][0-9]*)/.hs:SPAN/g'
}

# ---------------------------------------------------------------- case running

case_dirs() {
  local d
  for d in "$CORPUS_DIR"/cases/*/*/; do
    [ -f "$d/case.conf" ] || continue
    printf '%s\n' "${d%/}"
  done | sort
}

# Populated by read_conf.
CLASS='' SLUG='' TITLE='' WHY='' SOURCE='' ARGS='' WORKDIR='' STDIN_FILE='' CASE=''

read_conf() {
  local dir="$1"
  SLUG="$(basename "$dir")"
  CLASS="$(basename "$(dirname "$dir")")"
  TITLE='' WHY='' SOURCE='' ARGS='' WORKDIR='case' STDIN_FILE=''

  # $CASE is documented as available to ARGS, so it has to be set BEFORE the conf
  # is sourced. It used to be assigned in the main loop afterwards, which meant a
  # conf using "$CASE" either killed the run under `set -u` (first case) or, worse,
  # silently expanded to the PREVIOUS case's directory and produced a green run
  # against the wrong input.
  CASE="$dir"

  # The class decides whether a divergence fails the run, and it comes from a
  # directory name, so a typo silently downgrades a policy case to a symptom that
  # can never fail. `git mv` between the two is the documented way to reclassify,
  # which makes the typo a live risk rather than a theoretical one. Note APFS is
  # case-insensitive, so `cases/Policy/` would pass here and bite on Linux CI.
  case "$CLASS" in
    policy|symptom) ;;
    *) echo "run-corpus.sh: $dir" >&2
       echo "run-corpus.sh:   sits under 'cases/$CLASS/', but the only classes are 'policy' and 'symptom'." >&2
       echo "run-corpus.sh:   Refusing to run: a case in an unrecognised class is never checked and never fails." >&2
       exit 1 ;;
  esac

  # case.conf is a shell fragment written by us and living in this repo.
  #
  # It can still fail to parse — a stray apostrophe inside a single-quoted WHY ends
  # the string early, which has happened. When that occurs bash reports the syntax
  # error and abandons the rest of the file, leaving ARGS at the '' set above; the
  # case then runs with NO arguments and its diff blames dmnmd for what is really a
  # corpus authoring mistake. Return failure instead, so the caller can mark it
  # unrunnable and say so.
  # shellcheck disable=SC1090
  . "$dir/case.conf" || return 1

  # Every case must say what to run. An empty ARGS is the signature of a conf that
  # aborted midway, and is meaningless even when deliberate.
  [ -n "$ARGS" ] || return 1

  # Validate WORKDIR HERE, not in resolve_workdir. resolve_workdir is called inside
  # a command substitution, so an `exit` there kills only the subshell: the run
  # carries on with an empty working directory and reports a regression, which is
  # loud but blames the wrong thing.
  case "$WORKDIR" in
    case|pkg|repo) ;;
    *) echo "run-corpus.sh: $CLASS/$SLUG: WORKDIR must be case|pkg|repo, got '$WORKDIR'" >&2
       exit 1 ;;
  esac
  return 0
}

selected() {
  [ -z "$FILTER_CLASS" ] || [ "$CLASS" = "$FILTER_CLASS" ] || return 1
  # shellcheck disable=SC2254
  case "$SLUG" in $FILTER_SLUG) return 0 ;; *) return 1 ;; esac
}

resolve_workdir() {
  case "$WORKDIR" in
    case) printf '%s\n' "$1" ;;
    pkg)  printf '%s\n' "$PKG_ROOT" ;;
    repo) printf '%s\n' "$REPO_ROOT" ;;
    # Unreachable: read_conf validates WORKDIR before we get here, and does it
    # outside a command substitution so that the exit actually ends the run.
    *)    echo "run-corpus.sh: internal error: unvalidated WORKDIR '$WORKDIR'" >&2
          exit 1 ;;
  esac
}

# ---------------------------------------------------------------- main

if [ "$MODE" = list ]; then
  while IFS= read -r dir; do
    read_conf "$dir" || true
    selected || continue
    printf '%-8s %-40s %s\n' "$CLASS" "$SLUG" "$TITLE"
  done < <(case_dirs)
  exit 0
fi

BIN="$(find_binary)"
if [ -z "$BIN" ] || [ ! -x "$BIN" ]; then
  cat >&2 <<'EOF'
run-corpus.sh: could not find the dmnmd binary.

Build it first:

    cd languages/haskell && cabal build

or point at one explicitly:

    DMNMD=/path/to/dmnmd test/corpus/run-corpus.sh
EOF
  exit 1
fi

echo "corpus: using $BIN"

# The PATH fallback is a trap worth shouting about. An old `cabal install`d
# dmnmd sitting in ~/.local/bin will be picked up silently, and then every
# divergence you see is against a binary from weeks ago rather than against your
# working tree. It has already happened once during development of this script.
# The other three sources are all tied to this package, so only this one
# warrants the warning.
if [ "$BIN_SOURCE" = PATH ]; then
  cat >&2 <<EOF
corpus: WARNING — no build product was found under $PKG_ROOT, so this fell back
corpus:   to whatever 'dmnmd' is on your PATH. That binary may be old, and any
corpus:   divergence below may say nothing about your working tree. Prefer:
corpus:       cd $PKG_ROOT && cabal build
EOF
fi

[ "$MODE" = record ] && echo "corpus: RECORDING — expected/ will be overwritten"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

n_total=0 n_ok=0 n_symptom_diff=0 n_policy_diff=0 n_unrunnable=0 n_cosmetic=0
symptom_diffs='' policy_diffs='' unrunnable='' cosmetic_diffs=''

while IFS= read -r dir; do
  # CLASS and SLUG are assigned before the conf is sourced, so selection still works
  # for a case whose conf is broken — which is what lets us report it rather than
  # skip it silently.
  conf_ok=1
  read_conf "$dir" || conf_ok=0
  selected || continue
  n_total=$((n_total + 1))

  if [ "$conf_ok" = 0 ]; then
    echo "  UNRUNNABLE $CLASS/$SLUG: case.conf failed to parse, or sets no ARGS"
    n_unrunnable=$((n_unrunnable + 1))
    unrunnable="$unrunnable  $CLASS/$SLUG (bad case.conf)"$'\n'
    continue
  fi

  wd="$(resolve_workdir "$dir")"
  exp="$dir/expected"

  # ARGS is eval'd so a case can quote an argument containing spaces.
  # $CASE is available to ARGS as the absolute case directory (set in read_conf).
  #
  # Clear $@ first. A failing eval leaves the PREVIOUS case's argv in place, and the
  # case then runs with the wrong flags and records the result as its own — a case
  # declaring --to=py quietly recording a TypeScript baseline, and reporting
  # "unchanged" ever after.
  set --
  if ! eval "set -- $ARGS"; then
    echo "  UNRUNNABLE $CLASS/$SLUG: could not parse ARGS: $ARGS"
    n_unrunnable=$((n_unrunnable + 1))
    unrunnable="$unrunnable  $CLASS/$SLUG (bad ARGS)"$'\n'
    continue
  fi

  # Check STDIN_FILE before doing anything else — including before --record, or
  # --record writes the bogus recording this guard exists to prevent. Bash applies
  # redirections left to right, so a missing stdin file aborts the redirect chain
  # before the stdout redirect runs, and the case silently reports whatever was left
  # in the temp file from the previous case.
  if [ -n "$STDIN_FILE" ] && [ ! -f "$dir/$STDIN_FILE" ]; then
    echo "  UNRUNNABLE $CLASS/$SLUG: STDIN_FILE='$STDIN_FILE' does not exist in the case directory"
    n_unrunnable=$((n_unrunnable + 1))
    unrunnable="$unrunnable  $CLASS/$SLUG (missing STDIN_FILE)"$'\n'
    continue
  fi

  [ "$VERBOSE" = 1 ] && echo "  [$CLASS/$SLUG] (cd $wd && dmnmd $*)"

  # Per-case temp files. Shared ones let a failed redirect hand a case the previous
  # case's output; belt and braces alongside the STDIN_FILE guard above.
  out="$TMP/$CLASS-$SLUG.out" err="$TMP/$CLASS-$SLUG.err"
  if [ -n "$STDIN_FILE" ]; then
    ( cd "$wd" && "$BIN" "$@" ) > "$out" 2> "$err" < "$dir/$STDIN_FILE"
  else
    ( cd "$wd" && "$BIN" "$@" ) > "$out" 2> "$err" < /dev/null
  fi
  status=$?

  outn="$TMP/$CLASS-$SLUG.out.n" errn="$TMP/$CLASS-$SLUG.err.n" exitn="$TMP/$CLASS-$SLUG.exit.n"
  normalize < "$out" > "$outn"
  normalize < "$err" > "$errn"
  printf '%s\n' "$status" > "$exitn"

  if [ "$MODE" = record ]; then
    mkdir -p "$exp"
    cp "$outn"  "$exp/stdout"
    cp "$errn"  "$exp/stderr"
    cp "$exitn" "$exp/exit"
    echo "  recorded  $CLASS/$SLUG (exit $status)"
    n_ok=$((n_ok + 1))
    continue
  fi

  if [ ! -f "$exp/stdout" ] || [ ! -f "$exp/stderr" ] || [ ! -f "$exp/exit" ]; then
    echo "  UNRUNNABLE $CLASS/$SLUG: no recording in $exp (run --record)"
    n_unrunnable=$((n_unrunnable + 1))
    unrunnable="$unrunnable  $CLASS/$SLUG"$'\n'
    continue
  fi

  d=''
  d="$d$(diff -u "$exp/exit"   "$exitn" --label "expected/exit"   --label "actual/exit"   2>&1)"
  d="$d$(diff -u "$exp/stdout" "$outn"  --label "expected/stdout" --label "actual/stdout" 2>&1)"
  d="$d$(diff -u "$exp/stderr" "$errn"  --label "expected/stderr" --label "actual/stderr" 2>&1)"

  if [ -z "$d" ]; then
    n_ok=$((n_ok + 1))
    continue
  fi

  # Is the whole difference just moved source positions? If so it is cosmetic:
  # show it, tell the reader to re-record, but do not call it a regression.
  ds=''
  ds="$ds$(diff -u <(scrub_positions < "$exp/stdout") <(scrub_positions < "$outn") 2>&1)"
  ds="$ds$(diff -u <(scrub_positions < "$exp/stderr") <(scrub_positions < "$errn") 2>&1)"
  ds="$ds$(diff -u "$exp/exit" "$exitn" 2>&1)"
  if [ -z "$ds" ]; then
    n_cosmetic=$((n_cosmetic + 1))
    cosmetic_diffs="$cosmetic_diffs  $CLASS/$SLUG"$'\n'
    echo
    echo "cosmetic    $CLASS/$SLUG — source positions moved, behaviour unchanged"
    echo "            $TITLE"
    printf '%s\n' "$d" | sed 's/^/            /'
    n_ok=$((n_ok + 1))
    continue
  fi

  if [ "$CLASS" = policy ]; then
    n_policy_diff=$((n_policy_diff + 1))
    policy_diffs="$policy_diffs  $SLUG"$'\n'
    echo
    echo "REGRESSION  policy/$SLUG"
    echo "            $TITLE"
  else
    n_symptom_diff=$((n_symptom_diff + 1))
    symptom_diffs="$symptom_diffs  $SLUG"$'\n'
    echo
    echo "changed     symptom/$SLUG"
    echo "            $TITLE"
  fi
  printf '%s\n' "$d" | sed 's/^/            /'
done < <(case_dirs)

echo

# Selecting nothing is an error, never a pass. A typo in --only or --class would
# otherwise run zero cases, print a reassuring all-zeros summary and exit 0 —
# which in a CI step reads exactly like success while testing nothing at all.
if [ "$n_total" -eq 0 ]; then
  echo "corpus: no case matched --class '${FILTER_CLASS:-any}' --only '$FILTER_SLUG'." >&2
  echo "corpus: selecting nothing is treated as an error. './run-corpus.sh --list' shows the slugs." >&2
  exit 1
fi

echo "corpus: $n_total case(s): $n_ok unchanged, $n_symptom_diff symptom change(s), $n_policy_diff POLICY REGRESSION(S), $n_unrunnable unrunnable"
[ "$n_cosmetic" -gt 0 ] && echo "        ($n_cosmetic of the unchanged had moved source positions — re-record at leisure)"

if [ -n "$symptom_diffs" ]; then
  echo
  echo "symptom cases whose behaviour changed (expected during the rewrite —"
  echo "review each, then re-record with --record and reclassify if now correct):"
  printf '%s' "$symptom_diffs"
fi

if [ -n "$cosmetic_diffs" ]; then
  echo
  echo "cases whose only difference was a moved source position (not a behaviour"
  echo "change; re-record whenever convenient):"
  printf '%s' "$cosmetic_diffs"
fi

if [ -n "$policy_diffs" ]; then
  echo
  echo "POLICY cases that changed. These are regressions. Do not re-record until"
  echo "you have established that the NEW behaviour is right and the recording was wrong:"
  printf '%s' "$policy_diffs"
fi

if [ -n "$unrunnable" ]; then
  echo
  echo "cases with no recording:"
  printf '%s' "$unrunnable"
fi

if [ "$n_policy_diff" -gt 0 ] || [ "$n_unrunnable" -gt 0 ]; then
  exit 1
fi
exit 0
