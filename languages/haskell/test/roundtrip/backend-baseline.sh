#!/usr/bin/env bash
#
# backend-baseline.sh — record, or check, exactly what every EXISTING backend
# emits for every fixture, so the `--to=xml` work can prove it changed nothing
# else.
#
# The corpus already pins ~192 hand-picked behaviours, but it pins the ones
# somebody thought to record. This is the blunt instrument that complements it:
# every fixture × every implemented output format, byte for byte, plus stderr
# and exit status. Adding a FileFormat constructor and an outputTo clause is
# exactly the kind of edit that can perturb an unrelated format's dispatch, and
# this is what would notice.
#
# Formats. `ts js py l4` and no more. There is NO json backend in dmnmd:
# `showToJSON` in app/Main.hs is despite its name the interactive `-q` REPL's
# result printer, not an output format, and it has no FileFormat constructor.
# `md` and `xml` are FileFormat constructors with no implementation, so they are
# excluded here and are the subject of the work itself.
#
# Usage:
#   ./backend-baseline.sh --record [DIR]   write the baseline (default DIR below)
#   ./backend-baseline.sh --check  [DIR]   re-run and diff against it
#
# Exit status: 0 if every recording matched (or on --record), 1 otherwise.

set -u

RT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd -- "$RT_DIR/../.." && pwd)"
REPO_ROOT="$(cd -- "$PKG_ROOT/../.." && pwd)"

MODE=''
BASE_DIR=''
while [ $# -gt 0 ]; do
  case "$1" in
    --record) MODE=record ;;
    --check)  MODE=check ;;
    -h|--help) sed -n '2,25p' "${BASH_SOURCE[0]}"; exit 0 ;;
    -*) echo "backend-baseline.sh: unknown argument: $1" >&2; exit 1 ;;
    *) BASE_DIR="$1" ;;
  esac
  shift
done
[ -n "$MODE" ] || { echo "backend-baseline.sh: pass --record or --check" >&2; exit 1; }
BASE_DIR="${BASE_DIR:-$RT_DIR/baseline}"

if [ -n "${DMNMD:-}" ]; then
  :
elif DMNMD="$(cd "$PKG_ROOT" && cabal list-bin exe:dmnmd 2>/dev/null)" && [ -x "$DMNMD" ]; then
  :
else
  DMNMD="$(command -v dmnmd || true)"
  [ -n "$DMNMD" ] && echo "backend-baseline: WARNING falling back to dmnmd on PATH: $DMNMD" >&2
fi
[ -n "${DMNMD:-}" ] && [ -x "$DMNMD" ] || { echo "backend-baseline: no dmnmd binary. cabal build first." >&2; exit 1; }
echo "backend-baseline: using $DMNMD"

FORMATS="ts js py l4"

collect_fixtures() {
  echo "$REPO_ROOT/README.md"
  find "$PKG_ROOT/test" -name '*.md' ! -name 'README.md' -print | sort
  find "$PKG_ROOT/test/corpus/cases" -name 'input.md' -print | sort
  echo "$PKG_ROOT/test/golden/README.md"
  # the XML reader's own fixtures, exercised through every writer
  find "$PKG_ROOT/test" -name '*.dmn' -print | sort
}

slug_for() {
  local rel="${1#$REPO_ROOT/}"
  rel="${rel#languages/haskell/}"; rel="${rel#test/}"
  rel="${rel#corpus/cases/}"; rel="${rel%/input.md}"
  echo "$rel"
}

OUT="$BASE_DIR"
if [ "$MODE" = record ]; then rm -rf "$OUT"; mkdir -p "$OUT"; else
  [ -d "$OUT" ] || { echo "backend-baseline: no baseline at $OUT; --record first" >&2; exit 1; }
  WORK="$(mktemp -d "${TMPDIR:-/tmp}/dmnmd-baseline.XXXXXX")"; trap 'rm -rf "$WORK"' EXIT
fi

n=0; bad=0
while IFS= read -r f; do
  [ -f "$f" ] || continue
  slug="$(slug_for "$f")"; safe="${slug//\//__}"; safe="${safe// /_}"
  for fmt in $FORMATS; do
    n=$((n + 1))
    dest="${OUT}/${safe}.${fmt}"
    [ "$MODE" = check ] && dest="${WORK}/${safe}.${fmt}"
    {
      "$DMNMD" --to="$fmt" "$f" 2>"${dest}.err"
      echo "### exit $?"
    } >"$dest"
    # stderr can name the fixture's absolute path; normalise it out so the
    # baseline is comparable across checkouts and worktrees.
    sed -i.bak "s|$REPO_ROOT|<REPO>|g" "${dest}.err" && rm -f "${dest}.err.bak"
    if [ "$MODE" = check ]; then
      for suffix in '' '.err'; do
        # MANIFEST.sha is the authoritative record and is the part committed to
        # git; the full outputs are bulky and gitignored. So compare checksums
        # when the full file is absent, and show a real diff when it is there.
        if [ -f "${OUT}/${safe}.${fmt}${suffix}" ]; then
          diff -q "${OUT}/${safe}.${fmt}${suffix}" "${dest}${suffix}" >/dev/null 2>&1 && continue
          bad=$((bad + 1))
          echo "CHANGED: ${slug} --to=${fmt}${suffix:+ (stderr)}"
          diff -u "${OUT}/${safe}.${fmt}${suffix}" "${dest}${suffix}" | sed 's/^/    /' | head -30
        else
          want="$(awk -v k="./${safe}.${fmt}${suffix}" '$2==k{print $1}' "$OUT/MANIFEST.sha")"
          got="$(shasum < "${dest}${suffix}" | cut -d' ' -f1)"
          if [ -z "$want" ]; then
            bad=$((bad + 1)); echo "MISSING FROM MANIFEST: ${slug} --to=${fmt}${suffix:+ (stderr)}"
          elif [ "$want" != "$got" ]; then
            bad=$((bad + 1)); echo "CHANGED (sha): ${slug} --to=${fmt}${suffix:+ (stderr)}"
          fi
        fi
      done
    fi
  done
done < <(collect_fixtures | awk '!seen[$0]++')

if [ "$MODE" = record ]; then
  ( cd "$OUT" && find . -type f | sort | xargs shasum > /tmp/.bl.$$ && mv /tmp/.bl.$$ MANIFEST.sha ) 2>/dev/null
  echo "backend-baseline: recorded $n run(s) into $OUT"
else
  echo "backend-baseline: checked $n run(s): $bad changed"
  [ "$bad" -gt 0 ] && exit 1
fi
exit 0
