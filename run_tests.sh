#!/usr/bin/env bash
# run_tests.sh — build tinymath and test_runner, then run tests
set -euo pipefail

CC="${CC:-gcc}"
CFLAGS="-O2 -std=c99 -Wall"
LDFLAGS="-lm"

# Detect Windows/MSYS/Cygwin and use .exe suffix if appropriate
UNAME="$(uname -s 2>/dev/null || echo unknown)"
EXE_SUFFIX=""
case "$UNAME" in
  *MINGW*|*MSYS*|*CYGWIN*|*NT-*) EXE_SUFFIX=".exe" ;;
esac

TINYMATH_BIN="./tinymath${EXE_SUFFIX}"
TESTRUN_BIN="./test_runner${EXE_SUFFIX}"

echo "Using compiler: $CC"
if ! command -v "$CC" >/dev/null 2>&1; then
  echo "Error: compiler '$CC' not found on PATH." >&2
  exit 1
fi

# Build tinymath
echo "Building tinymath..."
"$CC" $CFLAGS -o "${TINYMATH_BIN}" tinymath.c $LDFLAGS
echo "Built ${TINYMATH_BIN}"

# Build test_runner
echo "Building test_runner..."
"$CC" $CFLAGS -o "${TESTRUN_BIN}" test_runner.c $LDFLAGS
echo "Built ${TESTRUN_BIN}"

# Ensure test_runner is executable
chmod +x "${TESTRUN_BIN}" || true

# Run tests
echo "Running tests..."
exec "${TESTRUN_BIN}" "${TINYMATH_BIN}"