#!/usr/bin/env bash
# Performance test runner: compile with -opt, assemble, run under QEMU with timing.
KIT="$(cd "$(dirname "$0")" && pwd)"
TOP="$(cd "$KIT/.." && pwd)"
ROOT="$(cd "$TOP/.." && pwd)"
QEMU=/usr/bin/qemu-riscv32
EXE="$ROOT/_build/default/bin/main.exe"
GCC="riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 -nostdlib -static"
timeout=10
pass=0; fail=0; total=0
tmpd=$(mktemp -d)
trap "rm -rf $tmpd" EXIT

echo "=============================="
echo " ToyC Performance Test Suite"
echo "=============================="
for tc in "$KIT"/*.tc; do
  name=$(basename "$tc" .tc)
  exp=$(cat "$KIT/$name.exp")
  total=$((total+1))

  if ! "$EXE" -opt < "$tc" > "$tmpd/$name.s" 2>"$tmpd/$name.err"; then
    echo "FAIL $name: COMPILE"; fail=$((fail+1)); continue
  fi

  if ! $GCC "$TOP/start.s" "$tmpd/$name.s" -o "$tmpd/$name.elf" 2>"$tmpd/$name.ae"; then
    echo "FAIL $name: ASSEMBLE"; fail=$((fail+1)); continue
  fi

  start=$(date +%s%N)
  timeout $timeout $QEMU "$tmpd/$name.elf"; got=$?
  end=$(date +%s%N)
  elapsed=$(( (end - start) / 1000000 ))

  if [ "$got" = "$exp" ]; then
    printf "PASS  %-25s %5dms  (exp=%d)\n" "$name" "$elapsed" "$exp"
    pass=$((pass+1))
  elif [ "$got" = 124 ] || [ "$got" = 143 ]; then
    printf "TIMEOUT %-22s >%ds\n" "$name" "$timeout"
    fail=$((fail+1))
  else
    printf "FAIL  %-25s %5dms  (got=%d exp=%d)\n" "$name" "$elapsed" "$got" "$exp"
    fail=$((fail+1))
  fi
done
echo "=============================="
echo " pass=$pass fail=$fail total=$total"
echo "=============================="
