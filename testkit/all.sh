#!/usr/bin/env bash
# Run all cases/*.tc, compile+assemble+qemu, compare to .exp. Usage: all.sh [-opt]
KIT="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$KIT/.." && pwd)"
QEMU=/usr/bin/qemu-riscv32
EXE="$ROOT/_build/default/bin/main.exe"
GCC="riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 -nostdlib -static"
opt="${1:-}"
pass=0; fail=0
for tc in "$KIT"/cases/*.tc; do
  name=$(basename "$tc" .tc)
  exp=$(cat "$KIT/cases/$name.exp")
  if ! "$EXE" $opt < "$tc" > "$KIT/o.s" 2>"$KIT/e.txt"; then
    echo "FAIL $name: COMPILE $(cat "$KIT/e.txt")"; fail=$((fail+1)); continue
  fi
  if ! $GCC "$KIT/start.s" "$KIT/o.s" -o "$KIT/o.elf" 2>"$KIT/ae.txt"; then
    echo "FAIL $name: ASSEMBLE $(head -1 "$KIT/ae.txt")"; fail=$((fail+1)); continue
  fi
  timeout 10 $QEMU "$KIT/o.elf"; got=$?
  if [ "$got" = "$exp" ]; then
    echo "PASS $name ($got)"; pass=$((pass+1))
  else
    echo "FAIL $name: got=$got exp=$exp"; fail=$((fail+1))
  fi
done
echo "==== pass=$pass fail=$fail ===="
