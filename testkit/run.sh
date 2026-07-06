#!/usr/bin/env bash
# End-to-end test harness: compile ToyC -> assemble rv32 -> run under qemu -> print exit code.
# Usage: run.sh <file.tc> [-opt]
set -u
KIT="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$KIT/.." && pwd)"
QEMU=/tmp/qroot/usr/bin/qemu-riscv32-static
EXE="$ROOT/_build/default/bin/main.exe"
GCC="riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 -nostdlib -static"

f="$1"; shift || true
optflag="${1:-}"
"$EXE" $optflag < "$f" > "$KIT/out.s" 2>"$KIT/err.txt" || { echo "COMPILE_FAIL: $(cat "$KIT/err.txt")"; exit 2; }
$GCC "$KIT/start.s" "$KIT/out.s" -o "$KIT/a.elf" 2>"$KIT/aserr.txt" || { echo "ASSEMBLE_FAIL:"; cat "$KIT/aserr.txt"; exit 3; }
timeout 10 $QEMU "$KIT/a.elf"; echo "exit=$?"
