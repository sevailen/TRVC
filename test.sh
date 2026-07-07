#!/usr/bin/env bash
# ToyC 统一测试运行器
# 用法:
#   ./test.sh              运行所有测试（功能 + 性能）
#   ./test.sh perf         仅运行性能测试
#   ./test.sh p01_const    运行单个性能测试（p01_const ~ p12_const_expr_chain）
#   ./test.sh func         仅运行功能测试
#   ./test.sh list         列出所有测试

set -u
ROOT="$(cd "$(dirname "$0")" && pwd)"
EXE="$ROOT/_build/default/bin/main.exe"
QEMU=/usr/bin/qemu-riscv32
GCC="riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 -nostdlib -static"
TIMEOUT=30
PASS=0; FAIL=0

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; CYAN='\033[0;36m'; NC='\033[0m'

banner() { echo -e "${CYAN}========================================${NC}"; echo -e "${CYAN} $1${NC}"; echo -e "${CYAN}========================================${NC}"; }

run_one() {
  local tc="$1" name="$2" exp="$3"
  local tmpd=$(mktemp -d)
  "$EXE" -opt < "$tc" > "$tmpd/a.s" 2>/dev/null || { echo -e "${RED}COMPILE_ERR${NC}"; rm -rf "$tmpd"; return 1; }
  $GCC "$ROOT/testkit/start.s" "$tmpd/a.s" -o "$tmpd/a.elf" 2>/dev/null || { echo -e "${RED}ASM_ERR${NC}"; rm -rf "$tmpd"; return 1; }
  local start=$(date +%s%N)
  timeout $TIMEOUT $QEMU "$tmpd/a.elf"; local got=$?
  local end=$(date +%s%N)
  local elapsed=$(( (end - start) / 1000000 ))
  rm -rf "$tmpd"
  if [ "$got" = "$exp" ]; then echo -e "${GREEN}PASS${NC} (${elapsed}ms)"; return 0
  elif [ $got = 124 ] || [ $got = 143 ]; then echo -e "${RED}TIMEOUT${NC} (>${TIMEOUT}s)"; return 1
  else echo -e "${RED}FAIL${NC} (${elapsed}ms, got=$got exp=$exp)"; return 1; fi
}

run_perf_single() {
  local name="$1"; local tc="$ROOT/testkit/perf/${name}.tc"; local exp="$ROOT/testkit/perf/${name}.exp"
  [ -f "$tc" ] || { echo -e "${RED}未知测试: $name${NC}"; return 1; }
  printf "  %-25s " "$name"
  run_one "$tc" "$name" "$(cat "$exp")"
}

run_perf_all() {
  banner "ToyC 性能测试 (12项)"
  local total=0; local passed=0
  for tc in "$ROOT/testkit/perf"/*.tc; do
    name=$(basename "$tc" .tc); exp=$(cat "$ROOT/testkit/perf/$name.exp")
    printf "  %-25s " "$name"
    if run_one "$tc" "$name" "$exp"; then passed=$((passed+1)); fi
    total=$((total+1))
  done
  echo -e "${CYAN}----------------------------------------${NC}"
  echo -e "  性能测试: ${GREEN}$passed${NC}/$total 通过"
}

run_func_all() {
  banner "ToyC 功能测试 (33项)"
  local total=0; local passed=0; local failed_list=""
  for tc in "$ROOT/testkit/cases"/*.tc; do
    name=$(basename "$tc" .tc); exp=$(cat "$ROOT/testkit/cases/$name.exp")
    printf "  %-25s " "$name"
    if run_one "$tc" "$name" "$exp"; then passed=$((passed+1)); else failed_list="$failed_list $name"; fi
    total=$((total+1))
  done
  echo -e "${CYAN}----------------------------------------${NC}"
  echo -e "  功能测试: ${GREEN}$passed${NC}/$total 通过"
  [ -n "$failed_list" ] && echo -e "  失败:${RED}$failed_list${NC}"
}

list_tests() {
  echo "性能测试 (12项):"
  for tc in "$ROOT/testkit/perf"/*.tc; do
    n=$(basename $tc .tc); e=$(cat "$ROOT/testkit/perf/$n.exp")
    echo "  $n (exp=$e)"
  done
  echo "功能测试 (33项):"
  for tc in "$ROOT/testkit/cases"/*.tc; do
    n=$(basename $tc .tc); e=$(cat "$ROOT/testkit/cases/$n.exp")
    echo "  $n (exp=$e)"
  done
}

# 编译检查
if [ ! -f "$EXE" ]; then
  echo -e "${YELLOW}编译中...${NC}"
  (cd "$ROOT" && dune build) || { echo -e "${RED}编译失败${NC}"; exit 1; }
fi

case "${1:-all}" in
  perf|performance) run_perf_all ;;
  func|functional|cases) run_func_all ;;
  list|ls|l) list_tests ;;
  p*) run_perf_single "$1" ;;
  all|"")
    banner "ToyC 编译器测试套件"
    run_perf_all
    echo
    run_func_all
    echo -e "${CYAN}========================================${NC}"
    ;;
  *) echo "用法: $0 [perf|func|list|<测试名>]" ; exit 1 ;;
esac
