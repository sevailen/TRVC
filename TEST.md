# ToyC 测试套件使用指南

## 快速开始

```bash
# 编译编译器（首次使用前）
dune build

# 运行全部 45 项测试
./test.sh

# 仅运行 12 项性能测试
./test.sh perf

# 仅运行 33 项功能测试
./test.sh func
```

## 命令参考

### `./test.sh [子命令]`

| 命令 | 作用 |
|------|------|
| `./test.sh` | 运行全部测试（功能 + 性能） |
| `./test.sh perf` | 仅运行 12 项性能测试 |
| `./test.sh func` | 仅运行 33 项功能测试 |
| `./test.sh p01_const` | 运行单个性能测试（支持 p01–p12） |
| `./test.sh list` | 列出所有测试及期望值 |

### 单测输出格式

```
  p06_tail_recursion       PASS (3ms)
  p09_advanced_graph       FAIL (523ms, got=42 exp=183)
  p10_advanced_matrix      TIMEOUT (>30s)
```

每项测试显示：测试名、结果（PASS/FAIL/TIMEOUT）、耗时、失败时显示实际与期望值。

## 性能测试一览（12 项）

| 测试 | 目标优化 | 循环规模 |
|------|---------|---------|
| `p01_const` | 常量传播/折叠 | 1000 次 |
| `p02_dead_code` | 死代码消除（DCE） | 1000 次 |
| `p03_copy` | 副本传播（Copy Propagation） | 1000 次 |
| `p04_common_subexpr` | 公共子表达式消除（CSE） | 1000 次 |
| `p05_algebra` | 代数化简 | 1000 次 |
| `p06_tail_recursion` | 尾调用优化（TCO） | 深度递归 |
| `p07_loop` | 循环优化（LICM） | 40×40×40 嵌套循环 |
| `p08_basic_combined` | 组合优化（迭代不动点） | 200×100 嵌套循环 |
| `p09_advanced_graph` | 寄存器分配 / CSE | 200 次 |
| `p10_advanced_matrix` | 寄存器分配 / 表达式化简 | 500 次长链表达式 |
| `p11_global_const_prop` | 全局常量传播 | 1000 次跨函数调用 |
| `p12_const_expr_chain` | 编译时常量求值 | 500 次 |

## 功能测试（33 项）

涵盖基础语法、控制流、函数调用、短路求值、数组等。不设超时限制。

## 测试流程

```
ToyC 源码 (*.tc)
  → main.exe -opt     (编译为 RISC-V 汇编)
  → riscv64-*-gcc -nostdlib -static + start.s
  → qemu-riscv32      (运行并获取退出码)
  → 对比 .exp 期望值
```

依赖工具：`riscv64-unknown-elf-gcc`、`qemu-riscv32`、`dune`。

## 手动运行单个测试

```bash
# 编译测试
_build/default/bin/main.exe -opt < testkit/perf/p01_const.tc > /tmp/out.s

# 汇编链接
riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 \
  -nostdlib -static testkit/start.s /tmp/out.s -o /tmp/out.elf

# 运行
qemu-riscv32 /tmp/out.elf
echo $?    # 显示退出码
```

## 文件结构

```
testkit/
├── start.s          # RISC-V 启动代码（QEMU user-mode）
├── cases/           # 33 项功能测试
│   ├── f01.tc       # 功能测试
│   ├── w1.tc        # while 循环测试
│   ├── x1.tc        # 复杂表达式测试
│   └── *.exp        # 期望退出码
└── perf/            # 12 项性能测试
    ├── p01_const.tc ~ p12_const_expr_chain.tc
    └── *.exp        # 期望退出码
test.sh               # 统一测试运行器
TEST.md                # 本文件
```
