# ToyC 优化编译器实践报告

## 一、项目概述

ToyC 是一个将 SimPL 语言编译为 RISC-V 汇编的优化编译器，使用 OCaml 语言开发，基于 Dune 构建系统。

### 语言特性
- 整数与布尔类型
- 变量绑定（let）
- 条件分支（if-then-else）
- 一等函数与闭包
- 函数调用

### 编译器管道
```
源代码 → 词法分析 → 语法分析 → AST → DAG → CFG(优化) → Low IR → RISC-V汇编
```

## 二、构建与运行

### 环境要求
- OCaml 5.x
- Dune 3.x
- Menhir（语法分析器生成器）

### 构建命令
```bash
dune build
```

### 运行命令
```bash
dune exec starter6_opt -- <输入文件> <输出文件>
# 示例
echo 'let a = 10 in let f = fun x -> x + a in f 3' > test.in
dune exec starter6_opt -- test.in output.s
```

## 三、模块设计

| 模块 | 文件 | 功能 |
|------|------|------|
| 词法分析 | [lexer.mll](lib/lexer.mll) | 基于 ocamllex 的词法分析器 |
| 语法分析 | [parser.mly](lib/parser.mly) | 基于 Menhir 的 LR 语法分析器 |
| AST | [ast.ml](lib/ast.ml) | 抽象语法树类型定义 |
| DAG | [dag.ml](lib/dag.ml) | 有向无环图 IR，支持 hash-consing 和常量折叠 |
| CFG | [cfg.ml](lib/cfg.ml) | 控制流图 IR，AST→CFG 转换，优化 Pass |
| IR | [ir.ml](lib/ir.ml) | 低级中间表示，指令选择，寄存器分配 |
| 代码生成 | [codegen.ml](lib/codegen.ml) | RISC-V 汇编发射，栈帧管理，溢出处理 |
| SSA | [ssa.ml](lib/ssa.ml) | SSA 形式优化 |
| 语义分析 | [semant.ml](lib/semant.ml) | 语义检查 |

## 四、核心优化技术

### 4.1 Hash-Consing（DAG 层）
通过结构哈希实现公共子表达式消除（CSE）。相同结构的子表达式共享同一节点，避免重复计算。

### 4.2 常量折叠
在编译期对常量表达式求值，如 `3 + 5` 直接替换为 `8`。

### 4.3 拷贝传播
追踪寄存器拷贝链，将对副本的引用替换为原始来源，配合死代码删除消除冗余拷贝。

### 4.4 常量传播
前向分析维护寄存器常量映射，在编译期替换常量引用并简化分支。

### 4.5 死代码删除
反向活跃性分析，从基本块终止符出发标记活跃寄存器，删除对不被使用的寄存器的定义。

### 4.6 SSA 优化
将 CFG 转换为静态单赋值形式进行优化，消除无用指令。

### 4.7 寄存器分配
贪心线性扫描算法，将虚拟寄存器映射到 RISC-V 物理寄存器（13 个可分配寄存器），超出时使用栈溢出。

## 五、测试用例

| 测试文件 | 说明 | 预期结果 |
|----------|------|----------|
| simple.in | 简单算术表达式 | 正确计算 |
| simple_if.in | 条件分支 | 正确分支 |
| cse_test.in | 公共子表达式消除测试 | 优化后指令数减少 |
| hoist_test.in | 循环不变量外提测试 | 优化后循环体精简 |

## 六、总结与收获

本项目实现了一个完整的编译器后端，从 AST 到 RISC-V 汇编的完整翻译过程。通过本次实践，深入理解了：

1. **中间表示设计**：DAG 和 CFG 的分层设计，DAG 处理纯表达式优化，CFG 处理控制流和闭包
2. **数据流分析**：活跃性分析、常量传播、拷贝传播等经典编译器优化技术
3. **闭包实现**：堆分配闭包结构体（代码指针 + 捕获变量）
4. **寄存器分配**：贪心线性扫描的工程实现
5. **代码生成**：RISC-V 汇编发射，栈帧管理和调用约定

## 七、参考文献

- Andrew W. Appel. *Modern Compiler Implementation in ML*. Cambridge University Press, 1998.
- Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman. *Compilers: Principles, Techniques, and Tools* (2nd Edition). Addison-Wesley, 2006.
- RISC-V Assembly Programmer's Manual
- OCaml Dune Build System Documentation
