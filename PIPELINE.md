# 优化编译器后端 — 实现文档

## 管道概览

```
Source Code (SimPL)
  │
  ▼ Parser (lexer.mll + parser.mly)
  AST (ast.ml)
  │
  ▼ CG — Code Generation (dag.ml)
  DAG (Directed Acyclic Graph)
  │
  ▼ CFG Builder (cfg.ml)
  CFG (Control Flow Graph + TAC)
  │
  ▼ Optimizer (cfg.ml)
  CFG' (optimized)
  │
  ▼ TCG — Target Code Generation (ir.ml)
  Low IR (RISC-V-like instructions)
  │
  ▼ Codegen (codegen.ml)
  RISC-V Assembly
```

## 项目结构

```
starter6_opt/
├── dune-project
├── PIPELINE.md          ← 本文档
├── lib/
│   ├── dune
│   ├── ast.ml            AST 类型定义
│   ├── lexer.mll         ocamllex 词法分析器
│   ├── parser.mly        Menhir 语法分析器
│   ├── util.ml           计数器/标签生成器
│   ├── dag.ml            DAG IR + hash-consing + 常量折叠
│   ├── cfg.ml            CFG IR + AST→CFG + 优化 Passes
│   ├── ir.ml             Low IR + CFG→IR + 寄存器分配
│   └── codegen.ml        汇编发射 + 序言/尾声 + 溢出处理
├── bin/
│   ├── dune
│   └── main.ml           管道编排 + CLI
└── test.in               测试用例
```

---

## 阶段 1：Parser → AST

### 输入

```
let a = 10 in let f = fun x -> x + a in f 3
```

### 输出：AST

```ocaml
Let("a", Int 10,
  Let("f", Func("x", Binop(Add, Var "x", Var "a")),
    App(Var "f", Int 3)))
```

### AST 类型定义（ast.ml）

```ocaml
type binop = Add | Sub | Mul | Div | Lqu

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Func of string * expr
  | App of expr * expr
  | Binop of binop * expr * expr
```

---

## 阶段 2：AST → DAG（dag.ml）

### 设计思路

DAG 只处理**纯表达式**（Int、Bool、Var、Binop），不涉及控制流和变量绑定。核心机制是 **hash-consing**：结构相同的子表达式共享同一节点，天然实现公共子表达式消除（CSE）。

### DAG 节点类型

```ocaml
type id = int

type node =
  | DConst of int           (* 整数常量 *)
  | DBool of bool           (* 布尔常量 *)
  | DVar of string          (* 变量引用 *)
  | DBinop of binop * id * id (* 二元运算 *)
```

### Hash-Consing 算法

```ocaml
(* 结构键：去除了节点 ID，只保留结构信息 *)
type node_key =
  | KConst of int
  | KBool of bool
  | KVar of string
  | KBinop of binop * id * id

let intern (dag : t) (n : node) : id =
  let key = key_of_node n in
  match Hashtbl.find_opt dag.hashcons key with
  | Some existing -> existing   (* CSE 命中：复用已有节点 *)
  | None ->
    let fresh = dag.count in     (* 新节点：分配 ID *)
    dag.nodes.(fresh) <- n;
    Hashtbl.add dag.hashcons key fresh;
    fresh
```

示例：`(x + 1) * (x + 1)` 中两个 `x + 1` 产生相同的 `KBinop(Add, id_x, id_1)` 键，第二个直接复用第一个的 ID。

### 常量折叠

遍历 DAG，将常量操作在编译期求值：

```
DBinop(Add, DConst 3, DConst 5)
  → eval_const_binop Add 3 5 = 8
  → DConst 8
```

```ocaml
let eval_const_binop (op : binop) (a : int) (b : int) : int =
  match op with
  | Add -> a + b | Sub -> a - b | Mul -> a * b
  | Div -> if b = 0 then failwith "Division by zero" else a / b
  | Lqu -> if a <= b then 1 else 0
```

### Flatten：DAG → TAC

后序遍历 + 记忆化，每个节点最多生成一条 TAC 指令：

```ocaml
let flatten (dag : t) (env : (string * int) list) (root : id)
  : tac list * int =
  let memo = Hashtbl.create 64 in
  let rec go node_id =
    match Hashtbl.find_opt memo node_id with
    | Some reg -> ([], reg)   (* 已计算，直接复用结果寄存器 *)
    | None ->
      match get dag node_id with
      | DConst n -> (new_reg, [TConst(r, n)])
      | DVar x -> lookup env x   (* 变量在环境中 -> 不生成指令 *)
      | DBinop (op, l, r) ->
          let (li, lr) = go l in  (* 递归处理左子树 *)
          let (ri, rr) = go r in  (* 递归处理右子树 *)
          (li @ ri @ [TBinop(r', op, lr, rr)], r')
  in go root
```

### 自由变量分析

遍历 DAG 子图，收集所有 `DVar` 节点，过滤掉绑定变量：

```ocaml
let free_vars_of_dag (dag : t) (root : id) (bound : string list) : string list =
  (* DFS 遍历 DAG，收集不在 bound 中的 DVar 名字，去重 *)
```

---

## 阶段 3：AST → CFG（cfg.ml）

### 设计思路

CFG 是编译器的核心 IR，将 AST 转换为**基本块 + 三地址码（TAC）**的形式。`build_expr` 递归遍历 AST，纯子表达式委托给 `dag.ml`，Let/If/Func/App 在 CFG 层处理。

### TAC 指令类型

```ocaml
type reg = int
type label = string

type tac =
  | TConst of reg * int              (* r := constant *)
  | TBool of reg * bool
  | TBinop of reg * binop * reg * reg (* r := r1 op r2 *)
  | TCopy of reg * reg               (* r_dst := r_src *)
  | TLoad of reg * reg * int         (* r := mem[base + off] *)
  | TStore of reg * int * reg        (* mem[base + off] := r *)
  | TAlloc of reg * int              (* r := malloc(size) *)
  | TLa of reg * label               (* r := &label *)
  | TCall of reg * reg * reg * reg   (* result = call(func, env, arg) *)
```

### 基本块结构

```ocaml
type terminator =
  | TReturn of reg                   (* return r *)
  | TJump of label                   (* goto label *)
  | TBranch of reg * label * label   (* if r != 0 goto l1 else l2 *)
  | THalt

type block = {
  label : label;
  body : tac list;        (* 基本块内的指令序列 *)
  term : terminator;      (* 基本块终止符 *)
}
```

### Builder 状态

```ocaml
type builder_state = {
  mutable current_label : label;
  mutable current_body : tac list;    (* 当前块正在构建的指令 *)
  mutable blocks : block list;        (* 已完成的块（逆序） *)
  mutable functions : func list;      (* 已记录的函数 *)
}
```

### Let 处理

```ocaml
| Let (x, e1, e2) ->
    let r1 = build_expr st env e1 in    (* 求值 e1 → 寄存器 r1 *)
    let env' = (x, r1) :: env in        (* 绑定 x → r1 *)
    build_expr st env' e2               (* 在扩展环境中求值 e2 *)
```

`let a = 10`：
1. `build_pure_expr`：`Int 10` → DAG → TConst(2, 10)，发射到当前块
2. `env' = [(a, 2)]` → 后续代码通过 `a` 查表得到寄存器 2

### Func（闭包）处理

```ocaml
| Func (param, body) ->
    (* 1. 自由变量分析 *)
    let fv = free_vars_of_expr body [param] in  (* 例: fv = ["a"] *)

    (* 2. 保存 builder 状态 *)
    let saved_body = st.current_body in
    let saved_label = st.current_label in
    let saved_blocks = st.blocks in

    (* 3. 构建函数体 *)
    st.current_body <- [];
    st.current_label <- func_label ^ "_entry";
    st.blocks <- [];

    (* 从 a1（闭包环境指针）加载捕获变量 *)
    let closure_env =
      List.mapi (fun i v ->
        let r = Util.fresh_reg () in
        emit_tac st (TLoad (r, 1, 8 + i * 8));  (* 从 env 加载 *)
        (v, r))
      fv
    in
    let local_env = (param, 0 (* a0 *)) :: closure_env in
    let result_reg = build_expr st local_env body in
    finish_block st (TReturn result_reg);

    (* 4. 记录函数 *)
    let func = { name = func_label; param; entry = ...;
                 blocks = st.blocks; captured = fv } in
    st.functions <- func :: st.functions;

    (* 5. 恢复 builder 状态 *)
    st.current_body <- saved_body;
    st.current_label <- saved_label;
    st.blocks <- saved_blocks;

    (* 6. 在闭包创建上下文中分配闭包 *)
    let closure_size = 8 + 8 * List.length fv in   (* 代码指针 + N 个捕获变量 *)
    let r_closure = Util.fresh_reg () in
    emit_tac st (TAlloc (r_closure, closure_size));       (* malloc *)
    let r_codeptr = Util.fresh_reg () in
    emit_tac st (TLa (r_codeptr, func_label));             (* 取函数地址 *)
    emit_tac st (TStore (r_closure, 0, r_codeptr));        (* 存代码指针 *)
    List.iteri (fun i v ->
      let r_val = List.assoc v env in
      emit_tac st (TStore (r_closure, 8 + i * 8, r_val))  (* 存捕获变量 *)
    ) fv;
    r_closure  (* 返回闭包指针 *)
```

**闭包内存布局：**

```
closure_ptr + 0:  代码指针 (func_1 的地址)     ← 8 bytes
closure_ptr + 8:  捕获变量 a 的值 (10)         ← 8 bytes
closure_ptr + 16: 捕获变量 b 的值 (如果有)     ← 8 bytes
...
```

### App（函数调用）处理

```ocaml
| App (e_func, e_arg) ->
    let r_func = build_expr st env e_func in   (* 闭包指针 *)
    let r_arg = build_expr st env e_arg in      (* 参数值 *)
    let r_result = Util.fresh_reg () in
    emit_tac st (TCall (r_result, r_func, r_func, r_arg));
    r_result
```

TCall 在 IR lowering 阶段展开为实际的调用序列（见阶段 5）。

### If 处理

```ocaml
| If (cond, e_then, e_else) ->
    let rc = build_expr st env cond in
    let then_lbl = fresh_label in
    let else_lbl = fresh_label in
    let end_lbl = fresh_label in
    finish_block st (TBranch (rc, then_lbl, else_lbl));

    (* Then 块 *)
    start_new_block then_lbl;
    let rt = build_expr st env e_then in
    emit_tac st (TCopy (r_result, rt));
    finish_block st (TJump end_lbl);

    (* Else 块 *)
    start_new_block else_lbl;
    let re = build_expr st env e_else in
    emit_tac st (TCopy (r_result, re));
    finish_block st (TJump end_lbl);

    (* 合并块 *)
    start_new_block end_lbl;
    r_result
```

If 创建三个基本块（then / else / merge），两个分支各自把结果拷贝到同一个寄存器实现值合并。

### 最终生成的 TAC（示例）

```
main_entry:
  TConst(2, 10)              # a = 10
  TAlloc(3, 16)              # r_closure = malloc(16)
  TLa(4, "func_1")           # r_codeptr = &func_1
  TStore(3, 0, 4)            # closure[0] = code_ptr
  TStore(3, 8, 2)            # closure[8] = a (10)
  TConst(6, 3)               # r_arg = 3
  TCall(7, 3, 3, 6)          # r_result = call(r_closure, 3)
  TReturn(7)                 # return r_result
```

---

## 阶段 4：CFG 优化（cfg.ml）

优化按以下顺序对每个函数独立执行两次迭代：

### Pass 1：Copy Propagation（拷贝传播）

前向扫描，维护 `copies: reg → reg` 映射。遇到 `TCopy(rd, rs)` 时记录映射；后续指令中用到的寄存器都替换为原始来源。

```
优化前:
  TCopy(r9, r3)     # r9 := r3
  TLoad(r9, r9, 0)  # r9 := mem[r9 + 0]

优化后:
  TCopy(r9, r3)     # r9 := r3
  TLoad(r3, r3, 0)  # r9 替换为 r3，r9 不再被使用
```

在随后的 DCE 中，不再被使用的 `TCopy(r9, r3)` 会被删除。

### Pass 2：Constant Propagation（常量传播）

前向扫描，维护 `consts: reg → int option`（`Some n` = 常量 n，`None` = 非常量）。

```
遇到 TConst(r, 5)       → 记录 r → Some 5
遇到 TBinop(r, Add, r1, r2):
  如果 r1 → Some 3, r2 → Some 5  → 替换为 TConst(r, 8)
遇到 TStore(_, _, r)    → 不影响 consts
遇到分支条件为常量      → 把 TBranch 替换为 TJump
```

### Pass 3：Dead Code Elimination（死代码删除）

**反向活跃性分析**：从基本块终止符出发，反向遍历指令，追踪哪些寄存器会被后续使用。

```
初始: live = term_live_out(TReturn(r7)) = {7}

反向处理每条指令:
  TCall(7, 3, 3, 6):
    7 在 live 中 → 保留，kill 7，add 3, 3, 6 → live = {3, 6}

  TConst(6, 3):
    6 在 live 中 → 保留，kill 6 → live = {3}

  TStore(3, 8, 2):
    Store 始终保留，add 3, 2 → live = {2, 3}

  TStore(3, 0, 4):
    Store 始终保留，add 3, 4 → live = {4, 2, 3}

  TLa(4, "func_1"):
    4 在 live 中 → 保留，kill 4 → live = {2, 3}

  TAlloc(3, 16):
    3 在 live 中 → 保留，kill 3 → live = {2}

  TConst(2, 10):
    2 在 live 中 → 保留，kill 2 → live = {}

  某条指令的 def reg 不在 live 中 → 删除该指令
```

### 优化管道

```ocaml
let optimize_func (fn : func) : func =
  (* 第一轮 *)
  blocks → DCE → ConstProp → CopyProp →
  (* 第二轮：第一轮暴露的新优化机会 *)
  DCE → 返回优化后的函数
```

---

## 阶段 5：CFG → Low IR（ir.ml）

### 设计思路

Low IR 使用 RISC-V-like 指令 + 虚拟寄存器。每条 CFG TAC 指令翻译成 1+ 条 Low IR 指令。虚拟寄存器 0 = a0（参数/返回值），虚拟寄存器 1 = a1（闭包环境），其余自由分配。

### Low IR 指令类型

```ocaml
type vreg = int

type instr =
  | Li of vreg * int64          (* rd := imm *)
  | Mv of vreg * vreg           (* rd := rs *)
  | Add of vreg * vreg * vreg   (* rd := rs1 + rs2 *)
  | Sub of vreg * vreg * vreg
  | Mul of vreg * vreg * vreg
  | Div of vreg * vreg * vreg
  | Slt of vreg * vreg * vreg   (* rd := rs1 < rs2 ? 1 : 0 *)
  | Xori of vreg * vreg * int64 (* rd := rs1 xor imm *)
  | Addi of vreg * vreg * int64 (* rd := rs1 + imm *)
  | Ld of vreg * vreg * int     (* rd := mem[rs + offset] *)
  | Sd of vreg * int * vreg     (* mem[rb + offset] := rs *)
  | La of vreg * string         (* rd := &label *)
  | Jalr of vreg                (* jalr ra, rs *)
  | Jal of string               (* jal ra, label *)
  | Beqz of vreg * string       (* if rs == 0 goto label *)
  | J of string                 (* goto label *)
  | Label of string
  | Ret
```

### 指令选择（TAC → IR 对照表）

| TAC | Low IR |
|---|---|
| `TConst(r, n)` | `Li(r, n)` |
| `TBool(r, true)` | `Li(r, 1)` |
| `TBool(r, false)` | `Li(r, 0)` |
| `TCopy(rd, rs)` | `Mv(rd, rs)` |
| `TBinop(r, Add, r1, r2)` | `Add(r, r1, r2)` |
| `TBinop(r, Sub, r1, r2)` | `Sub(r, r1, r2)` |
| `TBinop(r, Mul, r1, r2)` | `Mul(r, r1, r2)` |
| `TBinop(r, Div, r1, r2)` | `Div(r, r1, r2)` |
| `TBinop(r, Lqu, r1, r2)` | `Slt(tmp, r2, r1); Xori(r, tmp, 1)` |
| `TLoad(rd, rb, off)` | `Ld(rd, rb, off)` |
| `TStore(rb, off, rs)` | `Sd(rb, off, rs)` |
| `TLa(rd, lbl)` | `La(rd, lbl)` |
| `TAlloc(rd, size)` | `Li(tmp, size); Mv(0, tmp); Jal("malloc"); Mv(rd, 0)` |
| `TCall(result, func, _, arg)` | `Mv(0, arg); Addi(1, func, 8); Ld(tmp, func, 0); Jalr(tmp); Mv(result, 0)` |
| `TReturn(r)` | `Mv(0, r)`（epilogue 处理 ret） |
| `TJump(lbl)` | `J(lbl)` |
| `TBranch(r, l1, l2)` | `Beqz(r, l2); J(l1)` |

### TCall 展开详解

```
TCall(result, func, _env, arg)

展开为:
  Mv(0, arg)          # a0 = 参数
  Addi(1, func, 8)    # a1 = 闭包指针 + 8（环境指针）
  Ld(tmp, func, 0)    # 加载代码指针
  Jalr(tmp)           # 间接调用
  Mv(result, 0)       # 结果 = a0
```

### 寄存器分配（贪心线性扫描）

```ocaml
(* 物理寄存器池（13 个可分配寄存器） *)
let all_allocatable = [T0; T1; T2; T3; T4; T5; T6; A2; A3; A4; A5; A6; A7]

(* 固定映射 *)
mapping[0] = A0    (* 参数/返回值 *)
mapping[1] = A1    (* 闭包环境 *)

(* vreg 2+ 按编号顺序依次分配 *)
(* 如果可分配寄存器用尽，标记为 spill *)
```

---

## 阶段 6：Low IR → Assembly（codegen.ml）

### 序言/尾声

```
func_name:
    addi sp, sp, -FRAME_SIZE       # 分配栈帧
    sd ra, FRAME_SIZE-8(sp)        # 保存返回地址
    sd fp, FRAME_SIZE-16(sp)       # 保存帧指针
    mv fp, sp                      # 设置新帧指针

    ... body ...

    mv sp, fp                      # 恢复栈指针
    ld ra, FRAME_SIZE-8(sp)        # 恢复返回地址
    ld fp, FRAME_SIZE-16(sp)       # 恢复帧指针
    addi sp, sp, FRAME_SIZE        # 释放栈帧
    ret
```

**栈帧布局：**

```
高地址
  fp + FRAME_SIZE       (caller's sp)
  fp + FRAME_SIZE - 8   保存的 ra
  fp + FRAME_SIZE - 16  保存的 fp
  fp                    (帧指针)
  fp - 8                局部变量/spill 槽 1
  fp - 16               局部变量/spill 槽 2
  ...
低地址
  sp                    (栈顶)
```

栈帧大小 = 16（ra + fp）+ 8 × spill 数量。

### Spill 处理

当寄存器分配器标记某 vreg 为 spilled 时，使用 `t4`/`t5`/`t6` 作临时寄存器：

```
(* 使用前加载 *)
ld t4, -offset(fp)     # 从栈加载溢出值

(* 指令 *)
add t6, t4, t5         # 正常操作

(* 定义后存储 *)
sd t6, -offset(fp)     # 存回栈
```

### 物理寄存器映射

```
vreg 0  → a0 (x10)   参数/返回值
vreg 1  → a1 (x11)   闭包环境指针
vreg 2+ → t0-t6 (x5-x7, x28-x31) + a2-a7 (x12-x17)
sp      → sp  (x2)
fp      → fp  (x8)
ra      → ra  (x1)
```

---

## 阶段 7：管道编排（bin/main.ml）

```ocaml
let () =
  (* 1. 读取源文件 *)
  let source = In_channel.with_open_text input_file In_channel.input_all in

  (* 2. 解析 → AST *)
  let ast = parse source in

  (* 3. AST → CFG（内部使用 DAG） *)
  let cfg_prog = Cfg.build_program ast in

  (* 4. 优化 CFG（DCE + const prop + copy prop）× 2 *)
  let opt_cfg = Cfg.optimize_program cfg_prog in

  (* 5. CFG → Low IR（指令选择 + 寄存器分配） *)
  let ir_prog = Ir.lower_program opt_cfg in

  (* 6. Low IR → RISC-V 汇编（发射） *)
  let asm = Codegen.emit ir_prog in

  (* 7. 写入输出文件 *)
  Out_channel.with_open_text output_file (fun oc ->
    output_string oc asm)
```

---

## 完整示例追踪

输入：`let a = 10 in let f = fun x -> x + a in f 3`

### AST

```
Let("a", Int 10,
  Let("f", Func("x", Binop(Add, Var "x", Var "a")),
    App(Var "f", Int 3)))
```

### CFG TAC（优化后）

```
main_entry:
  TConst(2, 10)              # a = 10
  TAlloc(3, 16)              # r_closure = malloc(16)
  TLa(4, "func_1")           # r_codeptr = &func_1
  TStore(3, 0, 4)            # closure[0] = code_ptr
  TStore(3, 8, 2)            # closure[8] = 10
  TConst(6, 3)               # r_arg = 3
  TCall(7, 3, 3, 6)          # r_result = f(3)
  TReturn(7)                 # return r_result
```

### 生成汇编

```asm
.text
.global main
main:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd fp, 0(sp)
    mv fp, sp
main_entry:
    li t0, 10               # a = 10
    li t5, 16               # malloc 大小参数
    mv a0, t5
    jal ra, malloc
    mv t1, a0               # t1 = 闭包指针
    la t2, func_1           # 代码指针
    sd t2, 0(t1)            # closure[0] = code_ptr
    sd t0, 8(t1)            # closure[8] = 10
    li t3, 3                # 参数 = 3
    mv a0, t3               # a0 = 3
    addi a1, t1, 8          # a1 = closure + 8 (环境)
    ld t6, 0(t1)            # 加载代码指针
    jalr ra, 0(t6)          # 调用 f
    mv t4, a0               # 获取返回值
    mv a0, t4               # 放入 a0 准备返回
    mv sp, fp
    ld ra, 8(sp)
    ld fp, 0(sp)
    addi sp, sp, 16
    ret

func_1:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd fp, 0(sp)
    mv fp, sp
func_1_entry:
    ld t0, 8(a1)            # 从环境加载捕获的 a
    add t1, a0, t0          # x + a
    mv a0, t1               # 返回值
    mv sp, fp
    ld ra, 8(sp)
    ld fp, 0(sp)
    addi sp, sp, 16
    ret
```

### 执行语义

1. `a = 10`：存入闭包偏移 8 处
2. 分配闭包 `[func_1地址 | 10]`，`t1` 指向闭包首地址
3. `f 3`：`a0=3`，`a1=闭包+8(指向捕获的 a)`，通过 `jalr` 调用 `func_1`
4. `func_1` 从 `a1+8` 加载 `a=10`，计算 `a0 + 10 = 3 + 10 = 13`
5. 返回值 `13` 在 `a0` 中

---

## 关键设计决策

| 决策 | 说明 |
|---|---|
| DAG 只覆盖纯表达式 | Let/If/Func/App 在 CFG 层处理，避免在 DAG 中建模作用域和控制流 |
| 非 SSA 形式 | If 分支通过拷贝到共享寄存器合并值，避免 φ 节点 |
| 贪心寄存器分配 | SimPL 程序小，13 个可分配寄存器通常足够，很少需要 spill |
| 栈帧一次性分配 | 在序言中分配全部帧空间，函数体内部不再调整 sp |
| 闭包用 malloc 模拟 | 每次调用 Func 在堆上分配 `[code_ptr, cap_1, ..., cap_N]` |

## 使用方法

```bash
cd /home/sevailen/ocaml/starter6_opt
dune build
echo 'let a = 10 in let f = fun x -> x + a in f 3' > test.in
dune exec starter6_opt -- test.in output.s
cat output.s
```
