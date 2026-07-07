#!/usr/bin/env python3
# Differential fuzzer: random ToyC -> compare our compiler vs host gcc.
import random, subprocess, os, sys

random.seed(int(sys.argv[1]) if len(sys.argv)>1 else 0)
N = int(sys.argv[2]) if len(sys.argv)>2 else 300
KIT="/mnt/c/Users/Sevailen/Desktop/starter6_opt/testkit"
EXE=f"{KIT}/../_build/default/bin/main.exe"
QEMU="/tmp/qroot/usr/bin/qemu-riscv32-static"

def gen_func(name, nparams, depth, funcs):
    ps=[f"p{i}" for i in range(nparams)]
    body=[]
    nlocal=[0]
    def var():
        opts=list(ps)+[f"v{i}" for i in range(nlocal[0])]
        return random.choice(opts) if opts else "0"
    def expr(d):
        if d<=0:
            r=random.random()
            if r<0.4: return str(random.randint(-20,20))
            if r<0.8: return var()
            # call another already-defined func
            cand=[f for f in funcs if f[0]!=name or True]
            if cand and random.random()<0.6:
                fn,fp=random.choice(cand)
                args=",".join(expr(0) for _ in range(fp))
                return f"{fn}({args})"
            return var()
        op=random.choice(["+","-","*","<",">","<=",">=","==","!=","&&","||"])
        a=expr(d-1); b=expr(d-1)
        # avoid div/mod by zero: skip / %
        return f"({a} {op} {b})"
    nstmt=random.randint(2,6)
    for _ in range(nstmt):
        s=random.random()
        if s<0.3:
            body.append(f"int v{nlocal[0]} = {expr(depth)};"); nlocal[0]+=1
        elif s<0.5 and nlocal[0]>0:
            body.append(f"v{random.randint(0,nlocal[0]-1)} = {expr(depth)};")
        elif s<0.7:
            body.append(f"if({expr(depth)}){{ return {expr(depth)}; }}")
        elif s<0.85 and nlocal[0]>0:
            i=random.randint(0,nlocal[0]-1)
            body.append(f"while(v{i} > 0){{ v{i} = v{i} - 1; }}")
        else:
            body.append(f"return {expr(depth)};")
    body.append(f"return {expr(depth)};")
    args=",".join(f"int {p}" for p in ps)
    return f"int {name}({args}){{\n  " + "\n  ".join(body) + "\n}\n"

def gen_prog():
    funcs=[]
    src=""
    ng=random.randint(0,2)
    for i in range(ng):
        src+=f"int g{i} = {random.randint(-10,10)};\n"
    nf=random.randint(1,3)
    for i in range(nf):
        np=random.randint(0,4)
        fn=f"fn{i}"
        src+=gen_func(fn,np,random.randint(1,3),funcs)
        funcs.append((fn,np))
    # main calls a random fn
    fn,fp=random.choice(funcs)
    args=",".join(str(random.randint(0,6)) for _ in range(fp))
    src+=f"int main(){{ return {fn}({args}); }}\n"
    return src

def run_gcc(src):
    open("/tmp/f.c","w").write(src)
    if subprocess.run(["gcc","-w","-x","c","/tmp/f.c","-o","/tmp/fref"],
                      stderr=subprocess.DEVNULL).returncode!=0: return None
    r=subprocess.run(["/tmp/fref"]); return r.returncode & 255

def run_ours(src):
    open("/tmp/f.tc","w").write(src)
    r=subprocess.run([EXE],stdin=open("/tmp/f.tc"),stdout=open("/tmp/f.s","w"),
                     stderr=subprocess.DEVNULL)
    if r.returncode!=0: return ("CFAIL",None)
    if subprocess.run(["riscv64-unknown-elf-gcc","-march=rv32im","-mabi=ilp32",
                       "-nostdlib","-static",f"{KIT}/start.s","/tmp/f.s","-o","/tmp/f.elf"],
                      stderr=subprocess.DEVNULL).returncode!=0: return ("AFAIL",None)
    r=subprocess.run([QEMU,"/tmp/f.elf"],timeout=10)
    return ("OK",r.returncode & 255)

mism=0; cfail=0; afail=0; ok=0
for t in range(N):
    src=gen_prog()
    g=run_gcc(src)
    if g is None: continue          # gcc rejected (our-invalid) — skip
    st,o=run_ours(src)
    if st=="CFAIL": cfail+=1;
    elif st=="AFAIL": afail+=1
    elif o!=g:
        mism+=1
        if mism<=5:
            open(f"{KIT}/hard/fuzz_{mism}.tc","w").write(src)
            print(f"--- MISMATCH #{mism}: ours={o} gcc={g} -> hard/fuzz_{mism}.tc")
    else: ok+=1
    if st in("CFAIL","AFAIL") and (cfail+afail)<=5:
        open(f"{KIT}/hard/bad_{cfail+afail}.tc","w").write(src)
        print(f"--- {st}: saved hard/bad_{cfail+afail}.tc")
print(f"total={N} ok={ok} mismatch={mism} compile_fail={cfail} asm_fail={afail}")
