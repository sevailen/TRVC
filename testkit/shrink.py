#!/usr/bin/env python3
# Shrink a mismatching ToyC file line-by-line while preserving the ours!=gcc mismatch.
import subprocess, sys, os
KIT="/mnt/c/Users/Sevailen/Desktop/starter6_opt/testkit"
EXE=f"{KIT}/../_build/default/bin/main.exe"
QEMU="/tmp/qroot/usr/bin/qemu-riscv32-static"

def gcc_run(src):
    open("/tmp/s.c","w").write(src)
    if subprocess.run(["gcc","-w","-x","c","/tmp/s.c","-o","/tmp/sref"],stderr=subprocess.DEVNULL).returncode!=0: return None
    return subprocess.run(["/tmp/sref"]).returncode & 255
def ours_run(src):
    open("/tmp/s.tc","w").write(src)
    if subprocess.run([EXE],stdin=open("/tmp/s.tc"),stdout=open("/tmp/s.s","w"),stderr=subprocess.DEVNULL).returncode!=0: return None
    if subprocess.run(["riscv64-unknown-elf-gcc","-march=rv32im","-mabi=ilp32","-nostdlib","-static",f"{KIT}/start.s","/tmp/s.s","-o","/tmp/s.elf"],stderr=subprocess.DEVNULL).returncode!=0: return None
    try: return subprocess.run([QEMU,"/tmp/s.elf"],timeout=10).returncode & 255
    except: return None
def mismatches(src):
    g=gcc_run(src)
    if g is None: return False
    o=ours_run(src)
    return o is not None and o!=g

src=open(sys.argv[1]).read()
assert mismatches(src), "input does not mismatch"
lines=src.split("\n")
changed=True
while changed:
    changed=False
    i=0
    while i<len(lines):
        trial=lines[:i]+lines[i+1:]
        if mismatches("\n".join(trial)):
            lines=trial; changed=True
        else:
            i+=1
red="\n".join(lines)
g=gcc_run(red); o=ours_run(red)
print(f"=== minimized (ours={o} gcc={g}) ===")
print(red)
open(sys.argv[1].replace(".tc","_min.tc"),"w").write(red)
