.text
fn0:
	li t6,16
	sub sp,sp,t6
	add t6,sp,t6
	sw ra,-4(t6)
	sw fp,-8(t6)
	mv fp,t6
	sw s1,-12(fp)
fn0_entry:
	mv t0,a0
	mv t1,a1
	mv t2,a2
	bnez a1,L_2
L_1:
	li t3,0
	sub s1,t2,t3
	snez s1,s1
	j L_3
L_2:
	li s1,1
L_3:
	add t2,t1,t0
	sub t0,s1,t2
	mv a0,t0
	j .Lepi_fn0
.Lepi_fn0:
	lw s1,-12(fp)
	lw ra,-4(fp)
	lw t6,-8(fp)
	mv sp,fp
	mv fp,t6
	ret

fn1:
	li t6,16
	sub sp,sp,t6
	add t6,sp,t6
	sw ra,-4(t6)
	sw fp,-8(t6)
	mv fp,t6
	sw s1,-12(fp)
fn1_entry:
	mv s1,a0
	li t0,-2
	li t1,1
	mv a1,t0
	mv a2,t1
	call fn0
	mv t1,a0
	mv a0,t1
	mv a1,a0
	mv a2,a0
	call fn0
	mv t1,a0
	bnez t1,L_5
L_4:
	li t1,0
	sub t0,s1,t1
	snez t0,t0
	j L_6
L_5:
	li t0,1
L_6:
	mv a0,t0
	j .Lepi_fn1
.Lepi_fn1:
	lw s1,-12(fp)
	lw ra,-4(fp)
	lw t6,-8(fp)
	mv sp,fp
	mv fp,t6
	ret

.globl main
main:
	li t6,16
	sub sp,sp,t6
	add t6,sp,t6
	sw ra,-4(t6)
	sw fp,-8(t6)
	mv fp,t6
main_entry:
	li t0,6
	mv a0,t0
	call fn1
	mv t0,a0
	mv a0,t0
	j .Lepi_main
.Lepi_main:
	lw ra,-4(fp)
	lw t6,-8(fp)
	mv sp,fp
	mv fp,t6
	ret

