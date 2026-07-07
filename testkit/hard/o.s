.text
fn0:
	li t6,32
	sub sp,sp,t6
	add t6,sp,t6
	sw ra,-4(t6)
	sw fp,-8(t6)
	mv fp,t6
	sw s3,-12(fp)
	sw s4,-16(fp)
	sw s2,-20(fp)
	sw s1,-24(fp)
	sw s5,-28(fp)
fn0_entry:
	mv t0,a0
	mv t1,a1
	mv t2,a2
	mv t3,a3
	li s1,-2
	bnez s1,L_1
	j L_2
L_1:
	li s1,0
	sub s2,t3,s1
	snez s2,s2
	j L_3
L_2:
	li s2,0
L_3:
	li s1,1
	mul s3,s2,s1
	bnez s3,L_5
L_4:
	bnez t1,L_7
	j L_8
L_7:
	li s3,0
	sub s2,t1,s3
	snez s2,s2
	j L_9
L_8:
	li s2,0
L_9:
	slt s3,t1,t3
	sub s1,s2,s3
	li s2,0
	sub s3,s1,s2
	snez s3,s3
	j L_6
L_5:
	li s3,1
L_6:
	mv s1,s3
	sub s3,s1,t2
	seqz s3,s3
	bnez s3,L_10
	j L_11
L_10:
	li s3,20
	slt s2,s3,t1
	li s3,0
	sub s4,s2,s3
	snez s4,s4
	j L_12
L_11:
	li s4,0
L_12:
	li s2,0
	li s3,-7
	mul s5,s3,s1
	slt s3,s5,s2
	slt s2,s3,s4
	mv a0,s2
	j .Lepi_fn0
L_13:
	li s2,-6
	mul s4,s1,s2
	slt s2,s1,t1
	xori s2,s2,1
	slt s3,s2,s4
	bnez t3,L_14
	j L_15
L_14:
	li s4,0
	sub s2,t0,s4
	snez s2,s2
	j L_16
L_15:
	li s2,0
L_16:
	li s4,5
	slt s5,s4,s1
	sub s4,s2,s5
	seqz s4,s4
	slt s2,s3,s4
	xori s2,s2,1
	bnez s2,L_17
	j L_18
L_17:
	slt s2,t3,t1
	bnez s2,L_21
L_20:
	li s2,-7
	sub s3,s2,s1
	seqz s3,s3
	li s2,0
	sub s4,s3,s2
	snez s4,s4
	j L_22
L_21:
	li s4,1
L_22:
	li s3,-7
	sub s2,s1,s3
	snez s2,s2
	li s3,-5
	slt s5,s3,t3
	xori s5,s5,1
	sub s3,s2,s5
	snez s3,s3
	slt s2,s3,s4
	mv a0,s2
	j .Lepi_fn0
L_18:
L_19:
	li s2,0
	bnez s2,L_23
	j L_24
L_23:
	slt s2,t0,t3
	xori s2,s2,1
	li s4,0
	sub s3,s2,s4
	snez s3,s3
	j L_25
L_24:
	li s3,0
L_25:
	li s2,-8
	sub s4,t2,s2
	slt s2,s4,t1
	xori s2,s2,1
	slt s4,s3,s2
	mv s1,s4
	mul s4,t0,t2
	li s3,-4
	sub s2,s1,s3
	snez s2,s2
	slt s3,s4,s2
	li s4,-8
	sub s2,s4,t1
	snez s2,s2
	li s4,20
	add s5,s4,t3
	slt s4,s5,s2
	xori s4,s4,1
	add s2,s3,s4
	bnez s2,L_26
	j L_27
L_26:
	li s2,-10
	bnez s2,L_30
L_29:
	li s2,0
	sub s3,s1,s2
	snez s3,s3
	j L_31
L_30:
	li s3,1
L_31:
	li s2,1
	add s4,s3,s2
	li s3,1
	bnez s3,L_33
L_32:
	li s3,-4
	sub s2,t3,s3
	seqz s2,s2
	li s3,0
	sub s5,s2,s3
	snez s5,s5
	j L_34
L_33:
	li s5,1
L_34:
	slt s2,s5,s4
	xori s2,s2,1
	mv a0,s2
	j .Lepi_fn0
L_27:
L_28:
	bnez t3,L_36
L_35:
	li s2,0
	sub s4,t3,s2
	snez s4,s4
	j L_37
L_36:
	li s4,1
L_37:
	bnez t1,L_38
	j L_39
L_38:
	li s2,0
	sub s5,t1,s2
	snez s5,s5
	j L_40
L_39:
	li s5,0
L_40:
	slt s2,s4,s5
	xori s2,s2,1
	bnez t2,L_42
L_41:
	li s4,-17
	li s5,0
	sub s3,s4,s5
	snez s3,s3
	j L_43
L_42:
	li s3,1
L_43:
	li s4,-4
	slt s5,s4,t2
	sub s4,s3,s5
	slt s3,s4,s2
	mv s2,s3
	sub s3,s1,t3
	bnez s3,L_44
	j L_45
L_44:
	slt s3,t1,t3
	li t1,0
	sub t3,s3,t1
	snez t3,t3
	j L_46
L_45:
	li t3,0
L_46:
	bnez t3,L_47
	j L_48
L_47:
	li t3,-2
	add t1,t3,t2
	bnez t1,L_50
	j L_51
L_50:
	sub t1,t0,t2
	li t0,0
	sub t2,t1,t0
	snez t2,t2
	j L_52
L_51:
	li t2,0
L_52:
	li t1,0
	sub t0,t2,t1
	snez t0,t0
	j L_49
L_48:
	li t0,0
L_49:
	mv a0,t0
	j .Lepi_fn0
.Lepi_fn0:
	lw s3,-12(fp)
	lw s4,-16(fp)
	lw s2,-20(fp)
	lw s1,-24(fp)
	lw s5,-28(fp)
	lw ra,-4(fp)
	lw t6,-8(fp)
	mv sp,fp
	mv fp,t6
	ret

fn1:
	li t6,32
	sub sp,sp,t6
	add t6,sp,t6
	sw ra,-4(t6)
	sw fp,-8(t6)
	mv fp,t6
	sw s3,-12(fp)
	sw s4,-16(fp)
	sw s2,-20(fp)
	sw s1,-24(fp)
fn1_entry:
	mv t0,a0
	mv s1,a1
	mv s2,a2
	mv s3,a3
	li s4,-7
	li t1,16
	mv a0,t1
	mv a1,s3
	mv a2,t0
	mv a3,t0
	call fn0
	mv t1,a0
	slt t2,t1,s4
	xori t2,t2,1
	li t1,-9
	bnez t1,L_54
L_53:
	li t1,0
	sub t3,s3,t1
	snez t3,t3
	j L_55
L_54:
	li t3,1
L_55:
	sub t1,t2,t3
	bnez t1,L_56
	j L_57
L_56:
	li t1,16
	bnez t1,L_60
L_59:
	li t1,-12
	li t2,0
	sub t3,t1,t2
	snez t3,t3
	j L_61
L_60:
	li t3,1
L_61:
	sub t1,s2,s1
	sub t2,t3,t1
	seqz t2,t2
	mv a0,t2
	j .Lepi_fn1
L_57:
L_58:
	li t2,-5
	bnez t2,L_63
L_62:
	li t2,2
	li t3,0
	sub t1,t2,t3
	snez t1,t1
	j L_64
L_63:
	li t1,1
L_64:
	li t2,7
	sub t3,t2,s1
	add t2,t1,t3
	mv a0,t2
	j .Lepi_fn1
L_65:
	li t2,-10
	sub t1,t2,t0
	seqz t1,t1
	li t2,10
	sub t3,t2,t0
	add t0,t1,t3
	mv a0,t0
	j .Lepi_fn1
.Lepi_fn1:
	lw s3,-12(fp)
	lw s4,-16(fp)
	lw s2,-20(fp)
	lw s1,-24(fp)
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
	li t0,5
	li t1,1
	li t2,3
	li t3,0
	mv a0,t0
	mv a1,t1
	mv a2,t2
	mv a3,t3
	call fn1
	mv t3,a0
	mv a0,t3
	j .Lepi_main
.Lepi_main:
	lw ra,-4(fp)
	lw t6,-8(fp)
	mv sp,fp
	mv fp,t6
	ret

.data
g0:
	.word 8
