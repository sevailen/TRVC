.text
.global main
main:
	addi sp, sp, -16
	sd ra, 8(sp)
	sd fp, 0(sp)
	mv fp, sp
main_entry:
	li t6, 8
	mv a0, t6
	jal ra, malloc
	mv t0, a0
	la t1, func_1
	sd t1, 0(t0)
	li t2, 2
	li t3, 3
	mv a0, t3
	addi a1, t2, 8
	ld a2, 0(t2)
	jalr ra, 0(a2)
	mv t4, a0
	mv a0, t4
	addi a1, t0, 8
	ld a3, 0(t0)
	jalr ra, 0(a3)
	mv t5, a0
	mv a0, t5
	mv sp, fp
	ld ra, 8(sp)
	ld fp, 0(sp)
	addi sp, sp, 16
	ret

func_2:
	addi sp, sp, -16
	sd ra, 8(sp)
	sd fp, 0(sp)
	mv fp, sp
func_2_entry:
	ld t0, 8(a1)
	add t1, t0, a0
	j L_1
L_1:
	mul t2, t1, t1
	mv t3, t2
	j L_3
L_2:
	li t5, 5
	add t4, t1, t5
	mv t3, t4
	j L_3
L_3:
	mv a0, t3
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
	li t2, 32
	mv a0, t2
	jal ra, malloc
	mv t0, a0
	la t1, func_2
	sd t1, 0(t0)
	sd a0, 8(t0)
	sd a0, 16(t0)
	sd a0, 24(t0)
	mv a0, t0
	mv sp, fp
	ld ra, 8(sp)
	ld fp, 0(sp)
	addi sp, sp, 16
	ret

