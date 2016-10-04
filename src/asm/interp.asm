; |=======================================================================|
; |                                                                       |
; |                                OCaPIC                                 |
; |                                                                       |
; |                             Benoit Vaugon                             |
; |                                                                       |
; |    This file is distributed under the terms of the CeCILL license.    |
; |    See file ../../LICENSE-en.                                         |
; |                                                                       |
; |=======================================================================|

;;;;;;;|-------------------------------------|;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|      CONVENTIONS, LIMITATIONS       |;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|-------------------------------------|;;;;;;;

;;; * stack:    (@-) topL : topH : ... : bottomL : bottomH      (@+)
;;; * heap:     (@-) block1 : block2 : ...                      (@+)
;;; * block:    (@-) tag : size : b0L : b0H : b1L : b1H : ...   (@+)
;;; * bytecode: (@-) op0 : arg0.0 : arg0.1 : op1 : arg1.0 : ... (@+)
;;; * stack pointer:      FSR2
;;; * heap pointer:       FSR1
;;; * free usage pointer: FSR0
;;; * the garbage collector overwrite FSR0
;;; * external primitives: 128 maximum
;;; * external argument number: 255 maximum
;;; * pointer LSB = 0 ; integer LSB = 1
;;; * collisions between heap address and program address forbidden
;;; * 'stack size' mod 256 = 0
;;; * 'heap size' mod 256 = 0
;;; * CCALL semantics : environment not pushed
;;; * different bytecode instruction argument sizes (1 or 2 bytes)
;;; * absolute bytecode jump address
;;; * atom tags <> 0
;;; * atom in heap forbidden
;;; * extraArgs < 128 => no more than 128 (function argument number + env size)
;;; * no more than 127 values defined in a "let rec ... and ..."
;;; * BSR = (@ACCU)H during VM execution
;;; * custom comparaison primitives must not use TMP_REG_[6 -> E]
;;; * custom hash primitives must not use TMP_REG_[5 -> E]
	
;;;;;;;|-------------------------------------|;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|              CONSTANTS              |;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|-------------------------------------|;;;;;;;

HEAP1_ANCHOR		EQU	0x0
HEAP2_ANCHOR		EQU	HEAP_SIZE

CLOSURE_TAG		EQU	.247
OBJECT_TAG		EQU	.248
INFIX_TAG		EQU	.249
FORWARD_TAG		EQU	.250
NO_SCAN_TAG		EQU	.251
ABSTRACT_TAG		EQU	.251
STRING_TAG		EQU	.252
DOUBLE_TAG		EQU	.253
DOUBLE_ARRAY_TAG 	EQU	.254
CUSTOM_TAG		EQU	.255

OUT_OF_MEMORY_IND	EQU	0x0
FAILURE_IND		EQU	0x2
INVALID_ARG_IND		EQU	0x3
DIVISION_BY_0_IND	EQU	0x5
STACK_OVERFLOW_IND	EQU	0x8

OBJ_TAG_INT		EQU	.256
OBJ_TAG_OUT_OF_MEMORY	EQU	.257

ATOM0_ADR		EQU	0xF88

MAX_USER_RAM		EQU	0xF80 ; user ram = [0x000 ; 0xF80[


;;;;;;;|-------------------------------------|;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|              VARIABLES              |;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|-------------------------------------|;;;;;;;

ENVL		EQU	(MAX_USER_RAM-0x2E) ; environment
ENVH		EQU	(MAX_USER_RAM-0x2D)
ACCUL		EQU	(MAX_USER_RAM-0x2C) ; accumulator
ACCUH		EQU	(MAX_USER_RAM-0x2B)
INT_FUN_L	EQU	(MAX_USER_RAM-0x2A)
INT_FUN_H	EQU	(MAX_USER_RAM-0x29)
OO_ID_COUNTER_L EQU     (MAX_USER_RAM-0x28)
OO_ID_COUNTER_H EQU     (MAX_USER_RAM-0x27)
OOM_TAG		EQU	(MAX_USER_RAM-0x26)
OOM_SIZE	EQU	(MAX_USER_RAM-0x25)
OOM_FIELD0_L	EQU	(MAX_USER_RAM-0x24)
OOM_FIELD0_H	EQU	(MAX_USER_RAM-0x23)
CUR_HEAP_END	EQU	(MAX_USER_RAM-0x22)
OTH_HEAP_END	EQU	(MAX_USER_RAM-0x21)
TRAP_SPL	EQU	(MAX_USER_RAM-0x20) ; trapSp
TRAP_SPH	EQU	(MAX_USER_RAM-0x1F)
EXTRA_ARGS	EQU	(MAX_USER_RAM-0x1E) ; extraArgs (*2 + 1)
GC_TMP_REG_1	EQU	(MAX_USER_RAM-0x1D) ; gc tmp registers
GC_TMP_REG_2	EQU	(MAX_USER_RAM-0x1C)
GC_TMP_REG_3	EQU	(MAX_USER_RAM-0x1B)
GC_TMP_REG_4	EQU	(MAX_USER_RAM-0x1A)
GC_TMP_REG_5	EQU	(MAX_USER_RAM-0x19)
TMP_REG_1	EQU	(MAX_USER_RAM-0x18) ; interp/runtime tmp registers
TMP_REG_2	EQU	(MAX_USER_RAM-0x17)
TMP_REG_3	EQU	(MAX_USER_RAM-0x16)
TMP_REG_4	EQU	(MAX_USER_RAM-0x15)
TMP_REG_5	EQU	(MAX_USER_RAM-0x14)
TMP_REG_6	EQU	(MAX_USER_RAM-0x13)
TMP_REG_7	EQU	(MAX_USER_RAM-0x12)
TMP_REG_8	EQU	(MAX_USER_RAM-0x11)
TMP_REG_9	EQU	(MAX_USER_RAM-0x10)
TMP_REG_A	EQU	(MAX_USER_RAM-0x0F)
TMP_REG_B	EQU	(MAX_USER_RAM-0x0E)
TMP_REG_C	EQU	(MAX_USER_RAM-0x0D)
TMP_REG_D	EQU	(MAX_USER_RAM-0x0C)
TMP_REG_E	EQU	(MAX_USER_RAM-0x0B)
GC_COUNTER_L	EQU	(MAX_USER_RAM-0x0A) ; gc running counter
GC_COUNTER_H	EQU	(MAX_USER_RAM-0x09)
RAND_CUR_L	EQU	(MAX_USER_RAM-0x08)
RAND_CUR_H	EQU	(MAX_USER_RAM-0x07)
INT_FLAGS_1	EQU	(MAX_USER_RAM-0x06)
INT_FLAGS_2	EQU	(MAX_USER_RAM-0x05)
INT_FLAGS_3	EQU	(MAX_USER_RAM-0x04)
INT_RUNS_1	EQU	(MAX_USER_RAM-0x03)
INT_RUNS_2	EQU	(MAX_USER_RAM-0x02)
INT_RUNS_3	EQU	(MAX_USER_RAM-0x01)


;;;;;;;|-------------------------------------|;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|        COMPUTED CONSTANTS           |;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|-------------------------------------|;;;;;;;
	
STACK_ANCHOR		EQU	(ENVL-0x1)	; stack from 0xF57 to 0xE00

HEAP1_END		EQU	(HEAP1_ANCHOR + HEAP_SIZE - 0x1)  	; 0x6
HEAP2_END		EQU	(HEAP2_ANCHOR + HEAP_SIZE - 0x1)  	; 0xD
STACK_END		EQU	((high STACK_ANCHOR) - STACK_SIZE) 	; 0xE


;;;;;;;|-------------------------------------|;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|                MACROS               |;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|-------------------------------------|;;;;;;;
	
	variable CPT_CUH = 0
M_CHECK_UNFULL_HEAP macro N
		movf	CUR_HEAP_END, W
		cpfseq	FSR1H
		bra	CHECK_UNFULL_HEAP_END_#v(CPT_CUH)
		movlw	(N * 0x2 + 0x2)
		addwf	FSR1L, W
		bnc	CHECK_UNFULL_HEAP_END_#v(CPT_CUH)

		call	caml_gc_exec

		movf	CUR_HEAP_END, W
		cpfseq	FSR1H
		bra	CHECK_UNFULL_HEAP_END_#v(CPT_CUH)
		movlw	(N * 0x2 + 0x2)
		addwf	FSR1L, W
		btfsc	STATUS, C
		goto	caml_raise_out_of_memory
CHECK_UNFULL_HEAP_END_#v(CPT_CUH)
CPT_CUH = CPT_CUH + 1
	endm

;;;

	variable CPT_CUS
M_CHECK_UNFULL_STACK macro N
		movlw	STACK_END
		cpfseq	FSR2H
		bra	CHECK_UNFULL_STACK_END_#v(CPT_CUS)
		movlw	-(0x2 * N)
		addwf	FSR2L, W
		btfss	STATUS, C
		bra	_INDIRECT_CAML_RAISE_STACK_OVERFLOW
CHECK_UNFULL_STACK_END_#v(CPT_CUS)
CPT_CUS = CPT_CUS + 1
	endm

M_PUSH	macro
		movff	ACCUH, POSTDEC2
		movff	ACCUL, POSTDEC2
		movlw	(STACK_END - 0x1)
		cpfsgt	FSR2H
		bra	_INDIRECT_CAML_RAISE_STACK_OVERFLOW
	endm
	
;;;
	
M_CONST	macro N
		movlw	2 * N + 1
		movwf	ACCUL
		clrf	ACCUH
	endm

;;;
	
M_ACC	macro N
		movsf	[2 * N + 1], ACCUL
		movsf	[2 * N + 2], ACCUH
	endm

;;;

M_GETGLOBAL macro
		lfsr	FSR0, STACK_ANCHOR
		M_READ_ARG
		subwf	FSR0L, F
		movf	TABLAT, W
		subwfb	FSR0H, F
		movff	POSTDEC0, ACCUH
		movff	INDF0, ACCUL
	endm
	
;;;
	
M_READ_ARG macro
		tblrd*+
		movf	TABLAT, W
		tblrd*+
	endm

;;;

#if ((high caml_externals) & 0x1) != 0
	error	"invalid constant caml_externals (high byte is odd)"
#endif
	
M_CCALL	macro
		movlw	((high caml_externals) / 2)
		movwf	PCLATH
		tblrd*+
		rlcf	TABLAT, W
		rlcf	PCLATH, F
		callw
		movlw	high _THE_BIG_SWITCH
		movwf	PCLATH
	endm
	
;;;

M_ATOM0 macro
		movlw	low ATOM0_ADR
		movwf	ACCUL
		movlw	high ATOM0_ADR
		movwf	ACCUH
	endm
	

;;;;;;;|-------------------------------------|;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|                CODE                 |;;;;;;;
;;;;;;;|                                     |;;;;;;;
;;;;;;;|-------------------------------------|;;;;;;;

	org	0x00
	banksel	ACCUL				; init BSR
	bra	caml_virtual_machine

	org	0x08
low_interrupt_handler:
	goto	asm_interrupt_handler

	org	0x18
high_interrupt_handler:
	goto	asm_interrupt_handler

caml_virtual_machine:
	lfsr	FSR0, 0x0			; clear all user registers
_LOOP_INIT_REGISTERS:
	clrf	POSTINC0
	clrf	POSTINC0
	clrf	POSTINC0
	clrf	POSTINC0
	movlw	low MAX_USER_RAM
	cpfseq	FSR0L
	bra	_LOOP_INIT_REGISTERS
	movlw	high MAX_USER_RAM
	cpfseq	FSR0H
	bra	_LOOP_INIT_REGISTERS
	
	movlw	HEAP1_END 			; init heap ends
	movwf	CUR_HEAP_END
	movlw	HEAP2_END
	movwf	OTH_HEAP_END

	clrf	PCLATU		 		; init PCLAT{H,U}
	movlw	high _THE_BIG_SWITCH
	movwf	PCLATH

	clrf	TBLPTRU				; init TBLPTRU

	lfsr	FSR1, (HEAP1_ANCHOR * 0x100) 	; init heap pointer
	lfsr	FSR2, STACK_ANCHOR		; init stack pointer

	movlw	low caml_globals_init_stack 	; init globals
	movwf	TBLPTRL
	movlw	high caml_globals_init_stack
	movwf	TBLPTRH
	bra	_LOOP_INIT_STACK_TEST		; init stack bottom
_LOOP_INIT_STACK:
	tblrd*+
	movf	TABLAT, W
	tblrd*+
	movff	TABLAT, POSTDEC2
	movwf	POSTDEC2
_LOOP_INIT_STACK_TEST:
	movlw	low caml_globals_init_heap
	cpfseq	TBLPTRL
	bra	_LOOP_INIT_STACK
	movlw	high caml_globals_init_heap
	cpfseq	TBLPTRH
	bra	_LOOP_INIT_STACK
	
	bra	_LOOP_INIT_HEAP_TEST 		; init heap bottom
_LOOP_INIT_HEAP:
	tblrd*+
	movff	TABLAT, POSTINC1
	tblrd*+
	movff	TABLAT, POSTINC1
_LOOP_INIT_HEAP_TEST:
	movlw	low caml_globals_init_end
	cpfseq	TBLPTRL
	bra	_LOOP_INIT_HEAP
	movlw	high caml_globals_init_end
	cpfseq	TBLPTRH
	bra	_LOOP_INIT_HEAP

	movlw	0x1
	movwf	TRAP_SPL 			; init trapSp
	movwf	INT_FUN_L			; init interruption catcher
	movwf	ACCUL				; init ACCU
	movwf	ENVL				; init ENV
	movwf	EXTRA_ARGS			; init EXTRA_ARGS
	movwf	OOM_SIZE			; init Out_of_memory block
	movwf	OOM_FIELD0_L
        movwf   OO_ID_COUNTER_L                 ; init OO_ID_COUNTER

	movlw	low caml_bytecode		; init bytecode pointer
	movwf	TBLPTRL
	movlw	high caml_bytecode
	movwf	TBLPTRH

	goto	_THE_BIG_LOOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;       COMPARATIVES JUMPS          ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_BEQ:
	M_READ_ARG
	cpfseq	ACCUL
	bra	_SKIP_ARGUMENT
	movf	TABLAT, W
	cpfseq	ACCUH
	bra	_SKIP_ARGUMENT
	;; fallthrough
	
_BRANCH_LOCAL_COPY:
	M_READ_ARG
	movwf	TBLPTRL
	movf	TABLAT, W
	movwf	TBLPTRH
	return

_BNEQ:
	M_READ_ARG
	cpfseq	ACCUL
	bra	_BRANCH_LOCAL_COPY
	movf	TABLAT, W
	cpfseq	ACCUH
	bra	_BRANCH_LOCAL_COPY
	;; fallthrough
	
_SKIP_ARGUMENT:
	movlw	0x2
	addwf	TBLPTRL, F
	btfsc	STATUS, C
	incf	TBLPTRH, F
	return

_BLTINT:
	tblrd*+			  ; TMP_REG_1 <- valL
	movff	TABLAT, TMP_REG_1
	tblrd*+ 		  ; TABLAT <- valH + 0x80
	movlw	0x80
	addwf	TABLAT, F
	movf	ACCUH, W	  ; W <- ACCUH + 0x80
	addlw	0x80
	subwf	TABLAT, W 	  ; TABLAT - W
	bnz	_BLTINT_L0	  ; if TABLAT <> W then goto L0
	movf	ACCUL, W
	subwf	TMP_REG_1, W	  ; valL - ACCUL
_BLTINT_L0:
	bc	_SKIP_ARGUMENT	  ; if C = 1 then continue
	bra	_BRANCH_LOCAL_COPY; else jump
	
_BLEINT:
	movf	ACCUH, W 	  ; TMP_REG_1 <- ACCUH + 0x80
	addlw	0x80
	movwf	TMP_REG_1
	tblrd*+			  ; TMP_REG_2 <- valL
	movff	TABLAT, TMP_REG_2
	tblrd*+			  ; W <- valH + 0x80
	movf	TABLAT, W
	addlw	0x80
	subwf	TMP_REG_1, W 	  ; TMP_REG_1 - W
	bnz	_BLEINT_L0	  ; if TMP_REG_1 <> W then goto L0
	movf	TMP_REG_2, W
	subwf	ACCUL, W 	  ; ACCUL - valL
_BLEINT_L0:
	bc	_BRANCH_LOCAL_COPY; if C = 1 then jump
	bra	_SKIP_ARGUMENT	  ; else continue
	
_BGTINT:
	movf	ACCUH, W 	  ; TMP_REG_1 <- ACCUH + 0x80
	addlw	0x80
	movwf	TMP_REG_1
	tblrd*+			  ; TMP_REG_2 <- valL
	movff	TABLAT, TMP_REG_2
	tblrd*+			  ; W <- valH + 0x80
	movf	TABLAT, W
	addlw	0x80
	subwf	TMP_REG_1, W	  ; TMP_REG_1 - W
	bnz	_BGTINT_L0	  ; if TMP_REG_1 <> W then goto L0
	movf	TMP_REG_2, W
	subwf	ACCUL, W	  ; ACCUL - valL
_BGTINT_L0:
	bc	_SKIP_ARGUMENT	  ; if C = 1 then continue
	bra	_BRANCH_LOCAL_COPY; else jump
	
_BGEINT:
	tblrd*+			  ; TMP_REG_1 <- valL
	movff	TABLAT, TMP_REG_1
	tblrd*+ 		  ; TABLAT <- valH + 0x80
	movlw	0x80
	addwf	TABLAT, F
	movf	ACCUH, W	  ; W <- ACCUH + 0x80
	addlw	0x80
	subwf	TABLAT, W	  ; TABLAT - W
	bnz	_BGEINT_L0	  ; if TABLAT <> W then goto L0
	movf	ACCUL, W
	subwf	TMP_REG_1, W	  ; valL - ACCUL
_BGEINT_L0:
	bc	_BRANCH_LOCAL_COPY; if C = 1 then jump
	bra	_SKIP_ARGUMENT	  ; else continue
	
_BULTINT:
	M_READ_ARG
	movwf	TMP_REG_1	  ; TMP_REG_1 <- valL
	movf	TABLAT, W	  ; W <- valH
	subwf	ACCUH, W	  ; W <- ACCUH - valH
	bnc	_SKIP_ARGUMENT	  ; if ACCUH < valH then continue
	bnz	_BRANCH_LOCAL_COPY; else if ACCUH <> valH then jump
	movf	TMP_REG_1, W	  ; else (ACCUH = valH)  W <- valL
	cpfsgt	ACCUL		  ; if ACCUL <= valL
	bra	_SKIP_ARGUMENT	  ; then continue
	bra	_BRANCH_LOCAL_COPY; else jump
	
_BUGEINT:
	M_READ_ARG
	movwf	TMP_REG_1	  ; TMP_REG_1 <- valL
	movf	TABLAT, W	  ; W <- valH
	subwf	ACCUH, W	  ; W <- ACCUH - valH
	bnc	_BRANCH_LOCAL_COPY; if ACCUH < valH then jump
	bnz	_SKIP_ARGUMENT	  ; else if ACCUH <> valH then continue
	movf	TMP_REG_1, W	  ; else (ACCUH = valH)  W <- valL
	cpfsgt	ACCUL		  ; if ACCUL <= valL
	bra	_BRANCH_LOCAL_COPY; then jump
	bra	_SKIP_ARGUMENT	  ; else continue


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              STACK                ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
_ACC3_BEQ:
	bc	_BEQ
	M_ACC	3
	return

_ACC4_BNEQ:
	bc	_BNEQ
	M_ACC	4
	return

_ACC5_BLTINT:
	bc	_BLTINT
	M_ACC	5
	return

_ACC6_BLEINT:
	bc	_BLEINT
	M_ACC	6
	return

_ACC7_BGTINT:
	bc	_BGTINT
	M_ACC	7
	return

_PUSHACC1_BULTINT:
	bc	_BULTINT
	M_PUSH
	M_ACC	1
	return
	
_PUSHACC2_BUGEINT:
	bc	_BUGEINT
	M_PUSH
	M_ACC	2
	return
	
_PUSH_ULTINT:
	bc	_ULTINT
	M_PUSH
	return

_PUSHACC0_UGEINT:
	bc	_UGEINT
	M_PUSH
	return

_ACC_BGEINT:
	bc	_BGEINT
	movff	FSR2H, FSR0H
	movff	FSR2L, FSR0L
	M_READ_ARG
	addwf	FSR0L, F
	movf	TABLAT, W
	addwfc	FSR0H, F
	movff	PREINC0, ACCUL
	movff	PREINC0, ACCUH
	return

#ifdef caml_useprim_caml_set_interruption_handler
_INTRETURN:
	goto	caml_interrupt_handler_return
_PUSHACC_INTRETURN:
	bc	_INTRETURN
#else
_PUSHACC_INTRETURN:
#endif
_PUSHACC:
	M_PUSH			; fallthrough
	movff	FSR2H, FSR0H
	movff	FSR2L, FSR0L
	M_READ_ARG
	addwf	FSR0L, F
	movf	TABLAT, W
	addwfc	FSR0H, F
	movff	PREINC0, ACCUL
	movff	PREINC0, ACCUH
	return

#ifdef caml_useprim_caml_set_interruption_handler
_INTRAISE:
	goto	caml_interrupt_handler_raise
_POP_INTRAISE:
	bc	_INTRAISE
#else
_POP_INTRAISE:
#endif
_POP:
	M_READ_ARG
	addwf	FSR2L, F
	movf	TABLAT, W
	addwfc	FSR2H, F
	return
	
_ASSIGN:
	movff	FSR2H, FSR0H
	movff	FSR2L, FSR0L
	M_READ_ARG
	addwf	FSR0L, F
	movf	TABLAT, W
	addwfc	FSR0H, F
	movff	ACCUL, PREINC0
	movff	ACCUH, PREINC0
	M_CONST	0
	return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;      UNSIGNED COMPARAISON         ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_ULTINT:
	movf	ACCUH, W 	 ; W <- ACCUH
	clrf	ACCUH	    	 ; ACCUH <- 0
	subwf	[0x2], W 	 ; STKH - W
	bnc	_POP_ACCUL_FALSE ; if C = 0 (<=> STK<ACCU) then ACCU <- FALSE
	bnz	_POP_ACCUL_TRUE	 ; if Z = 0 (<=> STK>ACCU) then ACCU <- TRUE
	movf	ACCUL, W	 ; W <- ACCUL
	cpfsgt	[0x1]		 ; if STKL <= W
	bra	_POP_ACCUL_FALSE ; then ACCU <- FALSE
	bra	_POP_ACCUL_TRUE	 ; else ACCU <- TRUE

_UGEINT:
	movf	ACCUH, W	 ; W <- ACCUH
	clrf	ACCUH		 ; ACCUH <- 0
	subwf	[0x2], W	 ; STKH - W
	bnc	_POP_ACCUL_TRUE	 ; if C = 0 (<=> STK<ACCU) then ACCU <- TRUE
	bnz	_POP_ACCUL_FALSE ; if Z = 0 (<=> STK>ACCU) then ACCU <- FALSE
	movf	ACCUL, W	 ; W <- ACCUL
	cpfsgt	[0x1]		 ; if STKL <= W
	bra	_POP_ACCUL_TRUE	 ; then ACCU <- TRUE
	bra	_POP_ACCUL_FALSE ; else ACCU <- FALSE

_POP_ACCUL_TRUE:
	movlw	0x3
	movwf	ACCUL
	addulnk	0x2

_POP_ACCUL_FALSE:
	movlw	0x1
	movwf	ACCUL
	addulnk	0x2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;            COMPARAISONS           ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_EQ:
	movf	ACCUL, W
	clrf	ACCUL
	bsf	ACCUL, 0
	bsf	ACCUL, 1
	cpfseq	PREINC2
	bcf	ACCUL, 1
	movf	ACCUH, W
	cpfseq	PREINC2
	bcf	ACCUL, 1
	clrf	ACCUH
	return
	
_NEQ:
	movf	ACCUL, W
	clrf	ACCUL
	bsf	ACCUL, 0
	cpfseq	PREINC2
	bsf	ACCUL, 1
	movf	ACCUH, W
	cpfseq	PREINC2
	bsf	ACCUL, 1
	clrf	ACCUH
	return
	
_LTINT:
	movf	ACCUH, W	 ; TMP_REG_1 <- ACCUH + 0x80
	addlw	0x80
	movwf	TMP_REG_1
	clrf	ACCUH		 ; ACCUH <- 0
	movf	[0x2], W	 ; W <- STKH + 0x80
	addlw	0x80
	subwf	TMP_REG_1, W	 ; TMP_REG_1 - W
	bnz	_LTINT_L0	 ; if TMP_REG_1 <> W then goto L0
	movf	[0x1], W
	subwf	ACCUL, W	 ; ACCUL - STKL
_LTINT_L0:
	bc	_POP_ACCUL_FALSE ; if C = 1 then ACCU <- FALSE
	bra	_POP_ACCUL_TRUE	 ; else ACCU <- TRUE
	
_LEINT:
	movlw	0x80 		 ; STKH <- STKH + 0x80
	addwf	[0x2], F
	movf	ACCUH, W	 ; W <- ACCUH + 0x80
	addlw	0x80
	clrf	ACCUH		 ; ACCUH <- 0
	subwf	[0x2], W 	 ; STKH - W
	bnz	_LEINT_L0	 ; if STKH <> W then goto L0
	movf	ACCUL, W
	subwf	[0x1], W 	 ; STKL - ACCUL
_LEINT_L0:
	bc	_POP_ACCUL_TRUE	 ; if C = 1 then ACCU <- TRUE
	bra	_POP_ACCUL_FALSE ; else ACCU <- FALSE
	
_GTINT:
	movlw	0x80 		 ; STKH <- STKH + 0x80
	addwf	[0x2], F
	movf	ACCUH, W	 ; W <- ACCUH + 0x80
	addlw	0x80
	clrf	ACCUH		 ; ACCUH <- 0
	subwf	[0x2], W 	 ; STKH - W
	bnz	_GTINT_L0	 ; if STKH <> W then goto L0
	movf	ACCUL, W
	subwf	[0x1], W 	 ; STKL - ACCUL
_GTINT_L0:
	bc	_POP_ACCUL_FALSE ; if C = 1 then ACCU <- FALSE
	bra	_POP_ACCUL_TRUE	 ; else ACCU <- TRUE
	
_GEINT:
	movf	ACCUH, W 	 ; TMP_REG_1 <- ACCUH + 0x80
	addlw	0x80
	movwf	TMP_REG_1
	clrf	ACCUH		 ; ACCUH <- 0
	movf	[0x2], W	 ; W <- STKH + 0x80
	addlw	0x80
	subwf	TMP_REG_1, W 	 ; TMP_REG_1 - W
	bnz	_GEINT_L0	 ; if TMP_REG_1 <> W then goto L0
	movf	[0x1], W
	subwf	ACCUL, W 	 ; ACCUL - STKL
_GEINT_L0:
	bc	_POP_ACCUL_TRUE	 ; if C = 1 then ACCU <- TRUE
	bra	_POP_ACCUL_FALSE ; else ACCU <- FALSE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;         OFFSETS / STACK           ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_OFFSETINT:
	M_READ_ARG
	addwf	ACCUL, F
	movf	TABLAT, W
	addwfc	ACCUH, F
	return

_OFFSETREF:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	M_READ_ARG
	addwf	POSTINC0, F
	movf	TABLAT, W
	addwfc	INDF0, F
	return

_ACC0_OFFSETREF:
	bc	_OFFSETREF
	M_ACC	0
	return
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;          OBJECTS / STACK          ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_GETMETHOD:
	movf	[0x1], W		; FSR0 <- stack_top
	movwf	FSR0L
	movf	[0x2], W
	movwf	FSR0H
	movff	POSTINC0, TMP_REG_1 	; TMP_REG_1 <- stack_top[0]L
	movf	INDF0, W		; W <- stack_top[0]H
	movwf	FSR0H			; FSR0 <- stack_top[0]
	movff	TMP_REG_1, FSR0L
	decf	ACCUL, W 		; FSR0 <- FSR0 + 2*val_int(ACCU)
	addwf	FSR0L, F
	movf	ACCUH, W
	addwfc	FSR0H, F
	movff	POSTINC0, ACCUL		; ACCU <- *FSR0
	movff	POSTINC0, ACCUH
	return

_ACC2_GETMETHOD:
	bc	_GETMETHOD
	M_ACC	2
	return

_GETPUBMET:
	M_PUSH
	tblrd*+
	movff	TABLAT, ACCUL
	tblrd*+
	movff	TABLAT, ACCUH		; fallthrough
_GETDYNMET:
	movsf	[0x1], FSR0L		; FSR0 <- stack top
	movsf	[0x2], FSR0H
	movff	POSTINC0, TMP_REG_4 	; methL = TMP_REG_4 <- low *FSR0
	movf	INDF0, W		; methH = TMP_REG_5 <- high *FSR0
	movwf	TMP_REG_5
	movwf	FSR0H			; FSR0 <- meth
	movff	TMP_REG_4, FSR0L
	movlw	0x3			; li = TMP_REG_1 <- 3
	movwf	TMP_REG_1
	movff	INDF0, TMP_REG_2 	; hi = TMP_REG_2 <- FSR0[0]
_GETMET_LOOP:
	movf	TMP_REG_1, W		; if li >= hi
	cpfsgt	TMP_REG_2
	bra	_GETMET_END_LOOP	; then goto _GETMET_END_LOOP else
	addwf	TMP_REG_2, W		; mi = TMP_REG_3 <- ((li + hi) >> 1) | 1
	movwf	TMP_REG_3
	rrcf	TMP_REG_3, F 		; (STATUS.C computed by addwf)
	bsf	TMP_REG_3, 0
	movff	TMP_REG_5, FSR0H	; FSR0 <- TMP_REG_4:5 + TMP_REG_3 * 2
	bcf	STATUS, C
	rlcf	TMP_REG_3, W
	btfsc	STATUS, C
	incf	FSR0H, F
	addwf	TMP_REG_4, W
	movwf	FSR0L
	btfsc	STATUS, C
	incf	FSR0H, F
	addfsr	FSR0, 0x1
	movf	POSTDEC0, W		; if ACCU < *FSR0
	subwf	ACCUH, W
	bnc	_GETMET_THEN		; then goto _GETMET_THEN
	bnz	_GETMET_ELSE
	movf	INDF0, W
	subwf	ACCUL, W
	bnc	_GETMET_THEN		; then goto _GETMET_THEN
_GETMET_ELSE:
	movff	TMP_REG_3, TMP_REG_1 	; else li <- mi
	bra	_GETMET_LOOP		; continue loop
_GETMET_THEN:
	movlw	0x2			; hi <- mi - 2
	subwf	TMP_REG_3, W
	movwf	TMP_REG_2
	bra	_GETMET_LOOP		; continue loop
_GETMET_END_LOOP:
	movff	TMP_REG_5, FSR0H 	; FSR0 <- TMP_REG_4:5 + 2*(li & 0xFE)
	bcf	TMP_REG_1, 0
	bcf	STATUS, C
	rlcf	TMP_REG_1, W
	btfsc	STATUS, C
	incf	FSR0H, F
	addwf	TMP_REG_4, W
	movwf	FSR0L
	btfsc	STATUS, C
	incf	FSR0H, F
	movff	POSTINC0, ACCUL		; ACCU <- *FSR0
	movff	INDF0, ACCUH
	return

_PUSHACC3_GETPUBMET:
	bc	_GETPUBMET
	M_PUSH
	M_ACC	3
	return
	
_PUSHACC4_GETDYNMET:
	bc	_GETDYNMET
	M_PUSH
	M_ACC	4
	return

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;           ENVIRONMENT             ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_PUSHENVACC1:
	M_PUSH			; fallthrough
_ENVACC1:
	movff	ENVL, FSR0L
	movff	ENVH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_PUSHENVACC2:
	M_PUSH			; fallthrough
_ENVACC2:
	movff	ENVL, FSR0L
	movff	ENVH, FSR0H
	addfsr	FSR0, 0x4
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_PUSHENVACC3:
	M_PUSH			; fallthrough
_ENVACC3:
	movff	ENVL, FSR0L
	movff	ENVH, FSR0H
	addfsr	FSR0, 0x6
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_PUSHENVACC4:
	M_PUSH			; fallthrough
_ENVACC4:
	movff	ENVL, FSR0L
	movff	ENVH, FSR0H
	addfsr	FSR0, 0x8
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_PUSHENVACC:
	M_PUSH			; fallthrough
_ENVACC:
	movff	ENVL, FSR0L
	movff	ENVH, FSR0H
	tblrd*+
	movf	TABLAT, W
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               FIELDS              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_GETFIELD0:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_GETFIELD1:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_GETFIELD2:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x4
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_GETFIELD3:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x6
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_PUSHGETGLOBALFIELD:
	M_PUSH			; fallthrough
_GETGLOBALFIELD:
	M_GETGLOBAL		; fallthrough
_GETFIELD:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	tblrd*+
	movf	TABLAT, W
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_SETFIELD0:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	movff	PREINC2, POSTINC0
	movff	PREINC2, INDF0
	M_CONST	0
	return

_SETFIELD1:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	PREINC2, POSTINC0
	movff	PREINC2, INDF0
	M_CONST	0
	return

_SETFIELD2:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x4
	movff	PREINC2, POSTINC0
	movff	PREINC2, INDF0
	M_CONST	0
	return

_SETFIELD3:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x6
	movff	PREINC2, POSTINC0
	movff	PREINC2, INDF0
	M_CONST	0
	return

_SETFIELD:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	tblrd*+
	movf	TABLAT, W
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	movff	PREINC2, POSTINC0
	movff	PREINC2, INDF0
	M_CONST	0
	return

_VECTLENGTH:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	clrf	ACCUH
	subfsr	FSR0, 0x2
	movf	POSTINC0, W
	xorlw	DOUBLE_ARRAY_TAG
	bz	_VECTLENGTH_DOUBLE_ARRAY
	bsf	STATUS, C
	rlcf	INDF0, W
	movwf	ACCUL
	rlcf	ACCUH, F
	return
_VECTLENGTH_DOUBLE_ARRAY:
	movff	INDF0, ACCUL
	bsf	ACCUL, 0
	return

_GETVECTITEM:
	decf	PREINC2, W
	addwf	ACCUL, W
	movwf	FSR0L
	movf	ACCUH, W
	addwfc	PREINC2, W
	movwf	FSR0H
	movff	POSTINC0, ACCUL
	movff	INDF0, ACCUH
	return

_SETVECTITEM:
	decf	PREINC2, W
	addwf	ACCUL, W
	movwf	FSR0L
	movf	ACCUH, W
	addwfc	PREINC2, W
	movwf	FSR0H
	movff	PREINC2, POSTINC0
	movff	PREINC2, INDF0
	M_CONST	0
	return
	
_GETSTRINGCHAR:
	bcf	STATUS, C
	rrcf	[0x2], F
	rrcf	PREINC2, W
	addwf	ACCUL, W
	movwf	FSR0L
	movf	ACCUH, W
	addwfc	PREINC2, W
	movwf	FSR0H
	clrf	ACCUH
	bsf	STATUS, C
	rlcf	INDF0, W
	rlcf	ACCUH, F
	movwf	ACCUL
	return

_SETSTRINGCHAR:
	bcf	STATUS, C
	rrcf	[0x2], F
	rrcf	[0x1], W
	addwf	ACCUL, W
	movwf	FSR0L
	movf	ACCUH, W
	addwfc	[0x2], W
	movwf	FSR0H
	rrcf	[0x4], W ; STATUS.C ignored
	rrcf	[0x3], W
	movwf	INDF0
	M_CONST	0
	addulnk	0x4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              CONSTANTS            ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_PUSHCONST0:
	M_PUSH
_CONST0:
	M_CONST 0
	return

_PUSHCONST1:
	M_PUSH			; fallthrough
_CONST1:
	M_CONST	1
	return

_PUSHCONST2:
	M_PUSH			; fallthrough
_CONST2:
	M_CONST	2
	return

_PUSHCONST3:
	M_PUSH			; fallthrough
_CONST3:
	M_CONST	3
	return

_PUSHCONST:
	M_PUSH			; fallthrough
_CONSTINT:
	tblrd*+
	movff	TABLAT, ACCUL
	tblrd*+
	movff	TABLAT, ACCUH
	return
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              LOGICAL              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_ANDINT:
	movf	PREINC2, W
	andwf	ACCUL, F
	movf	PREINC2, W
	andwf	ACCUH, F
	return

_ORINT:
	movf	PREINC2, W
	iorwf	ACCUL, F
	movf	PREINC2, W
	iorwf	ACCUH, F
	return

_XORINT:
	movf	PREINC2, W
	xorwf	ACCUL, F
	movf	PREINC2, W
	xorwf	ACCUH, F
	bsf	ACCUL, 0
	return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              SHIFTS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_LSLINT:
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	_END_LSLINT
	bcf	ACCUL, 0
_LOOP_LSLINT:
	bcf	STATUS, C
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	decfsz	[0x1], F
	bra	_LOOP_LSLINT
	bsf	ACCUL, 0
_END_LSLINT:
	addulnk	0x2
	
_LSRINT:
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	_END_LSRINT
_LOOP_LSRINT:
	bcf	STATUS, C
	rrcf	ACCUH, F
	rrcf	ACCUL, F
	decfsz	[0x1], F
	bra	_LOOP_LSRINT
	bsf	ACCUL, 0
_END_LSRINT:
	addulnk	0x2
	
_ASRINT:
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	_END_ASRINT
_LOOP_ASRINT:
	rlcf	ACCUH, W ; STATUS.C ignored
	rrcf	ACCUH, F
	rrcf	ACCUL, F
	decfsz	[0x1], F
	bra	_LOOP_ASRINT
	bsf	ACCUL, 0
_END_ASRINT:
	addulnk	0x2
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               CCALLS              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_CCALL1:
	M_CCALL
	return

_CCALL2:
	M_CCALL
	addulnk	0x2

_CCALL3:
	M_CCALL
	addulnk	0x4

_CCALL4:
	M_CCALL
	addulnk	0x6

_CCALL5:
	M_CCALL
	addulnk	0x8

_CCALL:
	M_CCALL
	tblrd*+
	movf	TABLAT, W
	addwf	FSR2L, F
	btfsc	STATUS, C
	incf	FSR2H, F
	addwf	FSR2L, F
	btfsc	STATUS, C
	incf	FSR2H, F
	return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              JUMPS                ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_BRANCH:
	M_READ_ARG
	movwf	TBLPTRL
	movf	TABLAT, W
	movwf	TBLPTRH
	return

_BRANCHIF:
	decf	ACCUL, W
	iorwf	ACCUH, W
	bnz	_BRANCH
	movlw	0x2
	addwf	TBLPTRL, F
	btfsc	STATUS, C
	incf	TBLPTRH, F
	return

_BRANCHIFNOT:
	decf	ACCUL, W
	iorwf	ACCUH, W
	bz	_BRANCH
	movlw	0x2
	addwf	TBLPTRL, F
	btfsc	STATUS, C
	incf	TBLPTRH, F
	return

_SWITCH:
	btfsc	ACCUL, 0	  ; if isint(ACCU)
	bra	_SWITCH_LONG	  ; then goto _SWITCH_LONG else
	tblrd*+			  ; TABLAT <- sizeLong
	movf	TABLAT, W	  ; TBLPTR <- TBLPTR + 2*sizeLong
	addwf	TBLPTRL, F
	btfsc	STATUS, C
	incf	TBLPTRH, F
	addwf	TBLPTRL, F
	btfsc	STATUS, C
	incf	TBLPTRH, F
	movff	ACCUL, FSR0L	  ; TBLPTR <- TBLPTR + 2*tag(ACCU)
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2
	movf	INDF0, W
	addwf	TBLPTRL, F
	btfsc	STATUS, C
	incf	TBLPTRH, F
	addwf	TBLPTRL, F
	btfsc	STATUS, C
	incf	TBLPTRH, F
	M_READ_ARG		  ; TBLPTR <- *TBLPTR
	movff	TABLAT, TBLPTRH
	movwf	TBLPTRL
	return
_SWITCH_LONG:
	movf	ACCUL, W	  ; TBLPTR <- TBLPTR + 2*int_val(ACCU) + 1
	addwf	TBLPTRL, F
	movf	ACCUH, W
	addwfc	TBLPTRH, F
	M_READ_ARG		  ; TBLPTR <- *TBLPTR
	movwf	TBLPTRL
	movff	TABLAT, TBLPTRH
	return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;             GLOBALS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_SETGLOBAL:
	lfsr	FSR0, STACK_ANCHOR
	M_READ_ARG
	subwf	FSR0L, F
	movf	TABLAT, W
	subwfb	FSR0H, F
	movff	ACCUH, POSTDEC0
	movff	ACCUL, POSTDEC0
	return

_PUSHGETGLOBAL:
	M_PUSH			; fallthrough
_GETGLOBAL:
	M_GETGLOBAL
	return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               TOOLS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_INDIRECT_CAML_RAISE_STACK_OVERFLOW:
	bra	_INDIRECT_CAML_RAISE_STACK_OVERFLOW2	

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;          THE BIG SWITCH           ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	org	0x900
_THE_BIG_SWITCH:
        bra     _ACC0_OFFSETREF      ; opcode = 0  or 128
        bra     _ACC1_ISINT          ; opcode = 1  or 129
        bra     _ACC2_GETMETHOD      ; opcode = 2  or 130
        bra     _ACC3_BEQ            ; opcode = 3  or 131
        bra     _ACC4_BNEQ           ; opcode = 4  or 132
        bra     _ACC5_BLTINT         ; opcode = 5  or 133
        bra     _ACC6_BLEINT         ; opcode = 6  or 134
        bra     _ACC7_BGTINT         ; opcode = 7  or 135
        bra     _ACC_BGEINT          ; opcode = 8  or 136
        bra     _PUSH_ULTINT         ; opcode = 9  or 137
        bra     _PUSHACC0_UGEINT     ; opcode = 10 or 138
        bra     _PUSHACC1_BULTINT    ; opcode = 11 or 139
        bra     _PUSHACC2_BUGEINT    ; opcode = 12 or 140
        bra     _PUSHACC3_GETPUBMET  ; opcode = 13 or 141
        bra     _PUSHACC4_GETDYNMET  ; opcode = 14 or 142
        bra     _PUSHACC5_STOP       ; opcode = 15 or 143
        bra     _PUSHACC6_EVENT      ; opcode = 16 or 144
        bra     _PUSHACC7_BREAK      ; opcode = 17 or 145
        bra     _PUSHACC_INTRETURN   ; opcode = 18 or 146
        bra     _POP_INTRAISE        ; opcode = 19 or 147
        bra     _ASSIGN              ; opcode = 20
        bra     _ENVACC1             ; opcode = 21
        bra     _ENVACC2             ; opcode = 22
        bra     _ENVACC3             ; opcode = 23
        bra     _ENVACC4             ; opcode = 24
        bra     _ENVACC              ; opcode = 25
        bra     _PUSHENVACC1         ; opcode = 26
        bra     _PUSHENVACC2         ; opcode = 27
        bra     _PUSHENVACC3         ; opcode = 28
        bra     _PUSHENVACC4         ; opcode = 29
        bra     _PUSHENVACC          ; opcode = 30
        bra     _PUSHRETADDR         ; opcode = 31
        bra     _APPLY               ; opcode = 32
        bra     _APPLY1              ; opcode = 33
        bra     _APPLY2              ; opcode = 34
        bra     _APPLY3              ; opcode = 35
        bra     _APPTERM             ; opcode = 36
        bra     _APPTERM1            ; opcode = 37
        bra     _APPTERM2            ; opcode = 38
        bra     _APPTERM3            ; opcode = 39
        bra     _RETURN              ; opcode = 40
        bra     _RESTART             ; opcode = 41
        bra     _GRAB                ; opcode = 42
        bra     _CLOSURE             ; opcode = 43
        bra     _CLOSUREREC          ; opcode = 44
        bra     _OFFSETCLOSUREM2     ; opcode = 45
        bra     _OFFSETCLOSURE0      ; opcode = 46
        bra     _OFFSETCLOSURE2      ; opcode = 47
        bra     _OFFSETCLOSURE       ; opcode = 48
        bra     _PUSHOFFSETCLOSUREM2 ; opcode = 49
        bra     _PUSHOFFSETCLOSURE0  ; opcode = 50
        bra     _PUSHOFFSETCLOSURE2  ; opcode = 51
        bra     _PUSHOFFSETCLOSURE   ; opcode = 52
        bra     _GETGLOBAL           ; opcode = 53
        bra     _PUSHGETGLOBAL       ; opcode = 54
        bra     _GETGLOBALFIELD      ; opcode = 55
        bra     _PUSHGETGLOBALFIELD  ; opcode = 56
        bra     _SETGLOBAL           ; opcode = 57
        bra     _ATOM0               ; opcode = 58
        bra     _ATOM                ; opcode = 59
        bra     _PUSHATOM0           ; opcode = 60
        bra     _PUSHATOM            ; opcode = 61
        bra     _MAKEBLOCK           ; opcode = 62
        bra     _MAKEBLOCK1          ; opcode = 63
        bra     _MAKEBLOCK2          ; opcode = 64
        bra     _MAKEBLOCK3          ; opcode = 65
        bra     _MAKEFLOATBLOCK      ; opcode = 66
        bra     _GETFIELD0           ; opcode = 67
        bra     _GETFIELD1           ; opcode = 68
        bra     _GETFIELD2           ; opcode = 69
        bra     _GETFIELD3           ; opcode = 70
        bra     _GETFIELD            ; opcode = 71
        bra     _GETFLOATFIELD       ; opcode = 72
        bra     _SETFIELD0           ; opcode = 73
        bra     _SETFIELD1           ; opcode = 74
        bra     _SETFIELD2           ; opcode = 75
        bra     _SETFIELD3           ; opcode = 76
        bra     _SETFIELD            ; opcode = 77
        bra     _SETFLOATFIELD       ; opcode = 78
        bra     _VECTLENGTH          ; opcode = 79
        bra     _GETVECTITEM         ; opcode = 80
        bra     _SETVECTITEM         ; opcode = 81
        bra     _GETSTRINGCHAR       ; opcode = 82
        bra     _SETSTRINGCHAR       ; opcode = 83
        bra     _BRANCH              ; opcode = 84
        bra     _BRANCHIF            ; opcode = 85
        bra     _BRANCHIFNOT         ; opcode = 86
        bra     _SWITCH              ; opcode = 87
        bra     _BOOLNOT             ; opcode = 88
        bra     _PUSHTRAP            ; opcode = 89
        bra     _POPTRAP             ; opcode = 90
        bra     _RAISE               ; opcode = 91
        bra     _CHECKSIGNALS        ; opcode = 92
        bra     _CCALL1              ; opcode = 93
        bra     _CCALL2              ; opcode = 94
        bra     _CCALL3              ; opcode = 95
        bra     _CCALL4              ; opcode = 96
        bra     _CCALL5              ; opcode = 97
        bra     _CCALL               ; opcode = 98
        bra     _CONST0              ; opcode = 99
        bra     _CONST1              ; opcode = 100
        bra     _CONST2              ; opcode = 101
        bra     _CONST3              ; opcode = 102
        bra     _CONSTINT            ; opcode = 103
        bra     _PUSHCONST0          ; opcode = 104
        bra     _PUSHCONST1          ; opcode = 105
        bra     _PUSHCONST2          ; opcode = 106
        bra     _PUSHCONST3          ; opcode = 107
        bra     _PUSHCONST           ; opcode = 108
        bra     _NEGINT              ; opcode = 109
        bra     _ADDINT              ; opcode = 110
        bra     _SUBINT              ; opcode = 111
        bra     _MULINT              ; opcode = 112
        bra     _DIVINT              ; opcode = 113
        bra     _MODINT              ; opcode = 114
        bra     _ANDINT              ; opcode = 115
        bra     _ORINT               ; opcode = 116
        bra     _XORINT              ; opcode = 117
        bra     _LSLINT              ; opcode = 118
        bra     _LSRINT              ; opcode = 119
        bra     _ASRINT              ; opcode = 120
        bra     _EQ                  ; opcode = 121
        bra     _NEQ                 ; opcode = 122
        bra     _LTINT               ; opcode = 123
        bra     _LEINT               ; opcode = 124
        bra     _GTINT               ; opcode = 125
        bra     _GEINT               ; opcode = 126
        bra     _OFFSETINT           ; opcode = 127

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               TOOLS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_INDIRECT_CAML_RAISE_STACK_OVERFLOW2:
        goto    caml_raise_stack_overflow

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              EXCEPTIONS           ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_PUSHTRAP:
	M_CHECK_UNFULL_STACK 0x4
	clrf	POSTDEC2		; push extraArgs
	movff	EXTRA_ARGS, POSTDEC2
	movff	ENVH, POSTDEC2		; push environment
	movff	ENVL, POSTDEC2
	movff	TRAP_SPH, POSTDEC2	; push trapSp
	movff	TRAP_SPL, POSTDEC2
	M_READ_ARG			; push catchPc
	movff	TABLAT, POSTDEC2
	movwf	POSTDEC2
	movff	FSR2L, TRAP_SPL		; trapSp <- top of stack
	movff	FSR2H, TRAP_SPH
	return

_POPTRAP:
	movf	[0x3], W		; restore trapSp
	movwf	TRAP_SPL
	movf	[0x4], W
	movwf	TRAP_SPH
	addulnk	0x8			; pop 4 elements and return

_RAISE:
	decf	TRAP_SPL, W		; trapSp = 0 ?
	iorwf	TRAP_SPH, W
	btfsc	STATUS, Z
	bra	_STOP			; yes -> stop
	movff	TRAP_SPL, FSR2L		; no -> restore stack top
	movff	TRAP_SPH, FSR2H
	movff	PREINC2, TBLPTRL 	; set code pointer
	movff	PREINC2, TBLPTRH
	movff	PREINC2, TRAP_SPL 	; restore trapSp
	movff	PREINC2, TRAP_SPH
	movff	PREINC2, ENVL		; restore environment
	movff	PREINC2, ENVH
	movff	PREINC2, EXTRA_ARGS 	; restore extraArgs
	addulnk	0x1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              CLOSURES             ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_CLOSURE:
	tblrd*+				; read closure size (= nbArg + 1)
	call	_CHECK_UNFULL_HEAP_GEN
	movlw	CLOSURE_TAG		; write tag
	movwf	POSTINC1
	movf	TABLAT, W		; write size
	movwf	POSTINC1
	movwf	TMP_REG_1		; TMP_REG_1 <- size
	movff	ACCUL, TMP_REG_2	; TMP_REG_3:2 <- ACCU
	movff	ACCUH, TMP_REG_3
	movff	FSR1L, ACCUL		; ACCU <- closure
	movff	FSR1H, ACCUH
	tblrd*+				; read/write closure code pointer
	movff	TABLAT, POSTINC1
	tblrd*+
	movff	TABLAT, POSTINC1
	dcfsnz	TMP_REG_1, F		; decr TMP_REG_1
	return				; if nbArg = 0 then closure done
	movff	TMP_REG_2, POSTINC1	; closure[1] <- TMP_REG_3:2
	movff	TMP_REG_3, POSTINC1
	dcfsnz	TMP_REG_1, F		; decf TMP_REG_1
	return				; if nbArg = 1 then closure done
_CLOSURE_LOOP:
	movff	PREINC2, POSTINC1 	; closure[i] <- pop ()
	movff	PREINC2, POSTINC1
	decfsz	TMP_REG_1, F		; loop nbArg - 1 times
	bra	_CLOSURE_LOOP
	return				; closure done

_CLOSUREREC:
	tblrd*+				; read nbFuncs
	decf	TABLAT, W
	movwf	TMP_REG_1	 	; TMP_REG_1 <- nbFuncs - 1
	tblrd*+				; read nbVars
	movff	TABLAT, TMP_REG_2 	; TMP_REG_2 <- nbVars
	incf	TMP_REG_1, W		; compute size: 2 * nbFuncs - 1 + nbVars
	addwf	TMP_REG_1, W
	addwf	TMP_REG_2, W
	movwf	TMP_REG_3		; TMP_REG_3 <- closure size
	movwf	TABLAT			; TABLAT <- closure size
	call	_CHECK_UNFULL_HEAP_GEN	; check allocation
	movlw	CLOSURE_TAG		; write tag
	movwf	POSTINC1
	movf	TMP_REG_3, W		; write size
	movwf	POSTINC1
	movff	ACCUL, TMP_REG_3 	; TMP_REG_4:3 <- ACCU
	movff	ACCUH, TMP_REG_4
	movff	FSR1L, ACCUL		; ACCU <- @closure
	movff	FSR1H, ACCUH
	tblrd*+				; closure[0] <- closure pc
	movff	TABLAT, POSTINC1
	tblrd*+
	movff	TABLAT, POSTINC1
	movff	FSR1L, FSR0L		; FSR0 <- FSR1
	movff	FSR1H, FSR0H
	movf	TMP_REG_1, W		; FSR1 += 4 * (nbFuncs - 1)
	addwf	TMP_REG_1, W
	addwf	FSR1L, F
	btfsc	STATUS, C
	incf	FSR1H, F
	addwf	FSR1L, F
	btfsc	STATUS, C
	incf	FSR1H, F
	movf	TMP_REG_2, F		; nbVars = 0 ?
	bz	_CLOSUREREC_NO_VARS	; yes => no var to copy
	movff	TMP_REG_3, POSTINC1	; closure[i] <- TMP_REG_4:3 (old ACCU)
	movff	TMP_REG_4, POSTINC1
	decf	TMP_REG_2, F		; decr TMP_REG_2
	bz	_CLOSUREREC_NO_VARS	; if no var to copy => skip loop
_CLOSUREREC_LOOP_VARS:
	movff	PREINC2, POSTINC1 	; closure[i] = pop ()
	movff	PREINC2, POSTINC1
	decfsz	TMP_REG_2, F		; loop nbVars - 1 times
	bra	_CLOSUREREC_LOOP_VARS
_CLOSUREREC_NO_VARS:
	M_PUSH				; push the accumulator
	movf	TMP_REG_1, F
	btfsc	STATUS, Z		; if nbFuncs = 1
	return				; then closurerec done
	clrf	TMP_REG_5		; TMP_REG_5 <- 0
_CLOSUREREC_LOOP_FUNCS:
	incf	TMP_REG_5, F		; TMP_REG_5 ++
	movlw	INFIX_TAG		; write infix tag in infix header
	movwf	POSTINC0
	movf	TMP_REG_5, W		; write 2 * TMP_REG_5 as infix size in
	addwf	TMP_REG_5, W		; infix header
	movwf	POSTINC0
	movff	FSR0L, TMP_REG_6 	; save FSR0
	movff	FSR0H, TMP_REG_7
	M_CHECK_UNFULL_STACK 0x1
	movf	TMP_REG_7, W	 	; restore FSR0
	movwf	FSR0H
	movwf	POSTDEC2
	movf	TMP_REG_6, W
	movwf	FSR0L
	movwf	POSTDEC2
	tblrd*+				; infix_block[0] <- infix_pc read in bc
	movff	TABLAT, POSTINC0
	tblrd*+
	movff	TABLAT, POSTINC0
	decfsz	TMP_REG_1, F	  	; loop nbFuncs - 1 times
	bra	_CLOSUREREC_LOOP_FUNCS
	return

_PUSHOFFSETCLOSUREM2:
	M_PUSH			; fallthrough
_OFFSETCLOSUREM2:
	movlw	-0x4		; ACCU <- ENV - 4
	addwf	ENVL, W
	movwf	ACCUL
	movf	ENVH, W
	btfss	STATUS, C
	addlw	-0x1
	movwf	ACCUH
	return

_PUSHOFFSETCLOSURE0:
	M_PUSH			; fallthrough
_OFFSETCLOSURE0:
	movff	ENVL, ACCUL
	movff	ENVH, ACCUH
	return

_PUSHOFFSETCLOSURE2:
	M_PUSH			; fallthrough
_OFFSETCLOSURE2:
	movlw	0x4		; ACCU <- ENV + 4
	addwf	ENVL, W
	movwf	ACCUL
	movf	ENVH, W
	btfsc	STATUS, C
	addlw	0x1
	movwf	ACCUH
	return

_PUSHOFFSETCLOSURE:
	M_PUSH			; fallthrough
_OFFSETCLOSURE:
	movff	ENVH, ACCUH	; ACCU <- ENV + 4 * offset
	movff	ENVL, ACCUL
	tblrd*+			; read offset
	bcf	STATUS, C
	rlcf	TABLAT, W
	bc	_OFFSETCLOSURE_NEG
	addwf	ACCUL, F
	btfsc	STATUS, C
	incf	ACCUH, F
	addwf	ACCUL, F
	btfsc	STATUS, C
	incf	ACCUH, F
	return
_OFFSETCLOSURE_NEG:
	sublw	0x0
	subwf	ACCUL, F
	btfss	STATUS, C
	decf	ACCUH, F
	subwf	ACCUL, F
	btfss	STATUS, C
	decf	ACCUH, F
	return
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;           CALL/RETURN             ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_PUSHRETADDR:
	M_CHECK_UNFULL_STACK 0x3
	clrf	POSTDEC2		; push extraArgs
	movff	EXTRA_ARGS, POSTDEC2
	movff	ENVH, POSTDEC2		; push the environment
	movff	ENVL, POSTDEC2
	M_READ_ARG			; push the return address
	movff	TABLAT, POSTDEC2
	movwf	POSTDEC2
	return

_APPLY:
	tblrd*+				; read extraArgs
	movff	TABLAT, EXTRA_ARGS
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	return

_APPLY1:
	M_CHECK_UNFULL_STACK 0x3
	subfsr	FSR2, 0x6
	movss	[0x7], [0x1]		; move arg (stack[0] <- stack[3])
	movss	[0x8], [0x2]
	clrf	[0x8]			; stack[3] <- extraArgs
	movf	EXTRA_ARGS, W
	movwf	[0x7]
	movf	ENVL, W			; stack[2] <- environment
	movwf	[0x5]
	movf	ENVH, W
	movwf	[0x6]
	movf	TBLPTRL, W		; stack[1] <- PC
	movwf	[0x3]
	movf	TBLPTRH, W
	movwf	[0x4]
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	movlw	0x1			; extraArgs <- int_val(0)
	movwf	EXTRA_ARGS
	return

_APPLY2:
	M_CHECK_UNFULL_STACK 0x3
	subfsr	FSR2, 0x6
	movss	[0x7], [0x1]		; move arg (stack[0] <- stack[3])
	movss	[0x8], [0x2]
	movss	[0x9], [0x3]		; move arg (stack[1] <- stack[4])
	movss	[0xA], [0x4]
	clrf	[0xA]			; stack[4] <- int_val(extraArgs)
	movf	EXTRA_ARGS, W
	movwf	[0x9]
	movf	ENVL, W			; stack[3] <- environment
	movwf	[0x7]
	movf	ENVH, W
	movwf	[0x8]
	movf	TBLPTRL, W		; stack[2] <- PC
	movwf	[0x5]
	movf	TBLPTRH, W
	movwf	[0x6]
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	movlw	0x3			; extraArgs <- int_val(1)
	movwf	EXTRA_ARGS
	return

_APPLY3:
	M_CHECK_UNFULL_STACK 0x3
	subfsr	FSR2, 0x6
	movss	[0x7], [0x1]		; move arg (stack[0] <- stack[3])
	movss	[0x8], [0x2]
	movss	[0x9], [0x3]		; move arg (stack[1] <- stack[4])
	movss	[0xA], [0x4]
	movss	[0xB], [0x5]		; move arg (stack[2] <- stack[5])
	movss	[0xC], [0x6]
	clrf	[0xC]			; stack[5] <- int_val(extraArgs)
	movf	EXTRA_ARGS, W
	movwf	[0xB]
	movf	ENVL, W			; stack[4] <- environment
	movwf	[0x9]
	movf	ENVH, W
	movwf	[0xA]
	movf	TBLPTRL, W		; stack[3] <- PC
	movwf	[0x7]
	movf	TBLPTRH, W
	movwf	[0x8]
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	movlw	0x5			; extraArgs <- int_val(2)
	movwf	EXTRA_ARGS
	return

_APPTERM:
	movff	FSR2L, FSR0L		; FSR0 <- FSR2
	movff	FSR2H, FSR0H
	tblrd*+				; read (2 * s)
	movf	TABLAT, W		; pop s elements
	addwf	FSR2L, F
	btfsc	STATUS, C
	incf	FSR2H, F
	tblrd*+				; read n
	bcf	STATUS, C
	rlcf	TABLAT, W		; FSR0 += 2 * n
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	decf	TABLAT, W		; extraArgs <- extraArgs + n - 1
	addwf	EXTRA_ARGS, F
	addwf	EXTRA_ARGS, F
_APPTERM_LOOP:
	movff	POSTDEC0, POSTDEC2	; push (*FSR0--)
	movff	POSTDEC0, POSTDEC2
	decfsz	TABLAT, F		; loop n times
	bra	_APPTERM_LOOP
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	return
	
_APPTERM1:
	movff	FSR2L, FSR0L		; FSR0 <- FSR2
	movff	FSR2H, FSR0H
	tblrd*+				; read (2 * extraArgs)
	movf	TABLAT, W		; extraArgs elements
	addwf	FSR2L, F
	btfsc	STATUS, C
	incf	FSR2H, F
	addfsr	FSR0, 0x2	   	; FSR0 -= 2
	movff	POSTDEC0, POSTDEC2 	; push (*FSR0--)
	movff	POSTDEC0, POSTDEC2
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	return
	
_APPTERM2:
	movff	FSR2L, FSR0L		; FSR0 <- FSR2
	movff	FSR2H, FSR0H
	tblrd*+				; read (2 * extraArgs)
	movf	TABLAT, W		; pop extraArgs elements
	addwf	FSR2L, F
	btfsc	STATUS, C
	incf	FSR2H, F
	addfsr	FSR0, 0x4		; FSR0 -= 4
	movff	POSTDEC0, POSTDEC2 	; push (*FSR0--)
	movff	POSTDEC0, POSTDEC2
	movff	POSTDEC0, POSTDEC2 	; push (*FSR0--)
	movff	POSTDEC0, POSTDEC2
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	movlw	0x2			; incr extraArgs
	addwf	EXTRA_ARGS, F
	return
	
_APPTERM3:
	movff	FSR2L, FSR0L		; FSR0 <- FSR2
	movff	FSR2H, FSR0H
	tblrd*+				; read (2 * (extraArgs - 3))
	movf	TABLAT, W		; pop (extraArgs - 3) elements
	addwf	FSR2L, F
	btfsc	STATUS, C
	incf	FSR2H, F
	addfsr	FSR0, 0x6	   	; FSR0 -= 6
	movff	POSTDEC0, POSTDEC2 	; push (*FSR0--)
	movff	POSTDEC0, POSTDEC2
	movff	POSTDEC0, POSTDEC2 	; push (*FSR0--)
	movff	POSTDEC0, POSTDEC2
	movff	POSTDEC0, POSTDEC2 	; push (*FSR0--)
	movff	POSTDEC0, POSTDEC2
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	movlw	0x4			; extraArgs <- extraArgs + 2
	addwf	EXTRA_ARGS, F
	return

_RETURN:
	tblrd*+				; read (2 * n)
	movf	TABLAT, W		; pop n elements
	addwf	FSR2L, F
	btfsc	STATUS, C
	incf	FSR2H, F
	dcfsnz	EXTRA_ARGS, W		; if extraArgs = 0
	bra	_RETURN_ELSE		; then goto _RETURN_ELSE
	movlw	0x2			; decr extraArgs
	subwf	EXTRA_ARGS, F
	movf	ACCUL, W		; TBLPTR <- ACCU[0] ; ENV <- ACCU
	movwf	FSR0L
	movwf	ENVL
	movf	ACCUH, W
	movwf	FSR0H
	movwf	ENVH
	movff	POSTINC0, TBLPTRL
	movff	INDF0, TBLPTRH
	return
_RETURN_ELSE:
	movff	PREINC2, TBLPTRL 	; TBLPTR <- pop ()
	movff	PREINC2, TBLPTRH
	movff	PREINC2, ENVL		; ENV <- pop ()
	movff	PREINC2, ENVH
	movff	PREINC2, EXTRA_ARGS 	; extraArgs <- pop ()
	addulnk	0x1

_RESTART:
	movff	ENVL, FSR0L		; FSR0 <- ENV
	movff	ENVH, FSR0H
	addfsr	FSR0, 0x2		; ENV <- FSR0[1]
	movff	POSTINC0, ENVL
	movff	INDF0, ENVH
	subfsr	FSR0, 0x4		; compute (size(FSR0) - 2)
	movf	POSTINC0, W
	addlw	-0x2
	btfsc	STATUS, Z		; if no arg to push then done
	return
	movwf	TMP_REG_1     		; TMP_REG_1 <- size(FSR0) - 2
	bcf	STATUS, C		; extraArgs <- extraArgs + size(FSR0)-2
	rlcf	TMP_REG_1, W
	addwf	EXTRA_ARGS, F
	addwf	FSR0L, F		; FSR0 <- FSR0 + 2 * size(ENV) - 1
	btfsc	STATUS, C
	incf	FSR0H, F
	addfsr	FSR0, 0x3
_RESTART_LOOP:
	movff	POSTDEC0, POSTDEC2	; push *FSR0--
	movff	POSTDEC0, POSTDEC2
	decfsz	TMP_REG_1, F		; loop size(FSR0) - 2 times
	bra	_RESTART_LOOP
	return

_GRAB:
	tblrd*+				; read required
	bcf	STATUS, C
	rlcf	TABLAT, W
	cpfslt	EXTRA_ARGS		; if extraArgs >= required
	bra	_GRAB_ELSE		; then goto _GRAB_ELSE
	rrcf	EXTRA_ARGS, W		; TABLAT = 3 + extraArgs ; STATUS.C = 0
	addlw	0x3
	movwf	TABLAT
	rcall	_CHECK_UNFULL_HEAP_GEN 	; check heap allocation
	movlw	CLOSURE_TAG		; write tag
	movwf	POSTINC1
	movf	TABLAT, W		; write size
	movwf	POSTINC1
	movff	FSR1L, ACCUL		; ACCU <- closure
	movff	FSR1H, ACCUH
	movf	TBLPTRL, W		; closure[0] <- PC - 3
	addlw	-0x3
	movwf	POSTINC1
	movf	TBLPTRH, W
	btfss	STATUS, C
	addlw	-0x1
	movwf	POSTINC1
	movff	ENVL, POSTINC1		; closure[1] <- ENV
	movff	ENVH, POSTINC1
	movlw	0x2			; TABLAT -= 2
	subwf	TABLAT, F
	bz	_GRAB_SKIP_LOOP		; skip loop if TABLAT = 0
_GRAB_LOOP:
	movff	PREINC2, POSTINC1 	; closure[i] <- pop ()
	movff	PREINC2, POSTINC1
	decfsz	TABLAT, F		; loop TABLAT = required - 1 times
	bra	_GRAB_LOOP
_GRAB_SKIP_LOOP:
	movff	PREINC2, TBLPTRL 	; TBLPTR <- pop ()
	movff	PREINC2, TBLPTRH
	movff	PREINC2, ENVL		; ENV <- pop ()
	movff	PREINC2, ENVH
	movff	PREINC2, EXTRA_ARGS 	; extraArgs <- pop ()
	addulnk	0x1			; grab done
_GRAB_ELSE:
	subwf	EXTRA_ARGS, F		; extraArgs -= required
	return				; grab done


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;          OTHERS / STACK           ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_ISINT:
	movlw	0x1
	btfsc	ACCUL, 0
	movlw	0x3
	movwf	ACCUL
	clrf	ACCUH
	return

_ACC1_ISINT:
	bc	_ISINT
	M_ACC	1
	return
	
_PUSHACC5_STOP:
	bc	_STOP
	M_PUSH
	M_ACC	5
	return

_PUSHACC6_EVENT:
;	bc	_EVENT ; removed by bc2asm
	M_PUSH
	M_ACC	6
	return

_PUSHACC7_BREAK:
;	bc	_BREAK  ; removed by bc2asm
	M_PUSH
	M_ACC	7
	return

_BREAK:
_EVENT:
_PUSHATOM:
_ATOM:
_CHECKSIGNALS:
_STOP:
	sleep
	bra	_STOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               BLOCKS              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_PUSHATOM0:
	M_PUSH			; fallthrough
_ATOM0:
	M_ATOM0
	return

_MAKEBLOCK:
	tblrd*+			 ; read size
	rcall	_CHECK_UNFULL_HEAP_GEN
	movf	TABLAT, W
	tblrd*+			 ; read tag
	movff	TABLAT, POSTINC1 ; write tag
	movwf	POSTINC1	 ; write size
	addlw	-0x3
	movwf	TMP_REG_1	 ; TMP_REG_1 <- size - 3
	movf	FSR1L, W	 ; ACCU <- FSR1 ; *FSR1 <- ACCU ; FSR1 += 2
	movff	ACCUL, POSTINC1
	movwf	ACCUL
	movf	FSR1H, W	 ; ok because block address are even
	movff	ACCUH, POSTINC1
	movwf	ACCUH
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
_MAKEBLOCK_LOOP:
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
	decfsz	TMP_REG_1, F
	bra	_MAKEBLOCK_LOOP
	return

_MAKEBLOCK1:
	M_CHECK_UNFULL_HEAP 0x1
	tblrd*+
	movff	TABLAT, POSTINC1
	movlw	0x1
	movwf	POSTINC1
	movf	FSR1L, W
	movff	ACCUL, POSTINC1
	movwf	ACCUL
	movf	FSR1H, W
	movff	ACCUH, POSTINC1
	movwf	ACCUH
	return

_MAKEBLOCK2:
	M_CHECK_UNFULL_HEAP 0x2
	tblrd*+
	movff	TABLAT, POSTINC1
	movlw	0x2
	movwf	POSTINC1
	movf	FSR1L, W
	movff	ACCUL, POSTINC1
	movwf	ACCUL
	movf	FSR1H, W
	movff	ACCUH, POSTINC1
	movwf	ACCUH
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
	return

_MAKEBLOCK3:
	M_CHECK_UNFULL_HEAP 0x3
	tblrd*+
	movff	TABLAT, POSTINC1
	movlw	0x3
	movwf	POSTINC1
	movf	FSR1L, W
	movff	ACCUL, POSTINC1
	movwf	ACCUL
	movf	FSR1H, W
	movff	ACCUH, POSTINC1
	movwf	ACCUH
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
	movff	PREINC2, POSTINC1
	return
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              FLOAT                ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_MAKEFLOATBLOCK:
	tblrd*+				; read size
	rcall	_CHECK_UNFULL_HEAP_GEN
	movlw	DOUBLE_ARRAY_TAG	; write tag
	movwf	POSTINC1
	movff	TABLAT, POSTINC1	; write size
	movff	ACCUL, FSR0L		; FSR0 <- ACCU
	movff	ACCUH, FSR0H
	movff	FSR1L, ACCUL		; ACCU <- @block
	movff	FSR1H, ACCUH
	movff	POSTINC0, POSTINC1	; block[0] <- *FSR0 ; FSR1 ++
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movff	INDF0, POSTINC1
	rrncf	TABLAT, F
	dcfsnz	TABLAT, F		; if size = 1 then return
	return
_MAKEFLOATBLOCK_LOOP:
	movff	PREINC2, FSR0L		; FSR0 <- pop()
	movff	PREINC2, FSR0H
	movff	POSTINC0, POSTINC1 	; *FSR1++ <- *FSR0
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movff	INDF0, POSTINC1
	decfsz	TABLAT, F		; loop (size-1) times
	bra	_MAKEFLOATBLOCK_LOOP
	return

_GETFLOATFIELD:
	M_CHECK_UNFULL_HEAP 0x2
	tblrd*+
	movf	TABLAT, W	; FSR0 <- ACCU + 2 * n
	addwf	ACCUL, W
	movwf	FSR0L
	movlw	0x0
	addwfc	ACCUH, W
	movwf	FSR0H
	movf	TABLAT, W
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	movlw	DOUBLE_TAG	; write tag
	movwf	POSTINC1
	movlw	0x2		; write size
	movwf	POSTINC1
	movff	FSR1L, ACCUL	; ACCU <- @float
	movff	FSR1H, ACCUH
	movff	POSTINC0, POSTINC1 ; copy float
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movff	INDF0, POSTINC1
	return
	
_SETFLOATFIELD:
	movff	POSTINC2, FSR0L		; TMP_REG_4:3:2:1 <- *pop()
	movff	POSTINC2, FSR0H
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	movff	ACCUL, FSR0L		; FSR0 <- ACCU + n
	movff	ACCUH, FSR0H
	tblrd*+				; read n
	movf	TABLAT, W
	addwf	ACCUL, W
	movwf	FSR0L
	movlw	0x0
	addwfc	ACCUH, W
	movwf	FSR0H
	movf	TABLAT, W
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	movff	TMP_REG_1, POSTINC0 	; write the float
	movff	TMP_REG_2, POSTINC0
	movff	TMP_REG_3, POSTINC0
	movff	TMP_REG_4, INDF0
	M_CONST	0			; return ()
	return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;             ARITHMETIC            ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_BOOLNOT:
	decf	ACCUL, W
	iorwf	ACCUH, W
	movlw	0x1
	btfsc	STATUS, Z
	movlw	0x3
	movwf	ACCUL
	clrf	ACCUH
	return

_MULINT:
	bra	_INDIRECTED_MULINT

_MODINT:
	bra	_INDIRECTED_MODINT

_DIVINT:
	bra	_INDIRECTED_DIVINT
	
_NEGINT:
	comf	ACCUL, F
	comf	ACCUH, F
	movlw	0x3
	addwf	ACCUL, F
	btfsc	STATUS, C
	incf	ACCUH, F
	return

_ADDINT:
	decf	PREINC2, W
	addwf	ACCUL, F
	movf	PREINC2, W
	addwfc	ACCUH, F
	return

_SUBINT:
	decf	PREINC2, W
	subwf	ACCUL, F
	movf	PREINC2, W
	subwfb	ACCUH, F
	return

_INDIRECTED_MULINT:
	bcf	STATUS, C
	rrcf	[0x2], F     ; yH = [0x2]
	rrcf	[0x1], F     ; yL = [0x1]
	bcf	STATUS, C
	rrcf	ACCUH, W
	movwf	TMP_REG_2    ; xH = TMP_REG_2
	rrcf	ACCUL, W     ; xL = W
	mulwf	[0x1]	     ; [ACCUH:ACCUL] <- xL * yL
	movff	PRODL, ACCUL
	movff	PRODH, ACCUH
	mulwf	[0x2]	     ; ACCUH <- ACCUH + xL * yH
	movf	PRODL, W
	addwf	ACCUH, F
	movf	TMP_REG_2, W ; ACCUH <- ACCUH + xH * yL
	mulwf	[0x1]
	movf	PRODL, W
	addwf	ACCUH, F
	bsf	STATUS, C    ; [ACCUH:ACCUL] <- ([ACCUH:ACCUL] << 1) | 1
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	addulnk	0x2	     ; pop y

_INDIRECTED_DIVINT:
	rlcf	[0x2], W	; capture sign bit
	rrcf	[0x2], F	; yH = [0x2]
	rrcf	[0x1], F	; yL = [0x1]

	movf	[0x1], W	; y = 0 ?
	iorwf	[0x2], W
	bz	_INDIRECT_RAISE_DIV_BY_0

	rlcf	ACCUH, W	; capture sign bit
	rrcf	ACCUH, W	; xH = TMP_REG_2
	movwf	TMP_REG_2
	rrcf	ACCUL, W	; xL = TMP_REG_1
	movwf	TMP_REG_1
	
	bcf	TMP_REG_5, 0	; TMP_REG_5.0 = result sign

	btfss	TMP_REG_2, 7	; x < 0 ?
	bra	_DIVINT_L0	; no => skip
	comf	TMP_REG_1, F	; yes => negate x
	comf	TMP_REG_2, F
	infsnz	TMP_REG_1, F
	incf	TMP_REG_2, F
	btg	TMP_REG_5, 0	; update result sign
	
_DIVINT_L0:
	btfss	[0x2], 7	; y < 0 ?
	bra	_DIVINT_L1	; no => skip
	comf	[0x1], F	; yes => negate y
	comf	[0x2], F
	infsnz	[0x1], F
	incf	[0x2], F
	btg	TMP_REG_5, 0	; update result sign
	
_DIVINT_L1:
	clrf	ACCUL		; init result to 00...001
	clrf	ACCUH
	bsf	ACCUL, 0

	clrf	TMP_REG_3	; init bit raw to 00...010
	clrf	TMP_REG_4
	bsf	TMP_REG_3, 1
	
_DIVINT_LOOP1:
	bcf	STATUS, C	; y <- y << 1
	rlcf	[0x1], F
	rlcf	[0x2], F

	movf	[0x2], W	; xH - yH
	subwf	TMP_REG_2, W
	bnc	_DIVINT_LOOP2	; if C = 0 (xH < yH) then break
	bnz	_DIVINT_IF_L1	; if Z = 0 (xH > yH) then continue
	movf	[0x1], W	; xL - yL
	subwf	TMP_REG_1, W
	bnc	_DIVINT_LOOP2	; if C = 0 (xL < yL) then break

_DIVINT_IF_L1:
	bcf	STATUS, C	; raw <- raw << 1
	rlcf	TMP_REG_3, F
	rlcf	TMP_REG_4, F

	bra	_DIVINT_LOOP1	; loop

_DIVINT_LOOP2:
	bcf	STATUS, C	; y <- y >> 1
	rrcf	[0x2], F
	rrcf	[0x1], F

	movf	[0x2], W	; xH - yH
	subwf	TMP_REG_2, W
	bnc	_DIVINT_IFN_L2	; if C = 0 (xH < yH) then skip
	bnz	_DIVINT_IF_L2	; if Z = 0 (xH > yH) then continue
	movf	[0x1], W	; xL - yL
	subwf	TMP_REG_1, W
	bnc	_DIVINT_IFN_L2	; if C = 0 (xL < yL) then skip

_DIVINT_IF_L2:
	movf	[0x1], W	; x <- x - y
	subwf	TMP_REG_1, F
	movf	[0x2], W
	subwfb	TMP_REG_2, F
	
	movf	TMP_REG_3, W	; ACCU <- ACCU | raw
	iorwf	ACCUL, F
	movf	TMP_REG_4, W
	iorwf	ACCUH, F
	
_DIVINT_IFN_L2:
	bcf	STATUS, C	; raw <- raw >> 1
	rrcf	TMP_REG_4, F
	rrcf	TMP_REG_3, F

	btfss	TMP_REG_3, 0	; if raw.0 = 0
	bra	_DIVINT_LOOP2	; then loop

	btfss	TMP_REG_5, 0	; negative result ?
	addulnk	0x2		; no => division done
	comf	ACCUL, F	; yes => negate result
	comf	ACCUH, F
	movlw	0x3
	addwf	ACCUL, F
	btfsc	STATUS, C
	incf	ACCUH, F
	addulnk	0x2		; division done
	
_INDIRECT_RAISE_DIV_BY_0:
	goto	caml_raise_division_by_0

_INDIRECTED_MODINT:
	rlcf	[0x2], W	; capture sign bit
	rrcf	[0x2], F	; yH = [0x2]
	rrcf	[0x1], F	; yL = [0x1]

	movf	[0x1], W	; y = 0 ?
	iorwf	[0x2], W
	bz	_INDIRECT_RAISE_DIV_BY_0

	rlcf	ACCUH, W	; capture sign bit
	rrcf	ACCUH, F	; xH = ACCUH
	rrcf	ACCUL, F	; xL = ACCUL
	
	bcf	TMP_REG_5, 0	; TMP_REG_5.0 = result sign

	btfss	ACCUH, 7	; x < 0 ?
	bra	_MODINT_L0	; no => skip
	comf	ACCUL, F	; yes => negate x
	comf	ACCUH, F
	infsnz	ACCUL, F
	incf	ACCUH, F
	bsf	TMP_REG_5, 0	; negative result
	
_MODINT_L0:
	btfss	[0x2], 7	; y < 0 ?
	bra	_MODINT_L1	; no => skip
	comf	[0x1], F	; yes => negate y
	comf	[0x2], F
	infsnz	[0x1], F
	incf	[0x2], F
	
_MODINT_L1:
	clrf	TMP_REG_3	; counter = TMP_REG_3 <- 0

_MODINT_LOOP1:
	bcf	STATUS, C	; y <- y << 1
	rlcf	[0x1], F
	rlcf	[0x2], F

	incf	TMP_REG_3, F	; incr counter
	
	movf	[0x2], W	; xH - yH
	subwf	ACCUH, W
	bnc	_MODINT_LOOP2	; if C = 0 (xH < yH) then break
	bnz	_MODINT_LOOP1	; if Z = 0 (xH > yH) then loop
	movf	[0x1], W	; xL - yL
	subwf	ACCUL, W
	bc	_MODINT_LOOP1	; if C = 1 (xL >= yL) then loop
	
_MODINT_LOOP2:
	bcf	STATUS, C	; y <- y >> 1
	rrcf	[0x2], F
	rrcf	[0x1], F

	movf	[0x2], W	; xH - yH
	subwf	ACCUH, W
	bnc	_MODINT_IFN_L2	; if C = 0 (xH < yH) then skip
	bnz	_MODINT_IF_L2	; if Z = 0 (xH > yH) then do not skip
	movf	[0x1], W	; xL - yL
	subwf	ACCUL, W
	bnc	_MODINT_IFN_L2	; if C = 0 (xL < yL) then skip

_MODINT_IF_L2:
	movf	[0x1], W	; x <- x - y
	subwf	ACCUL, F
	movf	[0x2], W
	subwfb	ACCUH, F
	
_MODINT_IFN_L2:
	decfsz	TMP_REG_3, F	; decr counter
	bra	_MODINT_LOOP2	; if counter <> 0 then loop

	bsf	STATUS, C	; ACCU <- (ACCU << 1) | 1
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	
	btfss	TMP_REG_5, 0	; negative result ?
	addulnk	0x2		; no => modulo done
	comf	ACCUL, F	; yes => negate result
	comf	ACCUH, F
	movlw	0x3
	addwf	ACCUL, F
	btfsc	STATUS, C
	incf	ACCUH, F
	addulnk	0x2		; modulo done


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;           THE BIG LOOP            ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifndef caml_useprim_caml_set_interruption_handler
_THE_BIG_LOOP:
	tblrd*+			; load opcode
	rlcf	TABLAT, W	; shift opcode
	callw			; indirect to "the big switch"
	tblrd*+			; unroll 1
	rlcf	TABLAT, W
	callw
	tblrd*+			; unroll 2
	rlcf	TABLAT, W
	callw
	tblrd*+			; unroll 3
	rlcf	TABLAT, W
	callw
	tblrd*+			; unroll 4
	rlcf	TABLAT, W
	callw
	tblrd*+			; unroll 5
	rlcf	TABLAT, W
	callw
	tblrd*+			; unroll 6
	rlcf	TABLAT, W
	callw
	tblrd*+			; unroll 7
	rlcf	TABLAT, W
	callw
	bra	_THE_BIG_LOOP
#else
_THE_BIG_LOOP:
	tblrd*+			; load opcode
	rlcf	TABLAT, W	; shift opcode
	callw			; indirect to "the big switch"
	btfsc	INT_FLAGS_3, 7	; check interrupt_flag
	bra	_INDIRECTED_CAML_INTERRUPT_HANDLER
	tblrd*+			; unroll 1
	rlcf	TABLAT, W
	callw
	btfsc	INT_FLAGS_3, 7
	bra	_INDIRECTED_CAML_INTERRUPT_HANDLER
	tblrd*+			; unroll 2
	rlcf	TABLAT, W
	callw
	btfsc	INT_FLAGS_3, 7
	bra	_INDIRECTED_CAML_INTERRUPT_HANDLER
	tblrd*+			; unroll 3
	rlcf	TABLAT, W
	callw
	btfss	INT_FLAGS_3, 7
	bra	_THE_BIG_LOOP
_INDIRECTED_CAML_INTERRUPT_HANDLER
	goto	caml_interrupt_handler
#endif
