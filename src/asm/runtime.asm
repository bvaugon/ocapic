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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;             FAILURES              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

M_RAISE_FROM_GLOBAL macro EXN_IND
		M_CHECK_UNFULL_HEAP 0x1
		clrf	POSTINC1	; write tag
		movlw	0x1
		movwf	POSTINC1	; write size
		movff	FSR1L, ACCUL	; ACCU <- exn
		movff	FSR1H, ACCUH
		movlw	(0x2 * EXN_IND + 0x1) ; write EXN_IND
		movwf	POSTINC1
		clrf	POSTINC1
		bra	caml_extern_raise
	endm

M_WRITE_BYTE macro C
		movlw	C
		movwf	POSTINC1
	endm

caml_raise_out_of_memory:
	movlw	low OOM_FIELD0_L
	movwf	ACCUL
	movlw	high OOM_FIELD0_L
	movwf	ACCUH
	goto	caml_extern_raise

caml_raise_division_by_0:
	M_RAISE_FROM_GLOBAL DIVISION_BY_0_IND
	
caml_raise_stack_overflow:
	M_RAISE_FROM_GLOBAL STACK_OVERFLOW_IND

caml_extern_raise:
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
	addfsr	FSR2, 0x1
	movlw	high _THE_BIG_SWITCH 	; restore PCLATH
	movwf	PCLATH
	clrf	STKPTR			; clear return stack
	bra	_THE_BIG_LOOP		; goto THE_BIG_LOOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               HEAP                ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;; check allocation in heap
	;; start gc if necessary
	;; raise exception if memory overflow
	;; give its argument (the size of block) in TABLAT
_CHECK_UNFULL_HEAP_GEN:
	movf	CUR_HEAP_END, W
	cpfslt	FSR1H
	bra	_CHECK_UNFULL_HEAP_GEN_L0
	addlw	-0x1
	cpfseq	FSR1H
	return

	;; if FSR1H = CUR_HEAP_END - 1
	movf	FSR1L, W
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L1
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L2
	return

	;; if FSR1H = CUR_HEAP_END
_CHECK_UNFULL_HEAP_GEN_L0:
	movf	FSR1L, W
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L3
_CHECK_UNFULL_HEAP_GEN_L1:
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L3
_CHECK_UNFULL_HEAP_GEN_L2:
	addlw	0x2
	bc	_CHECK_UNFULL_HEAP_GEN_L3
	return

_CHECK_UNFULL_HEAP_GEN_L3:
	;; execute GC
	rcall	caml_gc_exec

	;; check a new time
	movf	CUR_HEAP_END, W
	cpfslt	FSR1H
	bra	_CHECK_UNFULL_HEAP_GEN_L4
	addlw	-0x1
	cpfseq	FSR1H
	return

	;; if FSR1H = CUR_HEAP_END - 1
	movf	FSR1L, W
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L5
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L6
	return

	;; if FSR1H = CUR_HEAP_END
_CHECK_UNFULL_HEAP_GEN_L4:
	movf	FSR1L, W
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L7
_CHECK_UNFULL_HEAP_GEN_L5:
	addwf	TABLAT, W
	bc	_CHECK_UNFULL_HEAP_GEN_L7
_CHECK_UNFULL_HEAP_GEN_L6:
	addlw	0x2
	btfss	STATUS, C
	return
_CHECK_UNFULL_HEAP_GEN_L7:
	goto	caml_raise_out_of_memory


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;          ALLOC DUMMY              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

caml_alloc_dummy_float:
	movf	ACCUL, W
	andlw	B'11111110'
	bz	caml_alloc_dummy_atom	; size = 0 ?
	movwf	TABLAT
	call	_CHECK_UNFULL_HEAP_GEN
	movlw	DOUBLE_ARRAY_TAG
	movwf	POSTINC1		; write tag
	bra	caml_alloc_dummy_l0
	
caml_alloc_dummy:
	rrcf	ACCUH, W		; STATUS.C ignored
	rrcf	ACCUL, W		; TABLAT <- size
	bz	caml_alloc_dummy_atom	; size = 0 ?
	movwf	TABLAT
	call	_CHECK_UNFULL_HEAP_GEN
	clrf	POSTINC1		; write tag
caml_alloc_dummy_l0:
	movf	TABLAT, W		; write size
	movwf	POSTINC1
	movff	FSR1L, ACCUL
	movff	FSR1H, ACCUH
	movlw	0x1
caml_alloc_dummy_loop:			; fill ()
	movwf	POSTINC1
	clrf	POSTINC1
	decfsz	TABLAT, F
	bra	caml_alloc_dummy_loop
	return
caml_alloc_dummy_atom:
	M_ATOM0
	return
	
caml_update_dummy:		 ; fail if size = 0
	movff	FSR1L, TMP_REG_1 	; save FSR1
	movff	FSR1H, TMP_REG_2
	movff	ACCUL, FSR1L		; FSR1 <- dummy
	movff	ACCUH, FSR1H
	movsf	[0x1], FSR0L		; FSR0 <- newval
	movsf	[0x2], FSR0H
	subfsr	FSR0, 0x2
	subfsr	FSR1, 0x2
	movff	POSTINC0, POSTINC1	; copy tag
	movf	POSTINC0, W		; read size
	bz	caml_update_dummy_end
	addfsr	FSR1, 0x1
caml_update_dummy_loop:
	movff	POSTINC0, POSTINC1 	; loop size times
	movff	POSTINC0, POSTINC1
	decfsz	WREG, F
	bra	caml_update_dummy_loop
caml_update_dummy_end:
	movff	TMP_REG_1, FSR1L 	; restore FSR1
	movff	TMP_REG_2, FSR1H
	return

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;          STOP & COPY              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef CAML_STOP_AND_COPY_GC
caml_gc_exec:
;;; PHASE 1: copy all roots if necessary

	;; exchange heap anchors and ends
	;; FSR1 <- new current heap anchor
	clrf	FSR1L
	movf	OTH_HEAP_END, W
	addlw	(-HEAP_SIZE + 0x1)
	movwf	FSR1H
	movf	OTH_HEAP_END, W
	movff	CUR_HEAP_END, OTH_HEAP_END
	movwf	CUR_HEAP_END

	;; save stack top in GC_TMP_REG_2:1
	;; FSR0 <- stack top
	movf	FSR2L, W
	movwf	GC_TMP_REG_1
	movwf	FSR0L
	movf	FSR2H, W
	movwf	GC_TMP_REG_2
	movwf	FSR0H

	;; check and copy stack elements
	addfsr	FSR0, 0x1
	bra	caml_sacgc_stack_loop_test
caml_sacgc_stack_loop:
	rcall	caml_sacgc_check_and_copy
caml_sacgc_stack_loop_test:
	movlw	(low (STACK_ANCHOR + 0x1))
	cpfseq	FSR0L
	bra	caml_sacgc_stack_loop
	movlw	(high (STACK_ANCHOR + 0x1))
	cpfseq	FSR0H
	bra	caml_sacgc_stack_loop
	
	;; check and copy environment, accu and interrupt_handler
	rcall	caml_sacgc_check_and_copy
	rcall	caml_sacgc_check_and_copy
	rcall	caml_sacgc_check_and_copy

;;; PHASE 2: sweep new heap and upgrade it

	;; FSR0 <- new heap anchor
	clrf	FSR0L
	movf	CUR_HEAP_END, W
	addlw	(-HEAP_SIZE + 0x1)
	movwf	FSR0H

	;; loop on new heap
	bra	caml_sacgc_heap_loop_test
caml_sacgc_heap_loop:
	movlw	NO_SCAN_TAG
	cpfsgt	POSTINC0	; read tag
	bra	caml_sacgc_scan_block
	movf	POSTINC0, W	; skip block ; read size
	addwf	FSR0L, F	; FSR0 += 2 * size
	btfsc	STATUS, C
	incf	FSR0H, F
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	bra	caml_sacgc_heap_loop_test
caml_sacgc_scan_block:
	movff	POSTINC0, GC_TMP_REG_3	; GC_TMP_REG_3 <- size
caml_sacgc_loop_block:
	rcall	caml_sacgc_check_and_copy
	decfsz	GC_TMP_REG_3, F		; loop size times
	bra	caml_sacgc_loop_block
caml_sacgc_heap_loop_test:		; if FSR0 = FSR1 then break else loop
	movf	FSR0L, W
	cpfseq	FSR1L
	bra	caml_sacgc_heap_loop
	movf	FSR0H, W
	cpfseq	FSR1H
	bra	caml_sacgc_heap_loop
	
	;; restore stack top
	movff	GC_TMP_REG_1, FSR2L
	movff	GC_TMP_REG_2, FSR2H
	;; increment counter
	infsnz	GC_COUNTER_L, F
	incf	GC_COUNTER_H, F
	return


caml_sacgc_check_and_copy:
;;; check *FSR0
;;; if it is a pointer in the old heap then
;;;        if it has not been copied then copy;
;;;        update *FSR0;
;;; increment FSR0 by 2
	movf	POSTINC0, W		; W <- (*FSR0++)L
	btfsc	WREG, 0			; isint ?
	bra	caml_sacgc_copy_skip_1 	; yes -> skip
	movwf	FSR2L			; FSR2L <- low pointer
	movf	POSTDEC0, W		; W <- (*FSR0--)H
	movwf	FSR2H			; FSR2H <- high pointer
	subwf	OTH_HEAP_END, W		; in old heap ?
	bnc	caml_sacgc_copy_skip_2 	; no -> skip
	addlw	-HEAP_SIZE
	bc	caml_sacgc_copy_skip_2 	; no -> skip
	subfsr	FSR2, 0x2		; FSR2 -= 2
	movlw	INFIX_TAG		; is infix block ?
	cpfseq	[0x0]
	bra	caml_sacgc_copy_block	; no -> just copy
	movf	[0x1], W		; yes -> shift FSR2
	movwf	GC_TMP_REG_5		; save offset in GC_TMP_REG_5
	subwf	FSR2L, F		; FSR2 -= 2 * offset
	btfss	STATUS, C
	decf	FSR2H, F
	subwf	FSR2L, F
	btfss	STATUS, C
	decf	FSR2H, F
	rcall	caml_sacgc_copy_block	; copy the encapsulating block
	movf	GC_TMP_REG_5, W		; shift result by 2 * offset
	subfsr	FSR0, 0x2
	addwf	POSTINC0, F
	btfsc	STATUS, C
	incf	INDF0, F
	subfsr	FSR0, 0x1
	addwf	POSTINC0, F
	btfsc	STATUS, C
	incf	INDF0, F
	addfsr	FSR0, 0x1
	return
caml_sacgc_copy_block:
	movf	[0x1], W		   ; is already copied ? read block size
	bnz	caml_sacgc_copy_not_copied ; no -> copy
	movf	[0x2], W		   ; yes -> just update *FSR0
	movwf	POSTINC0
	movf	[0x3], W
	movwf	POSTINC0
	return
caml_sacgc_copy_not_copied:		; copy block
	movff	POSTINC2, POSTINC1	; copy tag ; FSR2 ++ ; FSR1 ++
	movf	INDF2, W		; read size
	clrf	POSTINC2		; clear size ; FSR2 ++
	movwf	POSTINC1		; copy size
	movwf	GC_TMP_REG_4		; GC_TMP_REG_4 <- size
	movf	FSR1L, W		; *FSR0 <- FSR1 ; *FSR2 <- FSR1
	movff	INDF2, POSTINC1		; *FSR1 <- *FSR2
	movwf	POSTINC0		; FSR0 += 2 ; FSR1 += 2 ; FSR2 += 2
	movwf	POSTINC2
	movf	FSR1H, W		; ok because address are even
	movff	INDF2, POSTINC1
	movwf	POSTINC0
	movwf	POSTINC2
	decf	GC_TMP_REG_4, F		; GC_TMP_REG_4 --
	bz	caml_sacgc_copy_skip_0	; if size = 1 then skip loop
caml_sacgc_copy_block_loop:
	movff	POSTINC2, POSTINC1
	movff	POSTINC2, POSTINC1
	decfsz	GC_TMP_REG_4, F		; loop size - 1 times
	bra	caml_sacgc_copy_block_loop
	return
caml_sacgc_copy_skip_2:
	movf	POSTINC0, F		; FSR0 ++
caml_sacgc_copy_skip_1:
	movf	POSTINC0, F		; FSR0 ++
caml_sacgc_copy_skip_0:
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;          MARK & COMPACT           ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; |HXXXHXX:..:...HX:.HX:..HXHXHX:.HXX:...HX:..:..        |
;   1   1  11     1 111 11 1 1 1 111  11  1 11    1111
;
; |HXXXHXX:..:...HX:.HX:..HXHXHX:.HXX:...HX:..:..HX      |
;   1   1  11     1 111 11 1 1 1 111  11  1 11    1 1111
;         --       --  --       --   --    --       

#ifdef CAML_MARK_AND_COMPACT_GC
caml_gc_exec:
	;; FSR2
	movff	FSR2L, GC_TMP_REG_3
	movff	FSR2H, GC_TMP_REG_4

        ;; clear the bitmap
        movlw   HEAP2_ANCHOR
        movwf   FSR0H
        clrf    FSR0L
caml_macgc_clear_bitmap_loop:
        clrf    INDF0
        incfsz  FSR0L, F
        bra     caml_macgc_clear_bitmap_loop

        ;; mark 0011 at the end of used heap
        addfsr  FSR1, 0x4
        movff   FSR1L, FSR0L
        movff   FSR1H, FSR0H
        rcall   caml_macgc_set_bit
        addfsr  FSR1, 0x2
        movff   FSR1L, FSR0L
        movff   FSR1H, FSR0H
        rcall   caml_macgc_set_bit
        
        ;; FSR2 <- roots high addr
        addfsr  FSR2, 0x2
        movff   FSR2L, FSR0L
        movff   FSR2H, FSR0H
        rcall   caml_macgc_set_bit
        lfsr    FSR2, INT_FUN_L
        ;; FSR1 <- 0xFFE
        lfsr    FSR1, 0xFFE

;;; MARKING ALGORITHM
caml_macgc_mark_start:
        btfsc   [0x0], 0                   ; is *FSR2 a pointer?
        bra     caml_macgc_mark_next       ; no
        movlw   (HEAP1_END + 0x1)          ; is *FSR2 in heap?
        cpfslt  [0x1]
        bra     caml_macgc_mark_next       ; no
        movlw   HEAP1_ANCHOR
        cpfslt  [0x1]
        bra     caml_macgc_mark_test_mark  ; yes
caml_macgc_mark_next:
        movff   FSR2L, FSR0L               ; FSR0 <- FSR2
        movff   FSR2H, FSR0H
        rcall   caml_macgc_test_bit        ; already marked?
        bnz     caml_macgc_mark_end_block  ; yes
        subfsr  FSR2, 0x2                  ; FSR2 --
        bra     caml_macgc_mark_start
caml_macgc_mark_test_mark:
        movf    [0x0], W                   ; FSR0 <- *FSR2
        movwf   FSR0L
        movf    [0x1], W
        movwf   FSR0H
        subfsr  FSR0, 0x2
        movlw   INFIX_TAG                  ; is infix block?
        cpfseq  INDF0
        bra     caml_macgc_mark_no_infix   ; no
        movf    POSTINC0, F                ; FSR0 ++
	movf	POSTINC0, W                ; W <- offset
        movwf   GC_TMP_REG_1               ; TMP_1 <- offset
	subwf	FSR0L, F                   ; FSR0 -= 2 * offset
	btfss	STATUS, C
	decf	FSR0H, F
	subwf	FSR0L, F
	btfss	STATUS, C
	decf	FSR0H, F
        rcall   caml_macgc_test_and_set_bit ; already marked?
        bnz     caml_macgc_mark_next        ; yes
        movf    [0x0], W
        movwf   FSR0L
        movf    [0x1], W
        movwf   FSR0H
        rcall   caml_macgc_set_bit          ; mark the infix block
        bra     caml_macgc_mark_gen_block
caml_macgc_mark_no_infix:
        movlw   NO_SCAN_TAG                 ; scan block ?
        cpfsgt  POSTINC0
        bra     caml_macgc_mark_scan_block  ; yes
        movf    POSTINC0, F                 ; FSR0 ++
        rcall   caml_macgc_set_bit          ; mark block
        bra     caml_macgc_mark_next
caml_macgc_mark_scan_block:
        movf    POSTINC0, F                 ; FSR0 ++
        rcall   caml_macgc_test_and_set_bit ; already marked?
        bnz     caml_macgc_mark_next        ; yes
        clrf    GC_TMP_REG_1                ; TMP_1 <- 0
caml_macgc_mark_gen_block:
        movff   FSR1L, FSR0L    ; FSR0 <- FSR1
        movff   FSR1H, FSR0H
        movff   FSR2L, FSR1L    ; FSR1 <- FSR2
        movff   FSR2H, FSR1H
        movf    [0x0], W        ; W:GC_TMP_REG_5 <- *FSR2
        movwf   GC_TMP_REG_5
        movf    [0x1], W
        movff   FSR0L, POSTINC2 ; *FSR2 <- FSR0
        movff   FSR0H, INDF2
        movwf   FSR2H           ; FSR2 <- W:GC_TMP_REG_5
        movff   GC_TMP_REG_5, FSR2L
        movf    GC_TMP_REG_1, W
        bz      caml_macgc_mark_gen_block_no_infix
	subwf	FSR2L, F                   ; FSR2 -= 2 * offset
	btfss	STATUS, C
	decf	FSR2H, F
	subwf	FSR2L, F
	btfss	STATUS, C
	decf	FSR2H, F
caml_macgc_mark_gen_block_no_infix:
        movf    POSTDEC2, W     ; W <- size ; FSR2 -= 2
        movf    POSTDEC2, W
        addwf   FSR2L, F        ; FSR2 += 2 * size
        btfsc   STATUS, C
        incf    FSR2H, F
        addwf   FSR2L, F
        btfsc   STATUS, C
        incf    FSR2H, F
        bra     caml_macgc_mark_start
caml_macgc_mark_test_infix:
        subfsr  FSR2, 0x2                 ; is infix block?
        movlw   INFIX_TAG
        cpfseq  [0x0]
        bra     caml_macgc_mark_goback_p2 ; no
        subfsr  FSR2, 0x2                 ; skip infix header
        bsf     FSR1L, 0x0                ; tag FSR1
        bra     caml_macgc_mark_start     ; continue scan
caml_macgc_mark_goback_p2:
        addfsr  FSR2, 0x2       ; restore FSR2
caml_macgc_mark_goback:
        movff   FSR2L, FSR0L    ; FSR0 <- FSR2
        movff   FSR2H, FSR0H
        btfss   FSR1L, 0x0      ; is FSR1 tagged?
        bra     caml_macgc_mark_goback_no_infix ; no
        bcf     FSR1L, 0X0      ; untag FSR1
caml_macgc_mark_goback_infix_loop:
        addfsr  FSR2, 2         ; start at first infix block
        movff   FSR2L, FSR0L    ; FSR0 <- FSR2
        movff   FSR2H, FSR0H
        rcall   caml_macgc_test_bit ; is marked?
        bz      caml_macgc_mark_goback_infix_loop ; no
        movff   FSR2L, FSR0L    ; FSR0 <- FSR2
        movff   FSR2H, FSR0H
        rcall   caml_macgc_clear_bit ; unmark infix block
        movff   FSR2L, FSR0L    ; FSR0 <- FSR2
        movff   FSR2H, FSR0H
caml_macgc_mark_goback_no_infix:
        movff   FSR1L, FSR2L    ; FSR2 <- FSR1
        movff   FSR1H, FSR2H
        movf    [0x0], W        ; FSR1 <- *FSR2
        movwf   FSR1L
        movf    [0x1], W
        movwf   FSR1H
        movf    FSR0L, W        ; *FSR2 <- FSR0
        movwf   [0x0]
        movf    FSR0H, W
        movwf   [0x1]
        bra     caml_macgc_mark_next
caml_macgc_mark_end_block:
        movlw   0xFE
        cpfseq  FSR1L
        bra     caml_macgc_mark_test_infix
        movlw   0x0F
        cpfseq  FSR1H
        bra     caml_macgc_mark_test_infix

;;; COMPUTE OFFSETS
        setf    GC_TMP_REG_1                          ; TMP_1 <- B'11111111'
        lfsr    FSR2, 0xF87                           ; FSR2 <- unimplemented @
        lfsr    FSR1, (HEAP1_ANCHOR * 0x100)          ; FSR1 <- @ of 1st block
        lfsr    FSR0, ((HEAP2_ANCHOR * 0x100) | 0x01) ; FSR0 <- bitmap @1st fld
        movlw   B'00000001'                           ; TMP_5 <- B'00000001'
        movwf   GC_TMP_REG_5
        andwf   INDF0, W                              ; first block alive?
        bz      caml_macgc_offset_enter_dead_section  ; no
caml_macgc_offset_section_loop:
        ;; FSR1 = @ tag of block
        ;; TMP_5|FSR0 = bitmap @ of 1st field
        ;; current [TMP_5|FSR0] bit = 1
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
caml_macgc_offset_section_loop_entry:
        movf    PREINC1, W             ; W <- size
        movff   GC_TMP_REG_1, POSTINC1 ; write TMP_1 in the size field
        addwf   FSR1L, F               ; FSR1 += 2 * size
        btfsc   STATUS, C
        incf    FSR1H, F
        addwf   FSR1L, F
        btfsc   STATUS, C
        incf    FSR1H, F
        addwf   FSR0L, F               ; TMP_5|FSR0 += size
        btfsc   STATUS, C
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; living block?
        andwf   INDF0, W
        bnz     caml_macgc_offset_section_loop ; yes
caml_macgc_offset_enter_dead_section:
        ;; FSR1 = @ of 1st free byte of dead section
        ;; TMP_5|FSR0 = bitmap @ of 2nd free word of dead section
        ;; current [TMP_5|FSR0] bit = 0
        movf    GC_TMP_REG_5, W        ; set 2nd bit of dead section
        iorwf   INDF0, F
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; 3rd bit set?
        andwf   INDF0, W
        bnz     caml_macgc_offset_end  ; yes => end of heap
        movf    GC_TMP_REG_5, W        ; set 3rd bit of dead section
        iorwf   INDF0, F
        swapf   FSR1L, W               ; GC_TMP_REG_1 <- (FSR1 >> 4)
        andlw   B'00001111'
        movwf   GC_TMP_REG_1
        swapf   FSR1H, W
        andlw   B'11110000'
        iorwf   GC_TMP_REG_1, F
        movf    [0x0], W               ; TMP_2 <- low offset
        movwf   GC_TMP_REG_2
        movf    [0x1], W               ; W <- high offset
        movff   FSR1L, FSR2L           ; FSR2 <- FSR1
        movff   FSR1H, FSR2H
        addfsr  FSR1, 0x2              ; FSR1 += 2
        movwf   [0x1]                  ; write offset @ *FSR2
        movf    GC_TMP_REG_2, W
        movwf   [0x0]
        infsnz  [0x0], F               ; (*FSR2) ++
        incf    [0x1], F
caml_macgc_offset_dead_section_loop:
        infsnz  [0x0], F               ; (*FSR2) ++
        incf    [0x1], F
        addfsr  FSR1, 0x2              ; FSR1 += 2
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; start living block?
        andwf   INDF0, W
        bz      caml_macgc_offset_dead_section_loop ; no => continue
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; end of heap?
        andwf   INDF0, W
        bz      caml_macgc_offset_section_loop_entry ; no
caml_macgc_offset_end:
        ;; FSR1 = @ end of 1st byte after the end of heap
        ;; TMP_5|FSR0L = bitmap @ of the 3rd bit after the end of heap
        infsnz  FSR0L, F               ; set the 5th bit
        rlncf   GC_TMP_REG_5, F
        infsnz  FSR0L, F
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W
        iorwf   INDF0, F

;;; SHIFT ADDRESSES
        lfsr    FSR1, (HEAP1_ANCHOR * 0x100)          ; FSR1 <- @ of 1st block
        lfsr    FSR0, ((HEAP2_ANCHOR * 0x100) | 0x02) ; FSR0 <- bitmap @2nd fld
        movlw   B'00000001'                           ; TMP_5 <- B'00000001'
        movwf   GC_TMP_REG_5
        andwf   INDF0, W                              ; first block alive?
        bnz      caml_macgc_shift_dead_section        ; no
caml_macgc_shift_enter_block:
        ;; FSR1 = @ tag of block
        ;; TMP_5|FSR0 = bitmap @ 2nd field
        movlw   NO_SCAN_TAG                 ; scan block ?
        cpfsgt  POSTINC1
        bra     caml_macgc_shift_scan_block ; yes
        movf    POSTINC1, F            ; skip (unsound) size field
caml_macgc_shift_skip_block_loop:
        addfsr  FSR1, 0x2              ; skip field
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; end of block?
        andwf   INDF0, W
        bz      caml_macgc_shift_skip_block_loop ; no
        bra     caml_macgc_shift_block_end
caml_macgc_shift_scan_block:
        ;; FSR1 = @ of size field
        ;; TMP_5|FSR0 = bitmap @ of 2nd field
        movf    POSTINC1, F                ; skip (unsound) size field
caml_macgc_shift_scan_block_loop:
        ;; FSR1 = @ low field
        ;; TMP_5|FSR0 = bitmap @ of next field
        rcall   caml_macgc_shift_address
        movf    POSTINC1, F            ; skip high byte of field
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; end of block?
        andwf   INDF0, W
        bz      caml_macgc_shift_scan_block_loop ; no
caml_macgc_shift_block_end:
        ;; FSR1 = @ tag of next block | @ first dead byte
        ;; TMP_5|FSR0 = bitmap @ of 1st field
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; start of dead section?
        andwf   INDF0, W
        bz      caml_macgc_shift_enter_block ; no
caml_macgc_shift_dead_section:
        ;; FSR1 = @ of 3rd word of dead section
        ;; TMP_5|FSR0 = bitmap @ of 2nd free word
        addfsr  FSR1, 0x2              ; FSR1 += 2
caml_macgc_shift_dead_section_loop:
        ;; FSR1 = @ of 2nd free word of dead section
        ;; TMP_5|FSR0 = bitmap @ of 3rd free word
        addfsr  FSR1, 0x2
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; start living section?
        andwf   INDF0, W
        bz      caml_macgc_shift_dead_section_loop ; no
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; end of heap?
        andwf   INDF0, W
        bz      caml_macgc_shift_enter_block ; no

;;; SHIFT ROOTS
	movff	GC_TMP_REG_3, FSR1L
	movff	GC_TMP_REG_4, FSR1H
caml_macgc_shift_roots_loop:
        movf    POSTINC1, F
        rcall   caml_macgc_shift_address
        movlw   (low (INT_FUN_H))
        cpfseq  FSR1L
        bra     caml_macgc_shift_roots_loop
        movlw   (high (INT_FUN_H))
        cpfseq  FSR1H
        bra     caml_macgc_shift_roots_loop

;;; COMPACT BLOCKS & RESTORE SIZES
        lfsr    FSR2, (HEAP1_ANCHOR * 0x100)          ; FSR2 <- @ of 1st block
        lfsr    FSR1, (HEAP1_ANCHOR * 0x100)          ; FSR1 <- @ of 1st block
        lfsr    FSR0, ((HEAP2_ANCHOR * 0x100) | 0x02) ; FSR0 <- bitmap @2nd fld
        movlw   B'00000001'                           ; TMP_5 <- B'00000001'
        movwf   GC_TMP_REG_5
        andwf   INDF0, W                              ; first block alive?
        bnz     caml_macgc_compact_dead_section       ; no
caml_macgc_compact_section_loop:
        ;; FSR2 = @ tag of from block
        ;; FSR2 = @ tag of to block
        ;; TMP_5|FSR0 = bitmap @ 2nd field
        movff   POSTINC2, POSTINC1     ; copy tag
        clrf    INDF1                  ; to size <- 0
caml_macgc_compact_block_loop:
        incf    INDF1, F               ; to size ++
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; end of block?
        andwf   INDF0, W
        bz      caml_macgc_compact_block_loop ; no
        movf    POSTINC2, F            ; skip (unsound) from size
        movf    POSTINC1, W            ; W <- to size
caml_macgc_compact_copy_field_loop:    ; copy fields
        movff   POSTINC2, POSTINC1
        movff   POSTINC2, POSTINC1
        decfsz  WREG, F
        bra     caml_macgc_compact_copy_field_loop
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; start of dead section?
        andwf   INDF0, W
        bz      caml_macgc_compact_section_loop ; no
caml_macgc_compact_dead_section:
        ;; FSR2 = @ of 1st free byte of dead section
        ;; TMP_5|FSR0 = bitmap @ of 3rd free word
        addfsr  FSR2, 0x2              ; FSR2 += 2
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; start living section?
        andwf   INDF0, W
        bz      caml_macgc_compact_dead_section ; no
        addfsr  FSR2, 0x2              ; FSR2 += 2
        infsnz  FSR0L, F               ; TMP_5|FSR0 ++
        rlncf   GC_TMP_REG_5, F
        movf    GC_TMP_REG_5, W        ; end of heap?
        andwf   INDF0, W
        bz      caml_macgc_compact_section_loop ; no
        
        ;; restore stack top
	movff	GC_TMP_REG_3, FSR2L
	movff	GC_TMP_REG_4, FSR2H
	;; increment counter
	infsnz	GC_COUNTER_L, F
	incf	GC_COUNTER_H, F
	return

;;; Update value pointed by FSR1 (i.e, shift address if needed)
;;; Before call: FSR1 = @ of low byte
;;; After call: FSR1 = @ of high byte
caml_macgc_shift_address:
        btfsc   POSTINC1, 0                ; is int?
        return                             ; yes
        movlw   (HEAP1_END + 0x1)          ; is *FSR1 in heap?
        cpfslt  INDF1
        return                             ; no
        movlw   HEAP1_ANCHOR
        cpfslt  INDF1
        bra     caml_macgc_shift_scan_block_update_pointer ; yes
        return
caml_macgc_shift_scan_block_update_pointer:
        ;; FSR1 = @ of high byte of field
        movff   POSTDEC1, FSR2H        ; FSR2 <- @ of block
        movff   POSTINC1, FSR2L
        subfsr  FSR2, 0x2              ; FSR2 <- @ of tag
        movlw   INFIX_TAG
        cpfseq  POSTINC2, W
        bra     caml_macgc_shift_scan_block_update_pointer_no_infix
        movf    INDF2, W
	subwf	FSR2L, F                   ; FSR2 -= 2 * offset
	btfss	STATUS, C
	decf	FSR2H, F
	subwf	FSR2L, F
	btfss	STATUS, C
	decf	FSR2H, F
caml_macgc_shift_scan_block_update_pointer_no_infix:
        ;; FSR1 = @ of high byte of field
        ;; FSR2 = @ of size
        infsnz  INDF2, W               ; is FSR2 = 0xFF?
        return                         ; yes => nothing to do
        subfsr  FSR2, 0x5
        swapf   FSR2L, W               ; W <- (FSR2 >> 4)
        andlw   B'00001111'
        movwf   GC_TMP_REG_2
        swapf   FSR2H, W
        andlw   B'11110000'
        iorwf   GC_TMP_REG_2, W
        addfsr  FSR2, 0x5
        cpfseq  POSTINC2               ; local?
        bra     caml_macgc_shift_scan_block_update_distant_pointer ; no
caml_macgc_shift_scan_block_update_local_pointer:
        ;; FSR2 = @ of 1st field
        movlw   B'00000001'
        rrcf    FSR2H, F
        rrcf    FSR2L, F
        btfsc   FSR2H, 0
        rlncf   WREG, F
        btfss   FSR2H, 1
        bra     caml_macgc_shift_compute_bit_addr_0
        rlncf   WREG, F
        rlncf   WREG, F
caml_macgc_shift_compute_bit_addr_0:
        btfss   FSR2H, 2
        bra     caml_macgc_shift_compute_bit_addr_1
        rlncf   WREG, F
        rlncf   WREG, F
        rlncf   WREG, F
        rlncf   WREG, F
caml_macgc_shift_compute_bit_addr_1:
        movwf   GC_TMP_REG_2
        movlw   HEAP2_ANCHOR
        movwf   FSR2H
caml_macgc_shift_scan_block_update_local_pointer_loop:
        decf    FSR2L, F                   ; TMP_2|FSR2 --
        infsnz  FSR2L, W
        rrncf   GC_TMP_REG_2, F
        movf    GC_TMP_REG_2, W            ; a first 1?
        andwf   INDF2, W
        bz      caml_macgc_shift_scan_block_update_local_pointer_loop ; no
        decf    FSR2L, F                   ; TMP_2|FSR2 --
        infsnz  FSR2L, W
        rrncf   GC_TMP_REG_2, F
        movf    GC_TMP_REG_2, W            ; a second 1?
        andwf   INDF2, W
        bz      caml_macgc_shift_scan_block_update_local_pointer_loop ; no
        decf    FSR2L, F                   ; TMP_2|FSR2 --
        infsnz  FSR2L, W
        rrncf   GC_TMP_REG_2, F
        movf    GC_TMP_REG_2, W            ; a third 1?
        andwf   INDF2, W
        bz      caml_macgc_shift_scan_block_update_local_pointer_end  ; no
        decf    FSR2L, F                   ; TMP_2|FSR2 --
        infsnz  FSR2L, W
        rrncf   GC_TMP_REG_2, F
caml_macgc_shift_scan_block_update_local_pointer_end:
        clrf    WREG
        bcf     STATUS, C                  ; FSR2 <- bitmap decode FSR2
        rlcf    FSR2L, F
        rlcf    WREG, F
caml_macgc_shift_scan_block_update_local_pointer_end_decode_loop:
        btfsc   GC_TMP_REG_2, 0
        bra     caml_macgc_shift_scan_block_update_local_pointer_end_decode_end
        addlw   0x2
        rrncf   GC_TMP_REG_2, F
        bra     caml_macgc_shift_scan_block_update_local_pointer_end_decode_loop
caml_macgc_shift_scan_block_update_local_pointer_end_decode_end:
        movwf   FSR2H
        movf    POSTDEC1, F     ; *FSR1 -= 2 * offset
        movf    [0x0], W
        subwf   POSTINC1, F
        movf    [0x1], W
        subwfb  POSTDEC1, F
        movf    [0x0], W
        subwf   POSTINC1, F
        movf    [0x1], W
        subwfb  INDF1, F
        return
caml_macgc_shift_scan_block_update_distant_pointer:
        movf    POSTDEC2, F
        swapf   INDF2, W
        movwf   FSR2H
        andlw   B'11111110'
        iorlw   B'00001110'
        movwf   FSR2L
        addfsr  FSR2, 0x6
        bra     caml_macgc_shift_scan_block_update_local_pointer

caml_macgc_set_bit:
;;; set the bitmap bit @FSR0
;;; use W and GC_TMP_REG_5
        movlw   B'00000001'
        rcall   caml_macgc_compute_bit_addr
        movf    GC_TMP_REG_5, W
        iorwf   INDF0, F
        return

caml_macgc_clear_bit:
;;; clear the bitmap bit @FSR0
;;; use W and GC_TMP_REG_5
        movlw   B'11111110'
        rcall   caml_macgc_compute_bit_addr
        movf    GC_TMP_REG_5, W
        andwf   INDF0, F
        return

caml_macgc_test_bit:
;;; test the bitmap bit @FSR0, result in STATUS.Z
;;; use W and GC_TMP_REG_5
        movlw   B'00000001'
        rcall   caml_macgc_compute_bit_addr
        movf    GC_TMP_REG_5, W
        andwf   INDF0, W
        return

caml_macgc_test_and_set_bit:
;;; test the bitmap bit @FSR0, set it if not, result in STATUS.Z
;;; use W and GC_TMP_REG_5
        movlw   B'00000001'
        rcall   caml_macgc_compute_bit_addr
        movf    GC_TMP_REG_5, W ; test bit
        andwf   INDF0, W
        btfss   STATUS, Z
        return
        movf    GC_TMP_REG_5, W ; set bit
        iorwf   INDF0, F
        bsf     STATUS, Z       ; set STATUS.Z
        return
        
caml_macgc_compute_bit_addr:
;;; FSR0 <- bitmap addr of FSR0
;;; rotate W ((FSR0 >> 9) & 0b111) times, result in GC_TMP_REG_5
        rrcf    FSR0H, F
        rrcf    FSR0L, F
        btfsc   FSR0H, 0
        rlncf   WREG, F
        btfss   FSR0H, 1
        bra     caml_macgc_compute_bit_addr_0
        rlncf   WREG, F
        rlncf   WREG, F
caml_macgc_compute_bit_addr_0:
        btfss   FSR0H, 2
        bra     caml_macgc_compute_bit_addr_1
        rlncf   WREG, F
        rlncf   WREG, F
        rlncf   WREG, F
        rlncf   WREG, F
caml_macgc_compute_bit_addr_1:
        movwf   GC_TMP_REG_5
        movlw   HEAP2_ANCHOR
        movwf   FSR0H
        return
#endif
