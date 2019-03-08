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
;;;;;;;;             STRINGS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_blit_string
caml_blit_string:
	;; ACCU = src
	;; [0x2]:[0x1] = srcoff
	;; [0x4]:[0x3] = dst
	;; [0x6]:[0x5] = dstoff
	;; [0x8]:[0x7] = len
	movff	FSR1L, TMP_REG_1 ; save FSR1 in TMP_REG_2:1
	movff	FSR1H, TMP_REG_2
	bcf	STATUS, C	; FSR0 <- src + srcoff
	rrcf	[0x2], F
	rrcf	[0x1], W
	addwf	ACCUL, W
	movwf	FSR0L
	movf	[0x2], W
	addwfc	ACCUH, W
	movwf	FSR0H
	bcf	STATUS, C	; FSR1 <- dst + dstoff
	rrcf	[0x6], F
	rrcf	[0x5], W
	addwf	[0x3], W
	movwf	FSR1L
	movf	[0x6], W
	addwfc	[0x4], W
	movwf	FSR1H
	bcf	STATUS, C	; [0x8]:[0x7] <- len
	rrcf	[0x8], F
	rrcf	[0x7], F
	bz	caml_blit_string_loop_test
caml_blit_string_loop:
	movff	POSTINC0, POSTINC1
	decfsz	[0x7], F
	bra	caml_blit_string_loop
caml_blit_string_loop_test:
	decf	[0x8], F
	bz	caml_blit_string_loop
	movff	TMP_REG_1, FSR1L ; restore FSR1
	movff	TMP_REG_2, FSR1H
	M_CONST	0		; return unit
	return
#endif

#ifdef caml_useprim_caml_ml_string_length
caml_ml_string_length:
	;; ACCU = str
	movff	ACCUL, FSR0L	; FSR0 <- ACCU
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1	; W <- block_size
	movf	POSTINC0, W
	addwf	FSR0L, F	; FSR0 += 2 * block_size - 1 ; W --
	btfsc	STATUS, C
	incf	FSR0H, F
	addlw	-0x1
	addwf	FSR0L, F
	btfsc	STATUS, C
	incf	FSR0H, F
	movwf	ACCUL		; ACCU <- int_val(2 * W)
	clrf	ACCUH
	bcf	STATUS, C
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bsf	STATUS, C
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	btfss	INDF0, 0	; if INDF0.0 = 0 then ACCU ++
	bsf	ACCUL, 1
	return
#endif

#ifdef caml_useprim_caml_fill_string
caml_fill_string:
	;; ACCU = str
	;; [0x2]:[0x1] = start
	;; [0x4]:[0x3] = len
	;; [0x6]:[0x5] = c
	bcf	STATUS, C	; FSR0 <- str + start
	rrcf	[0x2], F
	rrcf	[0x1], W
	addwf	ACCUL, W
	movwf	FSR0L
	movf	[0x2], W
	addwfc	ACCUH, W
	movwf	FSR0H
	rrcf	[0x6], W	; [0x5] <- c
	rrcf	[0x5], F
	bcf	STATUS, C	; [0x4]:[0x3] <- len
	rrcf	[0x4], F
	rrcf	[0x3], F
	bz	caml_fill_string_miloop
caml_fill_string_loop:
	movf	[0x5], W
	movwf	POSTINC0
	decfsz	[0x3], F
	bra	caml_fill_string_loop
caml_fill_string_miloop:
	decf	[0x4], F
	bz	caml_fill_string_loop
	M_CONST	0		; return unit
	return
#endif

#ifdef caml_useprim_caml_create_string
#ifndef caml_useprim_caml_raise_ia_string_create
#define caml_useprim_caml_raise_ia_string_create
#endif
caml_create_string:
	;; ACCU = len
	rrcf	ACCUH, W	; STATUS.C ignored
	movwf	TMP_REG_1
	rrcf	ACCUL, W
	movwf	TABLAT
	movwf	TMP_REG_2
	rrcf	TMP_REG_1, W	; STATUS.C ignored
	andlw	0x3F
	bnz	caml_raise_ia_string_create
	rrcf	TABLAT, F
	incf	TABLAT, F
	bz	caml_raise_ia_string_create
	call	_CHECK_UNFULL_HEAP_GEN
	movlw	STRING_TAG	; write tag
	movwf	POSTINC1
	movf	TABLAT, W	; write size
	movwf	POSTINC1
	movff	FSR1L, ACCUL	; ACCU <- string
	movff	FSR1H, ACCUH
	addlw	-0x1		; FSR1 += 2 * size - 2
	addwf	FSR1L, F
	btfsc	STATUS, C
	incf	FSR1H, F
	addwf	FSR1L, F
	btfsc	STATUS, C
	incf	FSR1H, F
	clrf	POSTINC1       	; write 0
	movlw	0x0		; if odd size then write 0 else write 1
	btfss	TMP_REG_2, 0
	movlw	0x1
	movwf	POSTINC1
	return
#endif

#ifdef caml_useprim_caml_string_get
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds_string
#define caml_useprim_caml_raise_ia_index_out_of_bounds_string
#endif
caml_string_get:
	;; ACCU = str
	;; [0x2]:[0x1] = ind
	movff	ACCUL, FSR0L	; FSR0 <- @size
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	bcf	STATUS, C	; [0x2]:[0x1] <- valint(ind)
	rrcf	[0x2], F
	rrcf	[0x1], F
	bcf	STATUS, C	; check indH
	rrcf	[0x2], W	; C <- LSB(indH)
	bnz	caml_raise_ia_index_out_of_bounds_string
	rrcf	[0x1], W	; W <- ind / 2
	cpfsgt	INDF0		; size > ind / 2 ?
	bra	caml_raise_ia_index_out_of_bounds_string ; no -> error
	addlw	0x1		; W <- ind / 2 + 1
	cpfsgt	POSTINC0	; size = ind / 2 + 1 ?
	bra	caml_string_get_last_char ; yes -> goto last_char
	movf	[0x1], W	; no -> ok ; FSR0 += size
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	clrf	ACCUH		; read char
	bsf	STATUS, C
	rlcf	INDF0, W
	rlcf	ACCUH, F
	movwf	ACCUL
	return
caml_string_get_last_char:
	btfsc	[0x1], 0	; parity of ind ?
	bra	caml_raise_ia_index_out_of_bounds_string ; odd -> error
	movf	[0x1], W	; FSR0 <- @last byte (0 or 1)
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	addfsr	FSR0, 0x1
	movf	POSTDEC0, W	; test last byte
	bnz	caml_raise_ia_index_out_of_bounds_string ; if 1 -> raise
	clrf	ACCUH		; if 0 -> ok, read char
	bsf	STATUS, C
	rlcf	INDF0, W
	rlcf	ACCUH, F
	movwf	ACCUL
	return
#endif

#ifdef caml_useprim_caml_bytes_set
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds_string
#define caml_useprim_caml_raise_ia_index_out_of_bounds_string
#endif
caml_bytes_set:
	;; ACCU = str
	;; [0x2]:[0x1] = ind
	;; [0x4]:[0x3] = c
	movff	ACCUL, FSR0L	; FSR0 <- @size
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	bcf	STATUS, C	; [0x2]:[0x1] <- valint(ind)
	rrcf	[0x2], F
	rrcf	[0x1], F
	bcf	STATUS, C	; check indH
	rrcf	[0x2], W	; C <- LSB(indH)
	bnz	caml_raise_ia_index_out_of_bounds_string
	rrcf	[0x1], W	; W <- ind / 2
	cpfsgt	INDF0		; size > ind / 2 ?
	bra	caml_raise_ia_index_out_of_bounds_string ; no -> error
	addlw	0x1		; W <- ind / 2 + 1
	cpfsgt	POSTINC0	; size = ind / 2 + 1 ?
	bra	caml_bytes_set_last_char ; yes -> goto last_char
	movf	[0x1], W	; no -> ok ; FSR0 += size
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	rrcf	[0x4], W	; W <- c ; STATUS.C ignored
	rrcf	[0x3], W
	movwf	INDF0		; write char
	M_CONST	0		; return ()
	return
caml_bytes_set_last_char:
	btfsc	[0x1], 0	; parity of ind ?
	bra	caml_raise_ia_index_out_of_bounds_string ; odd -> error
	movf	[0x1], W	; FSR0 <- @last byte (0 or 1)
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	addfsr	FSR0, 0x1
	movf	POSTDEC0, W	; test last byte
	bnz	caml_raise_ia_index_out_of_bounds_string ; if 1 -> raise
	rrcf	[0x4], W	; W <- c ; STATUS.C ignored
	rrcf	[0x3], W
	movwf	INDF0		; write char
	M_CONST	0		; return ()
	return
#endif

#ifdef caml_useprim_caml_raise_ia_index_out_of_bounds_string
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds
#define caml_useprim_caml_raise_ia_index_out_of_bounds
#endif
caml_raise_ia_index_out_of_bounds_string:
	movlw	0x1			; erase ACCU and stack[0]
	movwf	ACCUL
	movwf	[0x1]
	clrf	ACCUH
	clrf	[0x2]
	bra	caml_raise_ia_index_out_of_bounds
#endif

#ifdef caml_useprim_caml_raise_ia_string_create
#ifndef caml_useprim_caml_raise_ia
#define caml_useprim_caml_raise_ia
#endif
caml_raise_ia_string_create:
	M_CHECK_UNFULL_HEAP 0xA		; check allocation of string and exn
	M_WRITE_BYTE STRING_TAG		; write string tag
	M_WRITE_BYTE 0x7		; write string size
	movff	FSR1L, TMP_REG_1	; mem string addr
	movff	FSR1H, TMP_REG_2
	M_WRITE_BYTE 'S'		; write string content
	M_WRITE_BYTE 't'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'i'
	M_WRITE_BYTE 'n'
	M_WRITE_BYTE 'g'
	M_WRITE_BYTE '.'
	M_WRITE_BYTE 'c'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 'a'
	M_WRITE_BYTE 't'
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 0x0		; write string \0
	goto	caml_raise_ia
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              ARRAYS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_make_array
caml_make_array:
	;; ACCU = tbl
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	movf	POSTINC0, F	; atom ?
	btfsc	STATUS, Z
	return
	btfsc	INDF0, 0	; is_int(tbl[0]) ?
	return
	movf	POSTINC0, W	; tag(tbl[0]) = DOUBLE_TAG ?
	movff	INDF0, FSR0H
	movwf	FSR0L
	subfsr	FSR0, 0x2
	movf	INDF0, W
	xorlw	DOUBLE_TAG
	btfss	STATUS, Z
	return
	movff	ACCUL, FSR0L	; TABLAT <- size(tbl) * 2
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	movff	INDF0, TABLAT
	bcf	STATUS, C
	rlcf	TABLAT, F	; size(tbl) > 127 ?
	bc	caml_raise_ia_array_make
	call	_CHECK_UNFULL_HEAP_GEN ; check allocation
	movlw	DOUBLE_ARRAY_TAG ; write tag
	movwf	POSTINC1
	movf	TABLAT, W	 ; write size
	movwf	POSTINC1
	rrncf	TABLAT, F	 ; TABLAT <- size / 2
	movff	FSR2L, TMP_REG_1 ; save FSR2
	movff	FSR2H, TMP_REG_2
	movff	ACCUL, FSR0L	 ; FSR0 <- tbl
	movff	ACCUH, FSR0H
caml_make_array_loop:
	movff	POSTINC0, FSR2L	 ; FSR2 <- tbl[i]
	movff	POSTINC0, FSR2H
	movff	POSTINC2, POSTINC1 ; copy float
	movff	POSTINC2, POSTINC1
	movff	POSTINC2, POSTINC1
	movff	POSTINC2, POSTINC1
	decfsz	TABLAT, F
	bra	caml_make_array_loop ; loop
	movff	TMP_REG_1, FSR2L     ; restore FSR2
	movff	TMP_REG_2, FSR2H
	return
#endif

#ifdef caml_useprim_caml_make_vect
#ifndef caml_useprim_caml_raise_ia_array_make
#define caml_useprim_caml_raise_ia_array_make
#endif
caml_make_vect:
	;; ACCU = size
	;; [0x2]:[0x1] = val
	bcf	STATUS, C
	rrcf	ACCUH, W
	bnz	caml_raise_ia_array_make
	rrcf	ACCUL, W
	bz	caml_make_vect_atom
	movwf	TABLAT
	call	_CHECK_UNFULL_HEAP_GEN
	clrf	POSTINC1		; write tag (0)
	movff	TABLAT, POSTINC1	; write size
	movff	FSR1L, ACCUL
	movff	FSR1H, ACCUH
caml_make_vect_loop:
	movf	[0x1], W
	movwf	POSTINC1
	movf	[0x2], W
	movwf	POSTINC1
	decfsz	TABLAT, F
	bra	caml_make_vect_loop
	return
caml_make_vect_atom:
	M_ATOM0
	return
#endif

#ifdef caml_useprim_caml_raise_ia_array_make
#ifndef caml_useprim_caml_raise_ia
#define caml_useprim_caml_raise_ia
#endif
caml_raise_ia_array_make:
	M_CHECK_UNFULL_HEAP 0x9		; check allocation of string and exn
	M_WRITE_BYTE STRING_TAG		; write string tag
	M_WRITE_BYTE 0x6		; write string size
	movff	FSR1L, TMP_REG_1	; mem string addr
	movff	FSR1H, TMP_REG_2
	M_WRITE_BYTE 'A'		; write string content
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'a'
	M_WRITE_BYTE 'y'
	M_WRITE_BYTE '.'
	M_WRITE_BYTE 'm'
	M_WRITE_BYTE 'a'
	M_WRITE_BYTE 'k'
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 0x0		; write string \0\1
	M_WRITE_BYTE 0x1
	goto	caml_raise_ia
#endif

#ifdef caml_useprim_caml_array_unsafe_get
#ifndef caml_useprim_caml_array_unsafe_get_addr
#define caml_useprim_caml_array_unsafe_get_addr
#endif
#ifndef caml_useprim_caml_array_unsafe_get_float
#define caml_useprim_caml_array_unsafe_get_float
#endif
caml_array_unsafe_get:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	movff	ACCUL, FSR0L		; FSR0 <- ACCU - 2
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2
	movlw	DOUBLE_ARRAY_TAG 	; tag = double[] tag ?
	cpfseq	POSTINC0
	bra	caml_array_unsafe_get_addr ; no
	bra	caml_array_unsafe_get_float ; yes
#endif

#ifdef caml_useprim_caml_array_get
#ifndef caml_useprim_caml_array_get_addr
#define caml_useprim_caml_array_get_addr
#endif
#ifndef caml_useprim_caml_array_get_float
#define caml_useprim_caml_array_get_float
#endif
caml_array_get:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	movff	ACCUL, FSR0L		; FSR0 <- ACCU - 2
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2
	movlw	DOUBLE_ARRAY_TAG	; tag = double[] tag ?
	cpfseq	POSTINC0
	bra	caml_array_get_addr 	; no
	bra	caml_array_get_float	; yes
#endif

#ifdef caml_useprim_caml_array_unsafe_set
#ifndef caml_useprim_caml_array_unsafe_set_addr
#define caml_useprim_caml_array_unsafe_set_addr
#endif
#ifndef caml_useprim_caml_array_unsafe_set_float
#define caml_useprim_caml_array_unsafe_set_float
#endif
caml_array_unsafe_set:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	;; [0x4]:[0x3] = val
	movff	ACCUL, FSR0L		; FSR0 <- ACCU - 2
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2
	movlw	DOUBLE_ARRAY_TAG	; tag = double[] tag ?
	cpfseq	POSTINC0
	bra	caml_array_unsafe_set_addr ; no
	bra	caml_array_unsafe_set_float ; yes
#endif

#ifdef caml_useprim_caml_array_set
#ifndef caml_useprim_caml_array_set_addr
#define caml_useprim_caml_array_set_addr
#endif
#ifndef caml_useprim_caml_array_set_float
#define caml_useprim_caml_array_set_float
#endif
caml_array_set:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	;; [0x4]:[0x3] = val
	movff	ACCUL, FSR0L		; FSR0 <- ACCU - 2
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2
	movlw	DOUBLE_ARRAY_TAG	; tag = double[] tag ?
	cpfseq	POSTINC0
	bra	caml_array_set_addr 	; no
	bra	caml_array_set_float	; yes
#endif

#ifdef caml_useprim_caml_array_unsafe_get_addr
caml_array_unsafe_get_addr:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	decf	[0x1], W	; FSR0 <- tbl + ind
	addwf	ACCUL, W
	movwf	FSR0L
	movf	ACCUH, W
	addwfc	[0x2], W
	movwf	FSR0H
	movff	POSTINC0, ACCUL	; ACCU <- *FSR0
	movff	INDF0, ACCUH
	return
#endif

#ifdef caml_useprim_caml_array_unsafe_set_addr
caml_array_unsafe_set_addr:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	;; [0x4]:[0x3] = val
	decf	[0x1], W	; FSR0 <- tbl + ind
	addwf	ACCUL, W
	movwf	FSR0L
	movf	ACCUH, W
	addwfc	[0x2], W
	movwf	FSR0H
	movf	[0x3], W	; *FSR0 <- val
	movwf	POSTINC0
	movf	[0x4], W
	movwf	INDF0
	M_CONST	0		; return ()
	return
#endif

#ifdef caml_useprim_caml_array_unsafe_get_float
caml_array_unsafe_get_float:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	M_CHECK_UNFULL_HEAP 0x2
	decf	[0x1], W	; FSR0 <- ACCU + 2 * ind
	addwf	ACCUL, W
	movwf	FSR0L
	movf	[0x2], W
	addwfc	ACCUH, W
	movwf	FSR0H
	decf	[0x1], W
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
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
#endif

#ifdef caml_useprim_caml_array_unsafe_set_float
caml_array_unsafe_set_float:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	;; [0x4]:[0x3] = val
	movsf	[0x3], FSR0L		; TMP_REG_2:1 <- *val
	movsf	[0x4], FSR0H
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	decf	[0x1], W	; FSR0 <- ACCU + 2 * ind
	addwf	ACCUL, W
	movwf	FSR0L
	movf	[0x2], W
	addwfc	ACCUH, W
	movwf	FSR0H
	decf	[0x1], W
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	movff	TMP_REG_1, POSTINC0 	; write the float
	movff	TMP_REG_2, POSTINC0
	movff	TMP_REG_3, POSTINC0
	movff	TMP_REG_4, INDF0
	M_CONST	0			; return ()
	return
#endif

#ifdef caml_useprim_caml_array_get_addr
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds
#define caml_useprim_caml_raise_ia_index_out_of_bounds
#endif
caml_array_get_addr:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	movff	ACCUL, FSR0L	; FSR0 <- @size
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	bcf	STATUS, C	; check indH
	rrcf	[0x2], W	; C <- LSB(indH)
	bnz	caml_raise_ia_index_out_of_bounds
	rrcf	[0x1], W	; W <- ind
	cpfsgt	POSTINC0	; size > ind ? FSR0++
	bra	caml_raise_ia_index_out_of_bounds ; no -> raise
	decf	[0x1], W	; FSR0 += ind
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	movff	POSTINC0, ACCUL	; ACCU <- *FSR0
	movff	INDF0, ACCUH
	return
#endif

#ifdef caml_useprim_caml_array_get_float
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds
#define caml_useprim_caml_raise_ia_index_out_of_bounds
#endif
caml_array_get_float:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	M_CHECK_UNFULL_HEAP 0x2
	movff	ACCUL, FSR0L	; FSR0 <- ACCU
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	movf	[0x2], W	; C <- LSB(indH)
	bnz	caml_raise_ia_index_out_of_bounds
	decf	[0x1], W	; W <- 2 * ind
	cpfsgt	POSTINC0	; size > ind ? FSR0++
	bra	caml_raise_ia_index_out_of_bounds ; no -> raise
	decf	[0x1], W	; FSR0 += 2 * ind
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	decf	[0x1], W
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
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
#endif

#ifdef caml_useprim_caml_array_set_addr
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds
#define caml_useprim_caml_raise_ia_index_out_of_bounds
#endif
caml_array_set_addr:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	;; [0x4]:[0x3] = val
	movff	ACCUL, FSR0L	; FSR0 <- @size
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	bcf	STATUS, C	; check indH
	rrcf	[0x2], W	; C <- LSB(indH)
	bnz	caml_raise_ia_index_out_of_bounds
	rrcf	[0x1], W	; W <- ind
	cpfsgt	POSTINC0	; size > ind ? FSR0++
	bra	caml_raise_ia_index_out_of_bounds ; no -> raise
	decf	[0x1], W	; FSR0 += ind
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	movf	[0x3], W	; *FSR0 <- val
	movwf	POSTINC0
	movf	[0x4], W
	movwf	INDF0
	M_CONST	0		; return ()
	return
#endif

#ifdef caml_useprim_caml_array_set_float
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds
#define caml_useprim_caml_raise_ia_index_out_of_bounds
#endif
#endif

#ifdef caml_useprim_caml_raise_ia_index_out_of_bounds
#ifndef caml_useprim_caml_raise_ia
#define caml_useprim_caml_raise_ia
#endif
caml_raise_ia_index_out_of_bounds:
	M_CHECK_UNFULL_HEAP 0xD		; check allocation of string and exn
	M_WRITE_BYTE STRING_TAG		; write string tag
	M_WRITE_BYTE 0xA		; write string size
	movff	FSR1L, TMP_REG_1	; mem string addr
	movff	FSR1H, TMP_REG_2
	M_WRITE_BYTE 'i'		; write string content
	M_WRITE_BYTE 'n'
	M_WRITE_BYTE 'd'
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 'x'
	M_WRITE_BYTE ' '
	M_WRITE_BYTE 'o'
	M_WRITE_BYTE 'u'
	M_WRITE_BYTE 't'
	M_WRITE_BYTE ' '
	M_WRITE_BYTE 'o'
	M_WRITE_BYTE 'f'
	M_WRITE_BYTE ' '
	M_WRITE_BYTE 'b'
	M_WRITE_BYTE 'o'
	M_WRITE_BYTE 'u'
	M_WRITE_BYTE 'n'
	M_WRITE_BYTE 'd'
	M_WRITE_BYTE 's'
	M_WRITE_BYTE 0x0		; write string \0
	goto	caml_raise_ia
#endif

#ifdef caml_useprim_caml_array_set_float
#ifndef caml_useprim_caml_raise_ia_index_out_of_bounds
#define caml_useprim_caml_raise_ia_index_out_of_bounds
#endif
caml_array_set_float:
	;; ACCU = tbl
	;; [0x2]:[0x1] = ind
	;; [0x4]:[0x3] = val
	movsf	[0x3], FSR0L		; TMP_REG_2:1 <- *val
	movsf	[0x4], FSR0H
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	movff	ACCUL, FSR0L		; FSR0 <- ACCU
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	movf	[0x2], W	; C <- LSB(indH)
	bnz	caml_raise_ia_index_out_of_bounds
	decf	[0x1], W	; W <- 2 * ind
	cpfsgt	POSTINC0	; size > ind ? FSR0++
	bra	caml_raise_ia_index_out_of_bounds ; no -> raise
	decf	[0x1], W	; FSR0 += 2 * ind
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	decf	[0x1], W
	addwf	FSR0L, F
	movf	[0x2], W
	addwfc	FSR0H, F
	movff	TMP_REG_1, POSTINC0 	; write the float
	movff	TMP_REG_2, POSTINC0
	movff	TMP_REG_3, POSTINC0
	movff	TMP_REG_4, INDF0
	M_CONST	0			; return ()
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              HASH                 ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_hash_univ_param
caml_hash_univ_param:
	;; ACCU = count
	;; [0x2]:[0x1] = limit
	;; [0x4]:[0x3] = obj
	movff	ACCUL, TMP_REG_7 ; TMP_REG_8:7 <- count * 2 + 1
	movff	ACCUH, TMP_REG_8
	bcf	STATUS, C	 ; TMP_REG_A:9 <- limit + 1
	rrcf	[0x2], W
	movwf	TMP_REG_A
	rrcf	[0x1], W
	movwf	TMP_REG_9
	infsnz	TMP_REG_9, F
	incf	TMP_REG_A, F
	movsf	[0x4], FSR0H	 ; FSR0 <- obj
	movsf	[0x3], FSR0L
	movff	FSR2H, TMP_REG_6 ; TMP_REG_6:5 <- FSR2
	movff	FSR2L, TMP_REG_5
	clrf	ACCUH		 ; ACCU <- 0
	clrf	ACCUL
caml_hash_univ_param_beg:
	btfss	FSR0L, 0	 ; isint(ACCU) ?
	bra	caml_hash_univ_param_block ; no -> goto block
	movf	FSR0L, W	 ; yes -> hash(int)
	rcall	caml_hash_univ_param_routine
	movf	FSR0H, W
	rcall	caml_hash_univ_param_routine
caml_hash_univ_param_back:
	movf	FSR2L, W	 ; check stack top
	cpfseq	TMP_REG_5
	bra	caml_hash_univ_param_pop
	movf	FSR2H, W
	cpfslt	TMP_REG_6
	bra	caml_hash_univ_param_return
caml_hash_univ_param_pop:	 ; go backward
	movff	PREINC2, TMP_REG_B
	movff	PREINC2, FSR0L
	movff	PREINC2, FSR0H
	bra	caml_hash_univ_param_loop
caml_hash_univ_param_block:
	subfsr	FSR0, 0x2	 ; FSR0 <- @tag
	movf	POSTINC0, W 	 ; W <- tag ; FSR0 ++
	xorlw	STRING_TAG	 ; string ?
	bz	caml_hash_univ_param_data
	xorlw	(DOUBLE_TAG ^ STRING_TAG) 	  ; float ?
	bz	caml_hash_univ_param_data
	xorlw	(DOUBLE_ARRAY_TAG ^ DOUBLE_TAG)   ; float[] ?
	bz	caml_hash_univ_param_data
	xorlw	(ABSTRACT_TAG ^ DOUBLE_ARRAY_TAG) ; abstract ?
	bz	caml_hash_univ_param_back
	xorlw	(INFIX_TAG ^ ABSTRACT_TAG) 	  ; infix ?
	bz	caml_hash_univ_param_infix
	xorlw	(FORWARD_TAG ^ INFIX_TAG) 	  ; forward ?
	bz	caml_hash_univ_param_forward
	xorlw	(OBJECT_TAG ^ FORWARD_TAG) 	  ; object ?
	bz	caml_hash_univ_param_object
	xorlw	(CUSTOM_TAG ^ OBJECT_TAG) 	  ; custom ?
	bz	caml_hash_univ_param_custom
caml_hash_univ_param_beg_loop:
	rcall	caml_hash_univ_param_routine 	; hash(tag)
	movff	POSTINC0, TMP_REG_B		; TMP_REG_B <- size
caml_hash_univ_param_loop:
	decf	TMP_REG_9, F			; check limit
	bnz	caml_hash_univ_param_loop_continue
	decf	TMP_REG_A, F
	bz	caml_hash_univ_param_popstop
caml_hash_univ_param_loop_continue:
	movf	TMP_REG_B, W			; last field ?
	bz	caml_hash_univ_param_back	; yes -> go back
	movf	POSTINC0, W			; isint(field) ?
	btfss	WREG, 0
	bra	caml_hash_univ_param_loop_block ; no -> goto loop_block
	rcall	caml_hash_univ_param_routine	; yes -> hash(int)
	movf	POSTINC0, W
	rcall	caml_hash_univ_param_routine
	decf	TMP_REG_B, F			; size --;
	bra	caml_hash_univ_param_loop	; loop
caml_hash_univ_param_loop_block:
	movwf	TMP_REG_C			; TMP_REG_C <- fieldL
	dcfsnz	TMP_REG_B, F			; size --; if size = 0
	bra	caml_hash_univ_param_loop_block_last ; then goto loop_block_last
	subfsr	FSR2, 3				; push3
	movlw	(STACK_END - 0x1)		; check STACK OVERFLOW
	cpfsgt	FSR2H
	goto	caml_raise_stack_overflow
	movf	TMP_REG_B, W			; push (size, FSR0)
	movwf	[0x1]
	movf	FSR0L, W
	movwf	[0x2]
	movf	FSR0H, W
	movwf	[0x3]
caml_hash_univ_param_loop_block_last:
	movf	INDF0, W			; forward
	movwf	FSR0H
	movff	TMP_REG_C, FSR0L
	bra	caml_hash_univ_param_beg
caml_hash_univ_param_data:			; hash all data block
	incf	POSTINC0, W			; TMP_REG_C <- size + 1
	movwf	TMP_REG_C
	rcall	caml_hash_univ_param_routine 	; hash(size)
caml_hash_univ_param_data_loop:
	dcfsnz	TMP_REG_C, F			; if TMP_REG_C -- = 0
	bra	caml_hash_univ_param_back	; then go back
	movf	POSTINC0, W			; hash fieldL
	rcall	caml_hash_univ_param_routine
	movf	POSTINC0, W			; hash fieldH
	rcall	caml_hash_univ_param_routine
	bra	caml_hash_univ_param_data_loop 	; loop
caml_hash_univ_param_infix:
	movf	INDF0, W	; FSR0 -= 2 * (*FSR0)
	subwf	FSR0L, F
	btfss	STATUS, C
	decf	FSR0H, F
	subwf	FSR0L, F
	btfss	STATUS, C
	decf	FSR0H, F
	bra	caml_hash_univ_param_beg_loop
caml_hash_univ_param_forward:
	addfsr	FSR0, 1		    ; FSR0++
	movff	POSTINC0, TMP_REG_C ; FSR0 <- *FSR0
	movf	INDF0, W
	movwf	FSR0H
	movff	TMP_REG_C, FSR0L
	bra	caml_hash_univ_param_beg ; hash forwarded
caml_hash_univ_param_object:
	addfsr	FSR0, 3		; FSR0 += 3
	movf	POSTINC0, W	; hash(object_id)
	rcall	caml_hash_univ_param_routine
	movf	INDF0, W
	rcall	caml_hash_univ_param_routine
	bra	caml_hash_univ_param_back ; go back
caml_hash_univ_param_custom:
	movff	ACCUL, TMP_REG_D ; save counter
	movff	ACCUH, TMP_REG_E
	addfsr	FSR0, 0x1	 ; FSR0 <- @
	movff	FSR0L, ACCUL	 ; ACCU <- @
	movff	FSR0H, ACCUH
	movlw	0x2		 ; PRODL <- hash custom index (2)
	movwf	PRODL
	movf	POSTINC0, W	 ; call custom indirection table
	movff	INDF0, PCLATH
	callw
	movf	ACCUL, W	 ; restore ACCU and hash result
	movff	TMP_REG_D, ACCUL
	movwf	TMP_REG_D
	movf	ACCUH, W
	movff	TMP_REG_E, ACCUH
	rcall 	caml_hash_univ_param_routine
	movf	TMP_REG_D, W
	rcall	caml_hash_univ_param_routine
	bra	caml_hash_univ_param_back ; go back
caml_hash_univ_param_popstop:
	pop
caml_hash_univ_param_stop:
	movff	TMP_REG_5, FSR2L
	movff	TMP_REG_6, FSR2H
caml_hash_univ_param_return:
	bcf	ACCUH, 7
	bsf	ACCUL, 0
	return
caml_hash_univ_param_routine:
	decf	TMP_REG_7, F
	bnz	caml_hash_univ_param_routine_continue
	decf	TMP_REG_8, F
	bz	caml_hash_univ_param_popstop
caml_hash_univ_param_routine_continue:
	addwf	ACCUL, F
	btfsc	STATUS, C
	incf	ACCUH, F
	movlw	0xDF
	mulwf	ACCUL
	movff	PRODL, ACCUL
	movf	PRODH, W
	addwf	ACCUH, F
	return
#endif

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               OBJ                 ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_obj_dup
caml_obj_dup:
	;; ACCU = obj
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1
	movf	POSTDEC0, W
	bz	caml_obj_dup_end
	movwf	TABLAT
	call	_CHECK_UNFULL_HEAP_GEN
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2
	movff	POSTINC0, POSTINC1 ; copy tag
	movff	POSTINC0, POSTINC1 ; copy size
	movff	FSR1L, ACCUL
	movff	FSR1H, ACCUH
caml_obj_dup_loop:
	movff	POSTINC0, POSTINC1 ; copy 1 element
	movff	POSTINC0, POSTINC1
	decfsz	TABLAT, F	   ; loop size times
	bra	caml_obj_dup_loop
caml_obj_dup_end:
	return
#endif

#ifdef caml_useprim_caml_obj_block
caml_obj_block:
	;; ACCU = tag
	;; [0x2]:[0x1] = size
	rrcf	[0x2], W	; check allocation
	rrcf	[0x1], W
	bz	caml_obj_block_atom
	movwf	TABLAT
	call	_CHECK_UNFULL_HEAP_GEN
	rrcf	ACCUH, W	; write tag
	rrcf	ACCUL, W
	movwf	POSTINC1
	rrcf	[0x2], W	; write size
	rrcf	[0x1], W
	movwf	POSTINC1
	movff	FSR1L, ACCUL
	movff	FSR1H, ACCUH
caml_obj_block_loop:		; fill with unit
	clrf	INDF1
	bsf	POSTINC1, 0
	clrf	POSTINC1
	decfsz	WREG, F		; loop size times
	bra	caml_obj_block_loop
	return
caml_obj_block_atom:
	M_ATOM0
	return
#endif

#ifdef caml_useprim_caml_obj_set_tag
caml_obj_set_tag:
	;; ACCU = obj
	;; [0x2]:[0x1] = tag
	movff	ACCUL, FSR0L	; FSR0 <- obj
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2	; FSR0 <- @old_tag
	rrcf	[0x2], W	; W <- new_tag
	rrcf	[0x1], W
	movwf	INDF0		; write new_tag
	return
#endif

#ifdef caml_useprim_caml_obj_tag
caml_obj_tag:
	btfsc	ACCUL, 0		; is int ?
	bra	caml_obj_tag_int	; yes -> return Int
	movf	ACCUH, W		; in heap ?
	subwf	CUR_HEAP_END, W
	bnc	caml_obj_tag_out_of_memory ; no -> return Out_of_heap
	addlw	-HEAP_SIZE
	bc	caml_obj_tag_out_of_memory ; no -> return Out_of_heap
	movff	ACCUL, FSR0L		; FSR0 <- obj
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x2		; FSR0 <- @tag
	clrf	ACCUH			; ACCU <- tag
	bsf	STATUS, C
	rlcf	INDF0, W
	rlcf	ACCUH, F
	movwf	ACCUL
	return
caml_obj_tag_int:
	movlw	low ((OBJ_TAG_INT * 0x2) + 0x1)
	movwf	ACCUL
	movlw	high ((OBJ_TAG_INT * 0x2) + 0x1)
	movwf	ACCUH
	return
caml_obj_tag_out_of_memory:
	movlw	low ((OBJ_TAG_OUT_OF_MEMORY * 0x2) + 0x1)
	movwf	ACCUL
	movlw	high ((OBJ_TAG_OUT_OF_MEMORY * 0x2) + 0x1)
	movwf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_obj_is_block
caml_obj_is_block:
	movlw	0x1
	btfss	ACCUL, 0
	movlw	0x3
	movwf	ACCUL
	clrf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_obj_truncate
#ifndef caml_useprim_caml_raise_ia_obj_truncate
#define caml_useprim_caml_raise_ia_obj_truncate
#endif
caml_obj_truncate:
	;; ACCU = obj
	;; [0x2]:[0x1] = size
	movff	ACCUL, FSR0L	; FSR0 <- obj
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1	; FSR0 <- @old_size
	bcf	STATUS, C	; W <- new_size
	rrcf	[0x2], W
	bnz	caml_raise_ia_obj_truncate ; if new_size > 255 -> raise
	rrcf	[0x1], W
	bz	caml_raise_ia_obj_truncate ; if new_size = 0 -> raise
	subwf	INDF0, W	; W <- old_size - new_size
	bnc	caml_raise_ia_obj_truncate ; if new_s > old_s -> raise
	subwf	INDF0, F	; write (old_size - (old_size - new_size))
	return
#endif

#ifdef caml_useprim_caml_raise_ia_obj_truncate
#ifndef caml_useprim_caml_raise_ia
#define caml_useprim_caml_raise_ia
#endif
caml_raise_ia_obj_truncate:
	M_CHECK_UNFULL_HEAP 0xA		; check allocation of string and exn
	M_WRITE_BYTE STRING_TAG		; write string tag
	M_WRITE_BYTE 0x7		; write string size
	movff	FSR1L, TMP_REG_1	; mem string addr
	movff	FSR1H, TMP_REG_2
	M_WRITE_BYTE 'O'		; write string content
	M_WRITE_BYTE 'b'
	M_WRITE_BYTE 'j'
	M_WRITE_BYTE '.'
	M_WRITE_BYTE 't'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'u'
	M_WRITE_BYTE 'n'
	M_WRITE_BYTE 'c'
	M_WRITE_BYTE 'a'
	M_WRITE_BYTE 't'
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 0x0		; write string \0\1
	M_WRITE_BYTE 0x1
	goto	caml_raise_ia
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;                GC                 ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_heap_size
caml_heap_size:
	movlw	low (((HEAP_SIZE * 0x100) * 0x2) + 0x1)
	movwf	ACCUL
	movlw	high (((HEAP_SIZE * 0x100) * 0x2) + 0x1)
	movwf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_stack_size
caml_stack_size:
	movlw	low (((STACK_ANCHOR - 0xEFF) + (STACK_SIZE * 0x100)) + 0x1)
	movwf	ACCUL
	movlw	high (((STACK_ANCHOR - 0xEFF) + (STACK_SIZE * 0x100)) + 0x1)
	movwf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_heap_occupation
caml_heap_occupation:
	movf	CUR_HEAP_END, W
	addlw	(-HEAP_SIZE + 0x1)
	subwf	FSR1H, W
	movwf	ACCUH
	bsf	STATUS, C
	rlcf	FSR1L, W
	movwf	ACCUL
	rlcf	ACCUH, F
	return
#endif

#ifdef caml_useprim_caml_stack_occupation
caml_stack_occupation:
	movf	FSR2L, W
	sublw	low (STACK_ANCHOR + 0x1)
	movwf	ACCUL
	movlw	high (STACK_ANCHOR + 0x1)
	subfwb	FSR2H, W
	movwf	ACCUH
	bsf	ACCUL, 0
	return
#endif

#ifdef caml_useprim_caml_running_number
caml_running_number:
	bsf	STATUS, C
	rlcf	GC_COUNTER_L, W
	movwf	ACCUL
	rlcf	GC_COUNTER_H, W
	movwf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_gc_run
caml_gc_run:
	goto	caml_gc_exec
#endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;                OO                 ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_set_oo_id
caml_set_oo_id:
        movff   ACCUL, FSR0L
        movff   ACCUH, FSR0H
        addfsr  FSR0, 0x2
        movff   OO_ID_COUNTER_L, POSTINC0
        movff   OO_ID_COUNTER_H, INDF0
        movlw   0x2
        addwf   OO_ID_COUNTER_L, F
        movlw   0x0
        addwfc  OO_ID_COUNTER_H, F
#endif

#ifdef caml_useprim_caml_fresh_oo_id
caml_fresh_oo_id:
        movff   OO_ID_COUNTER_L, ACCUL
        movff   OO_ID_COUNTER_H, ACCUH
        movlw   0x2
        addwf   OO_ID_COUNTER_L, F
        movlw   0x0
        addwfc  OO_ID_COUNTER_H, F
#endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              RANDOM               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_random_bits
#ifndef caml_useprim_caml_random_round
#define caml_useprim_caml_random_round
#endif
caml_random_bits:
	rcall	caml_random_round
	movff	RAND_CUR_L, ACCUL
	movff	RAND_CUR_H, ACCUH
	bsf	ACCUL, 0
	bcf	ACCUH, 7
	return
#endif

#ifdef caml_useprim_caml_random_bool
#ifndef caml_useprim_caml_random_round
#define caml_useprim_caml_random_round
#endif
caml_random_bool:
	rcall	caml_random_round
	clrf	ACCUH
	movf	RAND_CUR_L, W
	andlw	0x03
	iorlw	0x01
	movwf	ACCUL
	return
#endif

#ifdef caml_useprim_caml_random_round
caml_random_round:
	movlw	0x4
	movwf	TMP_REG_1
caml_random_round_loop:
	movff	RAND_CUR_L, FSR0L
	movf	RAND_CUR_H, W
	iorlw	0xF0
	incfsz	WREG, W
	decf	WREG, W
	movwf	FSR0H
	movf	POSTINC0, W
	xorlw	0xAA
	xorwf	RAND_CUR_L, F
	movf	POSTINC0, W
	xorlw	0xAA
	xorwf	RAND_CUR_H, F
	swapf	RAND_CUR_L, F
	swapf	RAND_CUR_H, F
	rlcf	RAND_CUR_L, W
	rlcf	RAND_CUR_H, F
	rlcf	RAND_CUR_L, F
	movlw	.223
	addwf	RAND_CUR_L, F
	addwf	RAND_CUR_H, F
	decfsz	TMP_REG_1, F
	bra	caml_random_round_loop
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;            SFR ACCESS             ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_pic_write_reg
caml_pic_write_reg:
	bsf	STATUS, C	; FSR0 <- 0x0F70 | (ACCUL >> 1)
	rrcf	ACCUL, W
	movwf	FSR0L
	setf	FSR0H
	rrcf	[0x2], W	; STATUS.C ignored
	rrcf	[0x1], W	; INDF0 <- stack[0] >> 1
	movwf	INDF0
	M_CONST	0
	return
#endif

#ifdef caml_useprim_caml_pic_read_reg
caml_pic_read_reg:
	bsf	STATUS, C	; FSR0 <- 0x0F70 | (ACCUL >> 1)
	rrcf	ACCUL, W
	movwf	FSR0L
	setf	FSR0H
	rlcf	INDF0, W	; ACCU <- (INDF0 << 1) | 1
	movwf	ACCUL
	rlcf	ACCUH, F
	return
#endif

#ifdef caml_useprim_caml_pic_set_bit
caml_pic_set_bit:
	bsf	STATUS, C	; FSR0 <- 0xF70 | (ACCUL >> 1)
	rrcf	ACCUL, W
	movwf	FSR0L
	setf	FSR0H
	movf	INDF0, W	; INDF0 <- INDF0 | ACCUH
	iorwf	ACCUH, W
	movwf	INDF0
	M_CONST	0
	return
#endif

#ifdef caml_useprim_caml_pic_clear_bit
caml_pic_clear_bit:
	bsf	STATUS, C	; FSR0 <- 0xF70 | (ACCUL >> 1)
	rrcf	ACCUL, W
	movwf	FSR0L
	setf	FSR0H
	comf	ACCUH, F	; INDF0 <- INDF0 & ~ACCUH
	movf	INDF0, W
	andwf	ACCUH, W
	movwf	INDF0
	M_CONST	0
	return
#endif

#ifdef caml_useprim_caml_pic_test_bit
caml_pic_test_bit:
	bsf	STATUS, C	; FSR0 <- 0xF70 | (ACCUL >> 1)
	rrcf	ACCUL, W
	movwf	FSR0L
	setf	FSR0H
	movf	ACCUH, W	; INDF0 & ACCUH
	andwf	INDF0, W
	movlw	0x1		; ACCU <- if Z then 1 else 3
	btfss	STATUS, Z
	movlw	0x3
	movwf	ACCUL
	clrf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_pic_tris_of_port
caml_pic_tris_of_port:
	movlw	((0x2 * LATA) & 0xFF)
	cpfslt	ACCUL
	addlw	(0x2 * (PORTA - LATA))
	addlw	((0x2 * (TRISA - PORTA) - (0x2 * LATA)) & 0xFF)
	addwf	ACCUL, F
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;             FORMATS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_int_of_string
#ifndef caml_useprim_caml_raise_ios_failure
#define caml_useprim_caml_raise_ios_failure
#endif
caml_int_of_string:
	movff	ACCUL, FSR0L	; FSR0 <- ACCU
	movff	ACCUH, FSR0H
	clrf	ACCUL		; ACCU <- 0
	clrf	ACCUH
	movf	POSTINC0, W
	bz	caml_raise_ios_failure
	sublw	'-'		; W <- '-' - str[0]
	bcf	TMP_REG_5, 0	; TMP_REG_5.0 = 0  <=>  res >= 0
	bnz	caml_int_of_string_sign_tested
	bsf	TMP_REG_5, 0
	movf	POSTINC0, W
caml_int_of_string_sign_tested:
	sublw	('-' - '0')	; W <- str[0] - '0'
	bnz	caml_int_of_string_dec
	movf	POSTINC0, W	; W <- str[1]
	bz	caml_int_of_string_return_zero
	sublw	'x'		; W <- 'x' - str[1]
	bz	caml_int_of_string_hex
	sublw	('x' - 'X')	; W <- str[1] - 'X'
	bz	caml_int_of_string_hex
	sublw	('b' - 'X')	; W <- 'b' - str[1]
	bz	caml_int_of_string_bin
	sublw	('b' - 'B')	; W <- str[1] - 'B'
	bz	caml_int_of_string_bin
	sublw	('o' - 'B')	; W <- 'o' - str[1]
	bz	caml_int_of_string_oct
	sublw	('o' - 'O')	; W <- str[1] - 'O'
	bz	caml_int_of_string_oct
	addlw	('O' - '0')	; W <- str[1] - '0'
caml_int_of_string_dec:
	sublw	('9' - '0')	; W <- ('9' - '0') - (c - '0')
	bnc	caml_raise_ios_failure
	sublw	('9' - '0')	; W <- ('9' - '0') - ('9' - c)
	bnc	caml_raise_ios_failure
	movwf	TMP_REG_1
	movf	ACCUL, W
	mullw	.10
	movf	TMP_REG_1, W
	addwf	PRODL, W
	movwf	ACCUL
	movf	PRODH, W
	addwfc	ACCUH, F
	bn	caml_raise_ios_failure
	movf	POSTINC0, W	; W <- str[i++]
	bz	caml_int_of_string_return
	addlw	(-'0')		; W <- c - '0'
	bra	caml_int_of_string_dec
caml_int_of_string_oct:
	movf	POSTINC0, W	; W <- str[i++]
	bz	caml_raise_ios_failure
caml_int_of_string_oct_loop:
	addlw	(-'0')		; W <- c - '0'
	swapf	WREG, F
	rlncf	WREG, F
	rlcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	rlcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	rlcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	andlw	B'11111000'	; check range
	bnz	caml_raise_ios_failure
	movf	POSTINC0, W	; W <- str[i++]
	bnz	caml_int_of_string_oct_loop
	bra	caml_int_of_string_return
caml_int_of_string_bin:
	movf	POSTINC0, W	; W <- str[i++]
	bz	caml_raise_ios_failure
caml_int_of_string_bin_loop:
	addlw	(-'0')		; W <- c - '0'
	rrcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	andlw	B'01111111'	; check range
	bnz	caml_raise_ios_failure
	movf	POSTINC0, W	; W <- str[i++]
	bnz	caml_int_of_string_bin_loop
	bra	caml_int_of_string_return
caml_int_of_string_hex:
	movf	POSTINC0, W	; W <- str[i++]
	bz	caml_raise_ios_failure
caml_int_of_string_hex_loop:
	addlw	(-'0')		; W <- c - '0'
	bnc	caml_raise_ios_failure
	addlw	('0' - '9' - 1)	; W <- (c - '0') + ('0' - '9' - 1)
	bnc	caml_int_of_string_hex_09
	addlw	('9' - 'A' + 1)	; W <- (c - '9' - 1) + ('9' - 'A' + 1)
	bnc	caml_raise_ios_failure
	addlw	('A' - 'F' - 1)	; W <- (c - 'A') + ('A' - 'F' - 1)
	bnc	caml_int_of_string_hex_AF
	addlw	('F' - 'a' + 1)	; W <- (c - 'F' - 1) + ('F' - 'a' + 1)
	bnc	caml_raise_ios_failure
	addlw	('a' - 'f' - 1)	; W <- (c - 'a') + ('a' - 'f' - 1)
	bc	caml_raise_ios_failure
caml_int_of_string_hex_af:
	addlw	('f' - 'F' + 'A' - 'a')
caml_int_of_string_hex_AF:
	addlw	('F' - '9' + '0' - 'A' + .10)
caml_int_of_string_hex_09:
	addlw	('9' - '0' + 1)
caml_int_of_string_hex_shift:
	swapf	WREG, F
	rlcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	rlcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	rlcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	rlcf	WREG, F
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	bn	caml_raise_ios_failure
	movf	POSTINC0, W
	bnz	caml_int_of_string_hex_loop
caml_int_of_string_return:
	bsf	STATUS, C
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	btfss	TMP_REG_5, 0	; negate ?
	return			; no -> return
	comf	ACCUL, F	; yes -> ACCU <- -ACCU
	comf	ACCUH, F
	infsnz	ACCUL, F
	incf	ACCUH, F
	return
caml_int_of_string_return_zero:
	bsf	ACCUL, 0
	return
#endif

#ifdef caml_useprim_caml_raise_ios_failure
caml_raise_ios_failure:
	movlw	0x1			; erase ACCU
	movwf	ACCUL
	clrf	ACCUH
	M_CHECK_UNFULL_HEAP 0xA		; check allocation of string and exn
	M_WRITE_BYTE STRING_TAG		; write string tag
	M_WRITE_BYTE 0x7		; write string size
	movff	FSR1L, TMP_REG_1	; mem string addr
	movff	FSR1H, TMP_REG_2
	M_WRITE_BYTE 'i'		; write string content
	M_WRITE_BYTE 'n'
	M_WRITE_BYTE 't'
	M_WRITE_BYTE '_'
	M_WRITE_BYTE 'o'
	M_WRITE_BYTE 'f'
	M_WRITE_BYTE '_'
	M_WRITE_BYTE 's'
	M_WRITE_BYTE 't'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'i'
	M_WRITE_BYTE 'n'
	M_WRITE_BYTE 'g'
	M_WRITE_BYTE 0x0		; write string \0
	M_WRITE_BYTE 0x0		; write exn tag
	M_WRITE_BYTE 0x2		; write exn size
	movff	FSR1L, ACCUL		; ACCU <- exn
	movff	FSR1H, ACCUH
	movlw	(FAILURE_IND * 0x2 + 0x1) ; write FAILURE_IND
	movwf	POSTINC1
	clrf	POSTINC1
	movff	TMP_REG_1, POSTINC1 	; write string addr
	movff	TMP_REG_2, POSTINC1
	goto	caml_extern_raise
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               INT32               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

M_CREATE_INT32 macro
#ifndef	caml_useprim_caml_int32_custom
#define	caml_useprim_caml_int32_custom
#endif
		M_CHECK_UNFULL_HEAP 0x3
		setf	POSTINC1 		; write tag
		movlw	0x3
		movwf	POSTINC1 		; write size
		movff	FSR1L, ACCUL
		movff	FSR1H, ACCUH		; ACCU <- @
		movlw	low caml_int32_custom
		movwf	POSTINC1 		; write custom @L
		movlw	high caml_int32_custom
		movwf	POSTINC1 		; write custom @H
	endm

M_PRIM_INT32_1ARG_INIT macro
#ifndef caml_useprim_caml_int32_1arg
#define caml_useprim_caml_int32_1arg
#endif
		call	caml_int32_1arg_init
	endm

M_PRIM_INT32_1ARG_END macro
		return
	endm

M_PRIM_INT32_2ARG_INIT macro
#ifndef caml_useprim_caml_int32_2arg	
#define caml_useprim_caml_int32_2arg
#endif
		call	caml_int32_2arg_init
	endm

M_PRIM_INT32_2ARG_END macro
		movff	TMP_REG_1, FSR2L
		movff	TMP_REG_2, FSR2H
		return
	endm
	
#ifdef caml_useprim_caml_int32_neg
caml_int32_neg:
	M_PRIM_INT32_1ARG_INIT
	comf	POSTINC0, W	; copy and incr lowest byte
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 2
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 3
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	INDF0, W	; 4
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	M_PRIM_INT32_1ARG_END
#endif

#ifdef caml_useprim_caml_int32_add
caml_int32_add:
	M_PRIM_INT32_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	addwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 4
	addwfc	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT32_2ARG_END
#endif

#ifdef caml_useprim_caml_int32_sub
caml_int32_sub:
	M_PRIM_INT32_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	subwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 4
	subwfb	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT32_2ARG_END
#endif

#ifdef caml_useprim_caml_int32_mul
caml_int32_mul:
	M_PRIM_INT32_2ARG_INIT
	movf	INDF0, W	 ; r0 += (x0*y0)L ; r1 += (x0*y0)H
	mulwf	[0x0]		 ; FSR1 ++
	movff	PRODL, POSTINC1
	movff	PRODH, INDF1
	mulwf	[0x1]		 ; r1 += (x0*y1)L ; r2 += (x0*y1)H
	movff	PRODH, TMP_REG_3
	clrf	TMP_REG_4
	movf	PRODL, W
	addwf	INDF1, F
	movlw	0x00
	addwfc	TMP_REG_3, F
	addwfc	TMP_REG_4, F
	movf	INDF0, W	 ; r2 += (x0*y2)L ; r3 += (x0*y2)H
	mulwf	[0x2]
	movf	PRODL, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	movf	PRODH, W
	addwf	TMP_REG_4, F
	movf	POSTINC0, W	 ; r3 += (x0*y3)L
	mulwf	[0x3]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movf	INDF0, W	 ; r1 += (x1*y0)L ; r2 += (x1*y0)H
	mulwf	[0x0]		 ; FSR1 ++
	movf	PRODL, W
	addwf	POSTINC1, F
	movlw	0x00
	addwfc	TMP_REG_3, F
	addwfc	TMP_REG_4, F
	movf	PRODH, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	movf	INDF0, W	 ; r2 += (x1*y1)L ; r3 += (x1*y1)H
	mulwf	[0x1]
	movf	PRODL, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	movf	PRODH, W
	addwf	TMP_REG_4, F
	movf	POSTINC0, W	 ; r3 += (x1*y2)L
	mulwf	[0x2]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movf	INDF0, W	 ; r2 += (x2*y0)L ; r3 += (x2*y0)H
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	movf	PRODH, W
	addwf	TMP_REG_4, F
	movf	POSTINC0, W	 ; r3 += (x2*y1)L
	mulwf	[0x1]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movf	INDF0, W	 ; r3 += (x3*y0)L
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movff	TMP_REG_3, POSTINC1 ; flush r2 ; FSR1 ++
	movff	TMP_REG_4, POSTINC1 ; flush r3 ; FSR1 ++
	M_PRIM_INT32_2ARG_END
#endif

#ifdef caml_useprim_caml_int32_div
caml_int32_div:
	movsf	[0x1], FSR0L
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_5 ; y = TMP_REG_8:7:6:5
	movff	POSTINC0, TMP_REG_6
	movff	POSTINC0, TMP_REG_7
	movf	INDF0, W
	movwf	TMP_REG_8
	iorwf	TMP_REG_7, W	    ; y = 0 ?
	iorwf	TMP_REG_6, W
	iorwf	TMP_REG_5, W
	btfsc	STATUS, Z
	goto	caml_raise_division_by_0
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_1 ; x = TMP_REG_4:3:2:1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	M_CREATE_INT32		    ; ACCU <- new int32
	bcf	TMP_REG_9, 0	    ; TMP_REG_9.0 = result sign
	btfss	TMP_REG_4, 7	    ; x < 0 ?
	bra	caml_int32_div_l0   ; no => skip
	btg	TMP_REG_9, 0	    ; update result sign
	comf	TMP_REG_1, F	    ; negate x
	comf	TMP_REG_2, F
	comf	TMP_REG_3, F
	comf	TMP_REG_4, F
	incf	TMP_REG_1, F
	bnc	caml_int32_div_l0
	incf	TMP_REG_2, F
	bnc	caml_int32_div_l0
	incf	TMP_REG_3, F
	bnc	caml_int32_div_l0
	incf	TMP_REG_4, F
caml_int32_div_l0:
	btfss	TMP_REG_8, 7	; y < 0 ?
	bra	caml_int32_div_l1 ; no => skip
	btg	TMP_REG_9, 0	; update result sign
	comf	TMP_REG_5, F	; negate y
	comf	TMP_REG_6, F
	comf	TMP_REG_7, F
	comf	TMP_REG_8, F
	incf	TMP_REG_5, F
	bnc	caml_int32_div_l1
	incf	TMP_REG_6, F
	bnc	caml_int32_div_l1
	incf	TMP_REG_7, F
	bnc	caml_int32_div_l1
	incf	TMP_REG_8, F
caml_int32_div_l1:
	clrf	TMP_REG_A	; init result to 0
	clrf	TMP_REG_B
	clrf	TMP_REG_C
	clrf	TMP_REG_D
	clrf	GC_TMP_REG_1	; init bit raw to 00...01
	clrf	GC_TMP_REG_2
	clrf	GC_TMP_REG_3
	clrf	GC_TMP_REG_4
	bsf	GC_TMP_REG_1, 0
caml_int32_div_loop1:
	bcf	STATUS, C	; y <- y << 1
	rlcf	TMP_REG_5, F
	rlcf	TMP_REG_6, F
	rlcf	TMP_REG_7, F
	rlcf	TMP_REG_8, F
	movf	TMP_REG_8, W	; x - y
	subwf	TMP_REG_4, W
	bnc	caml_int32_div_loop2 ; if x < y then break
	bnz	caml_int32_div_if_l1 ; if x > y then continue
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int32_div_loop2 ; if x < y then break
	bnz	caml_int32_div_if_l1 ; if w > y then continue
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int32_div_loop2 ; if x < y then break
	bnz	caml_int32_div_if_l1 ; if w > y then continue
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bnc	caml_int32_div_loop2 ; if x < y then break
caml_int32_div_if_l1:
	bcf	STATUS, C	; raw <- raw << 1
	rlcf	GC_TMP_REG_1, F
	rlcf	GC_TMP_REG_2, F
	rlcf	GC_TMP_REG_3, F
	rlcf	GC_TMP_REG_4, F
	bra	caml_int32_div_loop1 ; loop
caml_int32_div_loop2:
	bcf	STATUS, C	; y <- y >> 1
	rrcf	TMP_REG_8, F
	rrcf	TMP_REG_7, F
	rrcf	TMP_REG_6, F
	rrcf	TMP_REG_5, F
	movf	TMP_REG_8, W	; x - y
	subwf	TMP_REG_4, W
	bnc	caml_int32_div_ifn_l2 ; if x < y then skip
	bnz	caml_int32_div_if_l2  ; if x > y then continue
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int32_div_ifn_l2 ; if x < y then skip
	bnz	caml_int32_div_if_l2  ; if x > y then continue
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int32_div_ifn_l2 ; if x < y then skip
	bnz	caml_int32_div_if_l2  ; if x > y then continue
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bnc	caml_int32_div_ifn_l2 ; if x < y then skip
caml_int32_div_if_l2:
	movf	TMP_REG_5, W	; x <- x - y
	subwf	TMP_REG_1, F
	movf	TMP_REG_6, W
	subwfb	TMP_REG_2, F
	movf	TMP_REG_7, W
	subwfb	TMP_REG_3, F
	movf	TMP_REG_8, W
	subwfb	TMP_REG_4, F
	movf	GC_TMP_REG_1, W ; res <- res | raw
	iorwf	TMP_REG_A, F
	movf	GC_TMP_REG_2, W
	iorwf	TMP_REG_B, F
	movf	GC_TMP_REG_3, W
	iorwf	TMP_REG_C, F
	movf	GC_TMP_REG_4, W
	iorwf	TMP_REG_D, F
caml_int32_div_ifn_l2:
	bcf	STATUS, C	; raw <- raw >> 1
	rrcf	GC_TMP_REG_4, F
	rrcf	GC_TMP_REG_3, F
	rrcf	GC_TMP_REG_2, F
	rrcf	GC_TMP_REG_1, F
	btfss	STATUS, C		; if CARRY = 0
	bra	caml_int32_div_loop2	; then loop
	btfss	TMP_REG_9, 0		; negative result ?
	bra	caml_int32_div_end	; no => division done
	comf	TMP_REG_A, F	   	; yes => negate result
	comf	TMP_REG_B, F
	comf	TMP_REG_C, F
	comf	TMP_REG_D, F
	incf	TMP_REG_A, F
	bnc	caml_int32_div_end
	incf	TMP_REG_B, F
	bnc	caml_int32_div_end
	incf	TMP_REG_C, F
	bnc	caml_int32_div_end
	incf	TMP_REG_D, F
caml_int32_div_end:
	movff	TMP_REG_A, POSTINC1 ; flush result
	movff	TMP_REG_B, POSTINC1
	movff	TMP_REG_C, POSTINC1
	movff	TMP_REG_D, POSTINC1
	return
#endif

#ifdef caml_useprim_caml_int32_mod
caml_int32_mod:
	movsf	[0x1], FSR0L
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_5 ; y = TMP_REG_8:7:6:5
	movff	POSTINC0, TMP_REG_6
	movff	POSTINC0, TMP_REG_7
	movf	INDF0, W
	movwf	TMP_REG_8
	iorwf	TMP_REG_7, W	    ; y = 0 ?
	iorwf	TMP_REG_6, W
	iorwf	TMP_REG_5, W
	btfsc	STATUS, Z
	goto	caml_raise_division_by_0
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_1 ; x = TMP_REG_4:3:2:1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	M_CREATE_INT32		    ; ACCU <- new int32
	bcf	TMP_REG_9, 0	    ; TMP_REG_9.0 = result sign
	btfss	TMP_REG_4, 7	    ; x < 0 ?
	bra	caml_int32_mod_l0   ; no => skip
	btg	TMP_REG_9, 0	    ; update result sign
	comf	TMP_REG_1, F	    ; negate x
	comf	TMP_REG_2, F
	comf	TMP_REG_3, F
	comf	TMP_REG_4, F
	incf	TMP_REG_1, F
	bnc	caml_int32_mod_l0
	incf	TMP_REG_2, F
	bnc	caml_int32_mod_l0
	incf	TMP_REG_3, F
	bnc	caml_int32_mod_l0
	incf	TMP_REG_4, F
caml_int32_mod_l0:
	btfss	TMP_REG_8, 7	; y < 0 ?
	bra	caml_int32_mod_l1 ; no => skip
	btg	TMP_REG_9, 0	; update result sign
	comf	TMP_REG_5, F	; negate y
	comf	TMP_REG_6, F
	comf	TMP_REG_7, F
	comf	TMP_REG_8, F
	incf	TMP_REG_5, F
	bnc	caml_int32_mod_l1
	incf	TMP_REG_6, F
	bnc	caml_int32_mod_l1
	incf	TMP_REG_7, F
	bnc	caml_int32_mod_l1
	incf	TMP_REG_8, F
caml_int32_mod_l1:
	clrf	TMP_REG_E	; counter = TMP_REG_E <- 0
caml_int32_mod_loop1:
	bcf	STATUS, C	; y <- y << 1
	rlcf	TMP_REG_5, F
	rlcf	TMP_REG_6, F
	rlcf	TMP_REG_7, F
	rlcf	TMP_REG_8, F
	incf	TMP_REG_E, F	; incr counter
	movf	TMP_REG_8, W	; x - y
	subwf	TMP_REG_4, W
	bnc	caml_int32_mod_loop2 ; if x < y then break
	bnz	caml_int32_mod_loop1 ; if x > y then loop
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int32_mod_loop2 ; if x < y then break
	bnz	caml_int32_mod_loop1 ; if x > y then loop
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int32_mod_loop2 ; if x < y then break
	bnz	caml_int32_mod_loop1 ; if x > y then loop
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bc	caml_int32_mod_loop1 ; if x >= y then loop
caml_int32_mod_loop2:
	bcf	STATUS, C	; y <- y >> 1
	rrcf	TMP_REG_8, F
	rrcf	TMP_REG_7, F
	rrcf	TMP_REG_6, F
	rrcf	TMP_REG_5, F
	movf	TMP_REG_8, W	; x - y
	subwf	TMP_REG_4, W
	bnc	caml_int32_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int32_mod_if_l2  ; if x > y then do not skip
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int32_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int32_mod_if_l2  ; if x > y then do not skip
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int32_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int32_mod_if_l2  ; if x > y then do not skip
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bnc	caml_int32_mod_ifn_l2 ; if x < y then skip
caml_int32_mod_if_l2:
	movf	TMP_REG_5, W	; x <- x - y
	subwf	TMP_REG_1, F
	movf	TMP_REG_6, W
	subwfb	TMP_REG_2, F
	movf	TMP_REG_7, W
	subwfb	TMP_REG_3, F
	movf	TMP_REG_8, W
	subwfb	TMP_REG_4, F
caml_int32_mod_ifn_l2:
	decfsz	TMP_REG_E, F	; decr counter
	bra	caml_int32_mod_loop2 ; if counter <> 0 then loop
	btfss	TMP_REG_9, 0	; negative result ?
	bra	caml_int32_mod_end ; no => modulo done
	comf	TMP_REG_1, F	   ; yes => negate result
	comf	TMP_REG_2, F
	comf	TMP_REG_3, F
	comf	TMP_REG_4, F
	incf	TMP_REG_1, F
	bnc	caml_int32_mod_end
	incf	TMP_REG_2, F
	bnc	caml_int32_mod_end
	incf	TMP_REG_3, F
	bnc	caml_int32_mod_end
	incf	TMP_REG_4, F
caml_int32_mod_end:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	return
#endif

#ifdef caml_useprim_caml_int32_and
caml_int32_and:
	M_PRIM_INT32_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 4
	andwf	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT32_2ARG_END
#endif

#ifdef caml_useprim_caml_int32_or
caml_int32_or:
	M_PRIM_INT32_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 4
	iorwf	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT32_2ARG_END
#endif

#ifdef caml_useprim_caml_int32_xor
caml_int32_xor:
	M_PRIM_INT32_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 4
	xorwf	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT32_2ARG_END
#endif

#ifdef caml_useprim_caml_int32_shift_left
caml_int32_shift_left:
	M_PRIM_INT32_1ARG_INIT
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	caml_int32_shift_left_end_loop
caml_int32_shift_left_loop:
	bcf	STATUS, C
	rlcf	TMP_REG_1, F
	rlcf	TMP_REG_2, F
	rlcf	TMP_REG_3, F
	rlcf	TMP_REG_4, F
	decfsz	[0x1], F
	bra	caml_int32_shift_left_loop
caml_int32_shift_left_end_loop:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	M_PRIM_INT32_1ARG_END
#endif

#ifdef caml_useprim_caml_int32_shift_right
caml_int32_shift_right:
	M_PRIM_INT32_1ARG_INIT
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	caml_int32_shift_right_end_loop
caml_int32_shift_right_loop:
	rlcf	TMP_REG_4, W	; STATUS.C ignored
	rrcf	TMP_REG_4, F
	rrcf	TMP_REG_3, F
	rrcf	TMP_REG_2, F
	rrcf	TMP_REG_1, F
	decfsz	[0x1], F
	bra	caml_int32_shift_right_loop
caml_int32_shift_right_end_loop:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	M_PRIM_INT32_1ARG_END
#endif

#ifdef caml_useprim_caml_int32_shift_right_unsigned
caml_int32_shift_right_unsigned:
	M_PRIM_INT32_1ARG_INIT
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	caml_int32_shift_right_unsigned_end_loop
caml_int32_shift_right_unsigned_loop:
	bcf	STATUS, C
	rrcf	TMP_REG_4, F
	rrcf	TMP_REG_3, F
	rrcf	TMP_REG_2, F
	rrcf	TMP_REG_1, F
	decfsz	[0x1], F
	bra	caml_int32_shift_right_unsigned_loop
caml_int32_shift_right_unsigned_end_loop:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	M_PRIM_INT32_1ARG_END
#endif

#ifdef caml_useprim_caml_int32_of_int
caml_int32_of_int:
	movff	ACCUL, TMP_REG_1 ; TMP_REG_1:2 <- ACCU
	movff	ACCUH, TMP_REG_2
	M_CREATE_INT32
	rrcf	TMP_REG_2, W	 ; STATUS.C ignored
	rrcf	TMP_REG_1, W
	movwf	POSTINC1
	rlcf	TMP_REG_2, W
	bc	caml_int32_of_int_neg
	rrcf	TMP_REG_2, W
	movwf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	return
caml_int32_of_int_neg:
	rrcf	TMP_REG_2, W
	movwf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	return
#endif

#ifdef caml_useprim_caml_int32_1arg
caml_int32_1arg_init:
	movff	ACCUL, TMP_REG_1
	movff	ACCUH, TMP_REG_2
	M_CREATE_INT32		 ; ACCU <- alloc(int32)
	movff	TMP_REG_1, FSR0L ; FSR0 <- old ACCU
	movff	TMP_REG_2, FSR0H
	addfsr	FSR0, 0x2
	return
#endif

#ifdef caml_useprim_caml_int32_2arg
caml_int32_2arg_init:
	movff	ACCUL, TMP_REG_1
	movff	ACCUH, TMP_REG_2
	M_CREATE_INT32		 ; ACCU <- alloc(int32)
	movff	TMP_REG_1, FSR0L ; FSR0 <- old ACCU
	movff	TMP_REG_2, FSR0H
	addfsr	FSR0, 0x2
	movff	FSR2L, TMP_REG_1 ; save FSR2
	movff	FSR2H, TMP_REG_2
	movf	[0x1], W	 ; FSR2 <- arg2
	movsf	[0x2], FSR2H
	movwf	FSR2L
	addfsr	FSR2, 0x2
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               INT64               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

M_CREATE_INT64 macro
#ifndef	caml_useprim_caml_int64_custom
#define	caml_useprim_caml_int64_custom
#endif
		M_CHECK_UNFULL_HEAP 0x5
		setf	POSTINC1 		; write tag
		movlw	0x5
		movwf	POSTINC1 		; write size
		movff	FSR1L, ACCUL
		movff	FSR1H, ACCUH		; ACCU <- @
		movlw	low caml_int64_custom
		movwf	POSTINC1 		; write custom @L
		movlw	high caml_int64_custom
		movwf	POSTINC1 		; write custom @H
	endm

M_PRIM_INT64_1ARG_INIT macro
#ifndef caml_useprim_caml_int64_1arg
#define caml_useprim_caml_int64_1arg
#endif
		call	caml_int64_1arg_init
	endm

M_PRIM_INT64_1ARG_END macro
		return
	endm

M_PRIM_INT64_2ARG_INIT macro
#ifndef caml_useprim_caml_int64_2arg
#define caml_useprim_caml_int64_2arg
#endif
		call	caml_int64_2arg_init
	endm

M_PRIM_INT64_2ARG_END macro
		movff	TMP_REG_1, FSR2L
		movff	TMP_REG_2, FSR2H
		return
	endm
	
#ifdef caml_useprim_caml_int64_neg
caml_int64_neg:
	M_PRIM_INT64_1ARG_INIT
	comf	POSTINC0, W	; 1
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 2
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 3
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 4
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 5
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 6
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	POSTINC0, W	; 7
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	comf	INDF0, W	; 8
	btfsc	STATUS, C
	incf	WREG, F
	movwf	POSTINC1
	M_PRIM_INT64_1ARG_END
#endif

#ifdef caml_useprim_caml_int64_add
caml_int64_add:
	M_PRIM_INT64_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	addwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 4
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 5
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 6
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 7
	addwfc	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 8
	addwfc	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT64_2ARG_END
#endif

#ifdef caml_useprim_caml_int64_sub
caml_int64_sub:
	M_PRIM_INT64_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	subwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 4
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 5
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 6
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 7
	subwfb	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 8
	subwfb	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT64_2ARG_END
#endif

#ifdef caml_useprim_caml_int64_mul
caml_int64_mul:
	M_PRIM_INT64_2ARG_INIT
	movf	INDF0, W	 ; r0 += (x0*y0)L ; r1 += (x0*y0)H
	mulwf	[0x0]		 ; FSR1 ++
	movff	PRODL, POSTINC1
	movff	PRODH, INDF1
	mulwf	[0x1]		 ; r1 += (x0*y1)L ; r2 += (x0*y1)H
	movff	PRODH, TMP_REG_3
	clrf	TMP_REG_4
	clrf	TMP_REG_5
	clrf	TMP_REG_6
	clrf	TMP_REG_7
	clrf	TMP_REG_8
	movf	PRODL, W
	addwf	INDF1, F
	movlw	0x00
	addwfc	TMP_REG_3, F
	addwfc	TMP_REG_4, F
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r2 += (x0*y2)L ; r3 += (x0*y2)H
	mulwf	[0x2]
	movf	PRODL, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_4, F
	movlw	0x00
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r3 += (x0*y3)L ; r4 += (x0*y3)H
	mulwf	[0x3]
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movlw	0x00
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r4 += (x0*y4)L ; r5 += (x0*y4)H
	mulwf	[0x4]
	movf	PRODL, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r5 += (x0*y5)L ; r6 += (x0*y5)H
	mulwf	[0x5]
	movf	PRODL, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r6 += (x0*y6)L ; r7 += (x0*y6)H
	mulwf	[0x6]
	movf	PRODL, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_8, F
	movf	POSTINC0, W	 ; r7 += (x0*y7)L
	mulwf	[0x7]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movf	INDF0, W	 ; r1 += (x1*y0)L ; r2 += (x1*y0)H
	mulwf	[0x0]
	movf	PRODL, W
	addwf	POSTINC1, F
	movlw	0x00
	addwfc	TMP_REG_3, F
	addwfc	TMP_REG_4, F
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r2 += (x1*y1)L ; r3 += (x1*y1)H
	mulwf	[0x1]
	movf	PRODL, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_4, F
	movlw	0x00
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r3 += (x1*y2)L ; r4 += (x1*y2)H
	mulwf	[0x2]
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movlw	0x00
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r4 += (x1*y3)L ; r5 += (x1*y3)H
	mulwf	[0x3]
	movf	PRODL, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r5 += (x1*y4)L ; r6 += (x1*y4)H
	mulwf	[0x4]
	movf	PRODL, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r6 += (x1*y5)L ; r7 += (x1*y5)H
	mulwf	[0x5]
	movf	PRODL, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_8, F
	movf	POSTINC0, W	 ; r7 += (x1*y6)L
	mulwf	[0x6]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movf	INDF0, W	 ; r2 += (x2*y0)L ; r3 += (x2*y0)H
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_3, F
	movlw	0x00
	addwfc	TMP_REG_4, F
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_4, F
	movlw	0x00
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r3 += (x2*y1)L ; r4 += (x2*y1)H
	mulwf	[0x1]
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movlw	0x00
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r4 += (x2*y2)L ; r5 += (x2*y2)H
	mulwf	[0x2]
	movf	PRODL, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r5 += (x2*y3)L ; r6 += (x2*y3)H
	mulwf	[0x3]
	movf	PRODL, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r6 += (x2*y4)L ; r7 += (x2*y4)H
	mulwf	[0x4]
	movf	PRODL, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_8, F
	movf	POSTINC0, W	 ; r7 += (x2*y5)L
	mulwf	[0x5]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movf	INDF0, W	 ; r3 += (x3*y0)L ; r4 += (x3*y0)H
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_4, F
	movlw	0x00
	addwfc	TMP_REG_5, F
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r4 += (x3*y1)L ; r5 += (x3*y1)H
	mulwf	[0x1]
	movf	PRODL, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r5 += (x3*y2)L ; r6 += (x3*y2)H
	mulwf	[0x2]
	movf	PRODL, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r6 += (x3*y3)L ; r7 += (x3*y3)H
	mulwf	[0x3]
	movf	PRODL, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_8, F
	movf	POSTINC0, W	 ; r7 += (x3*y4)L
	mulwf	[0x4]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movf	INDF0, W	 ; r4 += (x4*y0)L ; r5 += (x4*y0)H
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_5, F
	movlw	0x00
	addwfc	TMP_REG_6, F
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r5 += (x4*y1)L ; r6 += (x4*y1)H
	mulwf	[0x1]
	movf	PRODL, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r6 += (x4*y2)L ; r7 += (x4*y2)H
	mulwf	[0x2]
	movf	PRODL, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_8, F
	movf	POSTINC0, W	 ; r7 += (x4*y3)L
	mulwf	[0x3]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movf	INDF0, W	 ; r5 += (x5*y0)L ; r6 += (x5*y0)H
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_6, F
	movlw	0x00
	addwfc	TMP_REG_7, F
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	INDF0, W	 ; r6 += (x5*y1)L ; r7 += (x5*y1)H
	mulwf	[0x1]
	movf	PRODL, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_8, F
	movf	POSTINC0, W	 ; r7 += (x5*y2)L
	mulwf	[0x2]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movf	INDF0, W	 ; r6 += (x6*y0)L ; r7 += (x6*y0)H
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_7, F
	movlw	0x00
	addwfc	TMP_REG_8, F
	movf	PRODH, W
	addwf	TMP_REG_8, F
	movf	POSTINC0, W	 ; r7 += (x6*y1)L
	mulwf	[0x1]		 ; FSR0 ++
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movf	INDF0, W	 ; r7 += (x7*y0)L
	mulwf	[0x0]
	movf	PRODL, W
	addwf	TMP_REG_8, F
	movff	TMP_REG_3, POSTINC1 ; flush r2 ; FSR1 ++
	movff	TMP_REG_4, POSTINC1 ; flush r3 ; FSR1 ++
	movff	TMP_REG_5, POSTINC1 ; flush r4 ; FSR1 ++
	movff	TMP_REG_6, POSTINC1 ; flush r5 ; FSR1 ++
	movff	TMP_REG_7, POSTINC1 ; flush r6 ; FSR1 ++
	movff	TMP_REG_8, POSTINC1 ; flush r7 ; FSR1 ++
	M_PRIM_INT64_2ARG_END
#endif

#ifdef caml_useprim_caml_int64_div
caml_int64_div:
	movsf	[0x1], FSR0L	    ; y = 0 ?
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x2
	movf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	INDF0, W
	btfsc	STATUS, Z
	goto	caml_raise_division_by_0
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_1 ; x = TMP_REG_D:C:B:A:4:3:2:1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	POSTINC0, TMP_REG_4
	movff	POSTINC0, TMP_REG_A
	movff	POSTINC0, TMP_REG_B
	movff	POSTINC0, TMP_REG_C
	movff	INDF0, TMP_REG_D
	M_CREATE_INT64		    ; ACCU <- new int64
	movsf	[0x1], FSR0L	    ; y = GC_TMP_REG_4:3:2:1:TMP_REG_8:7:6:5
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_5
	movff	POSTINC0, TMP_REG_6
	movff	POSTINC0, TMP_REG_7
	movff	POSTINC0, TMP_REG_8
	movff	POSTINC0, GC_TMP_REG_1
	movff	POSTINC0, GC_TMP_REG_2
	movff	POSTINC0, GC_TMP_REG_3
	movff	INDF0, GC_TMP_REG_4
	bcf	TMP_REG_9, 0	    ; TMP_REG_9.0 = result sign
	btfss	TMP_REG_D, 7	    ; x < 0 ?
	bra	caml_int64_div_l0   ; no => skip
	btg	TMP_REG_9, 0	    ; update result sign
	comf	TMP_REG_1, F	    ; negate x
	comf	TMP_REG_2, F
	comf	TMP_REG_3, F
	comf	TMP_REG_4, F
	comf	TMP_REG_A, F
	comf	TMP_REG_B, F
	comf	TMP_REG_C, F
	comf	TMP_REG_D, F
	incf	TMP_REG_1, F
	bnc	caml_int64_div_l0
	incf	TMP_REG_2, F
	bnc	caml_int64_div_l0
	incf	TMP_REG_3, F
	bnc	caml_int64_div_l0
	incf	TMP_REG_4, F
	bnc	caml_int64_div_l0
	incf	TMP_REG_A, F
	bnc	caml_int64_div_l0
	incf	TMP_REG_B, F
	bnc	caml_int64_div_l0
	incf	TMP_REG_C, F
	bnc	caml_int64_div_l0
	incf	TMP_REG_D, F
caml_int64_div_l0:
	btfss	GC_TMP_REG_4, 7	; y < 0 ?
	bra	caml_int64_div_l1 ; no => skip
	btg	TMP_REG_9, 0	; update result sign
	comf	TMP_REG_5, F	; negate y
	comf	TMP_REG_6, F
	comf	TMP_REG_7, F
	comf	TMP_REG_8, F
	comf	GC_TMP_REG_1, F
	comf	GC_TMP_REG_2, F
	comf	GC_TMP_REG_3, F
	comf	GC_TMP_REG_4, F
	incf	TMP_REG_5, F
	bnc	caml_int64_div_l1
	incf	TMP_REG_6, F
	bnc	caml_int64_div_l1
	incf	TMP_REG_7, F
	bnc	caml_int64_div_l1
	incf	TMP_REG_8, F
	bnc	caml_int64_div_l1
	incf	GC_TMP_REG_1, F
	bnc	caml_int64_div_l1
	incf	GC_TMP_REG_2, F
	bnc	caml_int64_div_l1
	incf	GC_TMP_REG_3, F
	bnc	caml_int64_div_l1
	incf	GC_TMP_REG_4, F
caml_int64_div_l1:
	clrf	POSTINC1	; init result to 0
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	INDF1
	subfsr	FSR1, 0x7
	clrf	PRODL		; init bit raw to 00...01
	clrf	PRODH
	bsf	PRODL, 0
caml_int64_div_loop1:
	bcf	STATUS, C	; y <- y << 1
	rlcf	TMP_REG_5, F
	rlcf	TMP_REG_6, F
	rlcf	TMP_REG_7, F
	rlcf	TMP_REG_8, F
	rlcf	GC_TMP_REG_1, F
	rlcf	GC_TMP_REG_2, F
	rlcf	GC_TMP_REG_3, F
	rlcf	GC_TMP_REG_4, F
	movf	GC_TMP_REG_4, W	; x - y
	subwf	TMP_REG_D, W
	bnc	caml_int64_div_loop2 ; if x < y then break
	bnz	caml_int64_div_if_l1 ; if x > y then continue
	movf	GC_TMP_REG_3, W
	subwf	TMP_REG_C, W
	bnc	caml_int64_div_loop2 ; if x < y then break
	bnz	caml_int64_div_if_l1 ; if x > y then continue
	movf	GC_TMP_REG_2, W
	subwf	TMP_REG_B, W
	bnc	caml_int64_div_loop2 ; if x < y then break
	bnz	caml_int64_div_if_l1 ; if x > y then continue
	movf	GC_TMP_REG_1, W
	subwf	TMP_REG_A, W
	bnc	caml_int64_div_loop2 ; if x < y then break
	bnz	caml_int64_div_if_l1 ; if x > y then continue
	movf	TMP_REG_8, W
	subwf	TMP_REG_4, W
	bnc	caml_int64_div_loop2 ; if x < y then break
	bnz	caml_int64_div_if_l1 ; if x > y then continue
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int64_div_loop2 ; if x < y then break
	bnz	caml_int64_div_if_l1 ; if w > y then continue
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int64_div_loop2 ; if x < y then break
	bnz	caml_int64_div_if_l1 ; if w > y then continue
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bnc	caml_int64_div_loop2 ; if x < y then break
caml_int64_div_if_l1:
	bcf	STATUS, C	; raw <- raw << 1
	rlcf	PRODL, F
	bnc	caml_int64_div_loop1 ; loop
	bsf	PRODL, 0
	incf	PRODH, F
	bra	caml_int64_div_loop1 ; loop
caml_int64_div_loop2:
	bcf	STATUS, C	; y <- y >> 1
	rrcf	GC_TMP_REG_4, F
	rrcf	GC_TMP_REG_3, F
	rrcf	GC_TMP_REG_2, F
	rrcf	GC_TMP_REG_1, F
	rrcf	TMP_REG_8, F
	rrcf	TMP_REG_7, F
	rrcf	TMP_REG_6, F
	rrcf	TMP_REG_5, F
	movf	GC_TMP_REG_4, W	; x - y
	subwf	TMP_REG_D, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
	bnz	caml_int64_div_if_l2  ; if x > y then continue
	movf	GC_TMP_REG_3, W
	subwf	TMP_REG_C, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
	bnz	caml_int64_div_if_l2  ; if x > y then continue
	movf	GC_TMP_REG_2, W
	subwf	TMP_REG_B, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
	bnz	caml_int64_div_if_l2  ; if x > y then continue
	movf	GC_TMP_REG_1, W
	subwf	TMP_REG_A, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
	bnz	caml_int64_div_if_l2  ; if x > y then continue
	movf	TMP_REG_8, W
	subwf	TMP_REG_4, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
	bnz	caml_int64_div_if_l2  ; if x > y then continue
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
	bnz	caml_int64_div_if_l2  ; if x > y then continue
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
	bnz	caml_int64_div_if_l2  ; if x > y then continue
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bnc	caml_int64_div_ifn_l2 ; if x < y then skip
caml_int64_div_if_l2:
	movf	TMP_REG_5, W	; x <- x - y
	subwf	TMP_REG_1, F
	movf	TMP_REG_6, W
	subwfb	TMP_REG_2, F
	movf	TMP_REG_7, W
	subwfb	TMP_REG_3, F
	movf	TMP_REG_8, W
	subwfb	TMP_REG_4, F
	movf	GC_TMP_REG_1, W
	subwfb	TMP_REG_A, F
	movf	GC_TMP_REG_2, W
	subwfb	TMP_REG_B, F
	movf	GC_TMP_REG_3, W
	subwfb	TMP_REG_C, F
	movf	GC_TMP_REG_4, W
	subwfb	TMP_REG_D, F
	movf	PRODH, W	; res <- res | raw
	addwf	FSR1L, F
	btfsc	STATUS, C
	incf	FSR1H, F
	movf	PRODL, W
	iorwf	INDF1, F
	movf	PRODH, W
	subwf	FSR1L, F
	btfss	STATUS, C
	decf	FSR1H, F
caml_int64_div_ifn_l2:
	bcf	STATUS, C	; raw <- raw >> 1
	rrcf	PRODL, F
	bnc	caml_int64_div_loop2
	bsf	PRODL, 7
	decf	PRODH, F
	btfss	PRODH, 7		; if not raw overflow
	bra	caml_int64_div_loop2 	; then loop
	addfsr	FSR1, 0x8
	btfss	TMP_REG_9, 0		; negative result ?
	return				; no => division done
	subfsr	FSR1, 0x1   		; yes => negate result
	comf	POSTDEC1, F
	comf	POSTDEC1, F
	comf	POSTDEC1, F
	comf	POSTDEC1, F
	comf	POSTDEC1, F
	comf	POSTDEC1, F
	comf	POSTDEC1, F
	comf	INDF1, F
	incf	POSTINC1, F
	bnc	caml_int64_div_end7
	incf	POSTINC1, F
	bnc	caml_int64_div_end6
	incf	POSTINC1, F
	bnc	caml_int64_div_end5
	incf	POSTINC1, F
	bnc	caml_int64_div_end4
	incf	POSTINC1, F
	bnc	caml_int64_div_end3
	incf	POSTINC1, F
	bnc	caml_int64_div_end2
	incf	POSTINC1, F
	bnc	caml_int64_div_end1
	incf	POSTINC1, F
	return
caml_int64_div_end7:
	addfsr	FSR1, 0x7
	return
caml_int64_div_end6:
	addfsr	FSR1, 0x6
	return
caml_int64_div_end5:
	addfsr	FSR1, 0x5
	return
caml_int64_div_end4:
	addfsr	FSR1, 0x4
	return
caml_int64_div_end3:
	addfsr	FSR1, 0x3
	return
caml_int64_div_end2:
	addfsr	FSR1, 0x2
	return
caml_int64_div_end1:
	addfsr	FSR1, 0x1
	return
#endif

#ifdef caml_useprim_caml_int64_mod
caml_int64_mod:
	movsf	[0x1], FSR0L	    ; y = 0 ?
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x2
	movf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	POSTINC0, W
	iorwf	INDF0, W
	btfsc	STATUS, Z
	goto	caml_raise_division_by_0
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_1 ; x = TMP_REG_D:C:B:A:4:3:2:1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	POSTINC0, TMP_REG_4
	movff	POSTINC0, TMP_REG_A
	movff	POSTINC0, TMP_REG_B
	movff	POSTINC0, TMP_REG_C
	movff	INDF0, TMP_REG_D
	M_CREATE_INT64		    ; ACCU <- new int64
	movsf	[0x1], FSR0L	    ; y = GC_TMP_REG_4:3:2:1:TMP_REG_8:7:6:5
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_5
	movff	POSTINC0, TMP_REG_6
	movff	POSTINC0, TMP_REG_7
	movff	POSTINC0, TMP_REG_8
	movff	POSTINC0, GC_TMP_REG_1
	movff	POSTINC0, GC_TMP_REG_2
	movff	POSTINC0, GC_TMP_REG_3
	movff	INDF0, GC_TMP_REG_4
	bcf	TMP_REG_9, 0	    ; TMP_REG_9.0 = result sign
	btfss	TMP_REG_D, 7	    ; x < 0 ?
	bra	caml_int64_mod_l0   ; no => skip
	btg	TMP_REG_9, 0	    ; update result sign
	comf	TMP_REG_1, F	    ; negate x
	comf	TMP_REG_2, F
	comf	TMP_REG_3, F
	comf	TMP_REG_4, F
	comf	TMP_REG_A, F
	comf	TMP_REG_B, F
	comf	TMP_REG_C, F
	comf	TMP_REG_D, F
	incf	TMP_REG_1, F
	bnc	caml_int64_mod_l0
	incf	TMP_REG_2, F
	bnc	caml_int64_mod_l0
	incf	TMP_REG_3, F
	bnc	caml_int64_mod_l0
	incf	TMP_REG_4, F
	bnc	caml_int64_mod_l0
	incf	TMP_REG_A, F
	bnc	caml_int64_mod_l0
	incf	TMP_REG_B, F
	bnc	caml_int64_mod_l0
	incf	TMP_REG_C, F
	bnc	caml_int64_mod_l0
	incf	TMP_REG_D, F
caml_int64_mod_l0:
	btfss	GC_TMP_REG_4, 7	; y < 0 ?
	bra	caml_int64_mod_l1 ; no => skip
	btg	TMP_REG_9, 0	; update result sign
	comf	TMP_REG_5, F	; negate y
	comf	TMP_REG_6, F
	comf	TMP_REG_7, F
	comf	TMP_REG_8, F
	comf	GC_TMP_REG_1, F
	comf	GC_TMP_REG_2, F
	comf	GC_TMP_REG_3, F
	comf	GC_TMP_REG_4, F
	incf	TMP_REG_5, F
	bnc	caml_int64_mod_l1
	incf	TMP_REG_6, F
	bnc	caml_int64_mod_l1
	incf	TMP_REG_7, F
	bnc	caml_int64_mod_l1
	incf	TMP_REG_8, F
	bnc	caml_int64_mod_l1
	incf	GC_TMP_REG_1, F
	bnc	caml_int64_mod_l1
	incf	GC_TMP_REG_2, F
	bnc	caml_int64_mod_l1
	incf	GC_TMP_REG_3, F
	bnc	caml_int64_mod_l1
	incf	GC_TMP_REG_4, F
caml_int64_mod_l1:
	clrf	TMP_REG_E	; counter = TMP_REG_E <- 0
caml_int64_mod_loop1:
	bcf	STATUS, C	; y <- y << 1
	rlcf	TMP_REG_5, F
	rlcf	TMP_REG_6, F
	rlcf	TMP_REG_7, F
	rlcf	TMP_REG_8, F
	rlcf	GC_TMP_REG_1, F
	rlcf	GC_TMP_REG_2, F
	rlcf	GC_TMP_REG_3, F
	rlcf	GC_TMP_REG_4, F
	incf	TMP_REG_E, F	; incr counter
	movf	GC_TMP_REG_4, W	; x - y
	subwf	TMP_REG_D, W
	bnc	caml_int64_mod_loop2 ; if x < y then break
	bnz	caml_int64_mod_loop1 ; if x > y then loop
	movf	GC_TMP_REG_3, W
	subwf	TMP_REG_C, W
	bnc	caml_int64_mod_loop2 ; if x < y then break
	bnz	caml_int64_mod_loop1 ; if x > y then loop
	movf	GC_TMP_REG_2, W
	subwf	TMP_REG_B, W
	bnc	caml_int64_mod_loop2 ; if x < y then break
	bnz	caml_int64_mod_loop1 ; if x > y then loop
	movf	GC_TMP_REG_1, W
	subwf	TMP_REG_A, W
	bnc	caml_int64_mod_loop2 ; if x < y then break
	bnz	caml_int64_mod_loop1 ; if x > y then loop
	movf	TMP_REG_8, W
	subwf	TMP_REG_4, W
	bnc	caml_int64_mod_loop2 ; if x < y then break
	bnz	caml_int64_mod_loop1 ; if x > y then loop
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int64_mod_loop2 ; if x < y then break
	bnz	caml_int64_mod_loop1 ; if x > y then loop
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int64_mod_loop2 ; if x < y then break
	bnz	caml_int64_mod_loop1 ; if x > y then loop
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bc	caml_int64_mod_loop1 ; if x >= y then loop
caml_int64_mod_loop2:
	bcf	STATUS, C	; y <- y >> 1
	rrcf	GC_TMP_REG_4, F
	rrcf	GC_TMP_REG_3, F
	rrcf	GC_TMP_REG_2, F
	rrcf	GC_TMP_REG_1, F
	rrcf	TMP_REG_8, F
	rrcf	TMP_REG_7, F
	rrcf	TMP_REG_6, F
	rrcf	TMP_REG_5, F
	movf	GC_TMP_REG_4, W	; x - y
	subwf	TMP_REG_D, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int64_mod_if_l2  ; if x > y then do not skip
	movf	GC_TMP_REG_3, W
	subwf	TMP_REG_C, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int64_mod_if_l2  ; if x > y then do not skip
	movf	GC_TMP_REG_2, W
	subwf	TMP_REG_B, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int64_mod_if_l2  ; if x > y then do not skip
	movf	GC_TMP_REG_1, W
	subwf	TMP_REG_A, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int64_mod_if_l2  ; if x > y then do not skip
	movf	TMP_REG_8, W
	subwf	TMP_REG_4, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int64_mod_if_l2  ; if x > y then do not skip
	movf	TMP_REG_7, W
	subwf	TMP_REG_3, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int64_mod_if_l2  ; if x > y then do not skip
	movf	TMP_REG_6, W
	subwf	TMP_REG_2, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
	bnz	caml_int64_mod_if_l2  ; if x > y then do not skip
	movf	TMP_REG_5, W
	subwf	TMP_REG_1, W
	bnc	caml_int64_mod_ifn_l2 ; if x < y then skip
caml_int64_mod_if_l2:
	movf	TMP_REG_5, W	; x <- x - y
	subwf	TMP_REG_1, F
	movf	TMP_REG_6, W
	subwfb	TMP_REG_2, F
	movf	TMP_REG_7, W
	subwfb	TMP_REG_3, F
	movf	TMP_REG_8, W
	subwfb	TMP_REG_4, F
	movf	GC_TMP_REG_1, W
	subwfb	TMP_REG_A, F
	movf	GC_TMP_REG_2, W
	subwfb	TMP_REG_B, F
	movf	GC_TMP_REG_3, W
	subwfb	TMP_REG_C, F
	movf	GC_TMP_REG_4, W
	subwfb	TMP_REG_D, F
caml_int64_mod_ifn_l2:
	decfsz	TMP_REG_E, F	; decr counter
	bra	caml_int64_mod_loop2 ; if counter <> 0 then loop
	btfss	TMP_REG_9, 0	; negative result ?
	bra	caml_int64_mod_end ; no => modulo done
	comf	TMP_REG_1, F	   ; yes => negate result
	comf	TMP_REG_2, F
	comf	TMP_REG_3, F
	comf	TMP_REG_4, F
	comf	TMP_REG_A, F
	comf	TMP_REG_B, F
	comf	TMP_REG_C, F
	comf	TMP_REG_D, F
	incf	TMP_REG_1, F
	bnc	caml_int64_mod_end
	incf	TMP_REG_2, F
	bnc	caml_int64_mod_end
	incf	TMP_REG_3, F
	bnc	caml_int64_mod_end
	incf	TMP_REG_4, F
	bnc	caml_int64_mod_end
	incf	TMP_REG_A, F
	bnc	caml_int64_mod_end
	incf	TMP_REG_B, F
	bnc	caml_int64_mod_end
	incf	TMP_REG_C, F
	bnc	caml_int64_mod_end
	incf	TMP_REG_D, F
caml_int64_mod_end:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	movff	TMP_REG_A, POSTINC1
	movff	TMP_REG_B, POSTINC1
	movff	TMP_REG_C, POSTINC1
	movff	TMP_REG_D, POSTINC1
	return
#endif

#ifdef caml_useprim_caml_int64_and
caml_int64_and:
	M_PRIM_INT64_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 4
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 5
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 6
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 7
	andwf	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 8
	andwf	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT64_2ARG_END
#endif

#ifdef caml_useprim_caml_int64_or
caml_int64_or:
	M_PRIM_INT64_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 4
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 5
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 6
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 7
	iorwf	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 8
	iorwf	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT64_2ARG_END
#endif

#ifdef caml_useprim_caml_int64_xor
caml_int64_xor:
	M_PRIM_INT64_2ARG_INIT
	movf	POSTINC2, W	 ; 1
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 2
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 3
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 4
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 5
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 6
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	POSTINC2, W	 ; 7
	xorwf	POSTINC0, W
	movwf	POSTINC1
	movf	INDF2, W	 ; 8
	xorwf	INDF0, W
	movwf	POSTINC1
	M_PRIM_INT64_2ARG_END
#endif

#ifdef caml_useprim_caml_int64_shift_left
caml_int64_shift_left:
	M_PRIM_INT64_1ARG_INIT
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	POSTINC0, TMP_REG_4
	movff	POSTINC0, TMP_REG_5
	movff	POSTINC0, TMP_REG_6
	movff	POSTINC0, TMP_REG_7
	movff	INDF0, TMP_REG_8
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	caml_int64_shift_left_end_loop
caml_int64_shift_left_loop:
	rlcf	TMP_REG_8, W	; STATUS.C ignored
	rlcf	TMP_REG_1, F
	rlcf	TMP_REG_2, F
	rlcf	TMP_REG_3, F
	rlcf	TMP_REG_4, F
	rlcf	TMP_REG_5, F
	rlcf	TMP_REG_6, F
	rlcf	TMP_REG_7, F
	rlcf	TMP_REG_8, F
	decfsz	[0x1], F
	bra	caml_int64_shift_left_loop
caml_int64_shift_left_end_loop:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	movff	TMP_REG_5, POSTINC1
	movff	TMP_REG_6, POSTINC1
	movff	TMP_REG_7, POSTINC1
	movff	TMP_REG_8, POSTINC1
	M_PRIM_INT64_1ARG_END
#endif

#ifdef caml_useprim_caml_int64_shift_right
caml_int64_shift_right:
	M_PRIM_INT64_1ARG_INIT
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	POSTINC0, TMP_REG_4
	movff	POSTINC0, TMP_REG_5
	movff	POSTINC0, TMP_REG_6
	movff	POSTINC0, TMP_REG_7
	movff	INDF0, TMP_REG_8
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	caml_int64_shift_right_end_loop
caml_int64_shift_right_loop:
	bcf	STATUS, C
	rrcf	TMP_REG_8, F
	rrcf	TMP_REG_7, F
	rrcf	TMP_REG_6, F
	rrcf	TMP_REG_5, F
	rrcf	TMP_REG_4, F
	rrcf	TMP_REG_3, F
	rrcf	TMP_REG_2, F
	rrcf	TMP_REG_1, F
	decfsz	[0x1], F
	bra	caml_int64_shift_right_loop
caml_int64_shift_right_end_loop:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	movff	TMP_REG_5, POSTINC1
	movff	TMP_REG_6, POSTINC1
	movff	TMP_REG_7, POSTINC1
	movff	TMP_REG_8, POSTINC1
	M_PRIM_INT64_1ARG_END
#endif

#ifdef caml_useprim_caml_int64_shift_right_unsigned
caml_int64_shift_right_unsigned:
	M_PRIM_INT64_1ARG_INIT
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	POSTINC0, TMP_REG_4
	movff	POSTINC0, TMP_REG_5
	movff	POSTINC0, TMP_REG_6
	movff	POSTINC0, TMP_REG_7
	movff	INDF0, TMP_REG_8
	bcf	STATUS, C
	rrcf	[0x1], F
	bz	caml_int64_shift_right_unsigned_end_loop
caml_int64_shift_right_unsigned_loop:
	bcf	STATUS, C
	rrcf	TMP_REG_8, F
	rrcf	TMP_REG_7, F
	rrcf	TMP_REG_6, F
	rrcf	TMP_REG_5, F
	rrcf	TMP_REG_4, F
	rrcf	TMP_REG_3, F
	rrcf	TMP_REG_2, F
	rrcf	TMP_REG_1, F
	decfsz	[0x1], F
	bra	caml_int64_shift_right_unsigned_loop
caml_int64_shift_right_unsigned_end_loop:
	movff	TMP_REG_1, POSTINC1
	movff	TMP_REG_2, POSTINC1
	movff	TMP_REG_3, POSTINC1
	movff	TMP_REG_4, POSTINC1
	movff	TMP_REG_5, POSTINC1
	movff	TMP_REG_6, POSTINC1
	movff	TMP_REG_7, POSTINC1
	movff	TMP_REG_8, POSTINC1
	M_PRIM_INT64_1ARG_END
#endif

#ifdef caml_useprim_caml_int64_of_int
caml_int64_of_int:
	movff	ACCUL, TMP_REG_1 ; TMP_REG_1:2 <- ACCU
	movff	ACCUH, TMP_REG_2
	M_CREATE_INT64
	rrcf	TMP_REG_2, W	 ; STATUS.C ignored
	rrcf	TMP_REG_1, W
	movwf	POSTINC1
	rlcf	TMP_REG_2, W	 ; STATUS.C ignored
	bc	caml_int64_of_int_neg
	rrcf	TMP_REG_2, W
	movwf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	return
caml_int64_of_int_neg:
	rrcf	TMP_REG_2, W
	movwf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	return
#endif

#ifdef caml_useprim_caml_int64_of_int32
caml_int64_of_int32:
	M_PRIM_INT64_1ARG_INIT
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movff	INDF0, POSTINC1
	btfsc	INDF0, 7
	bra	caml_int64_of_int32_neg
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	clrf	POSTINC1
	M_PRIM_INT64_1ARG_END
caml_int64_of_int32_neg:
	setf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	setf	POSTINC1
	M_PRIM_INT64_1ARG_END
#endif

#ifdef caml_useprim_caml_int64_to_int32
caml_int64_to_int32:
	M_PRIM_INT32_1ARG_INIT
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	M_PRIM_INT32_1ARG_END
#endif

#ifdef caml_useprim_caml_int64_1arg
caml_int64_1arg_init:
	movff	ACCUL, TMP_REG_1
	movff	ACCUH, TMP_REG_2
	M_CREATE_INT64		 ; ACCU <- alloc(int64)
	movff	TMP_REG_1, FSR0L ; FSR0 <- old ACCU
	movff	TMP_REG_2, FSR0H
	addfsr	FSR0, 0x2
	return
#endif

#ifdef caml_useprim_caml_int64_2arg
caml_int64_2arg_init:
	movff	ACCUL, TMP_REG_1
	movff	ACCUH, TMP_REG_2
	M_CREATE_INT64		 ; ACCU <- alloc(int64)
	movff	TMP_REG_1, FSR0L ; FSR0 <- old ACCU
	movff	TMP_REG_2, FSR0H
	addfsr	FSR0, 0x2
	movff	FSR2L, TMP_REG_1 ; save FSR2
	movff	FSR2H, TMP_REG_2
	movf	[0x1], W	 ; FSR2 <- arg2
	movsf	[0x2], FSR2H
	movwf	FSR2L
	addfsr	FSR2, 0x2
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              CUSTOMS              ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

caml_custom_skip_addr:

#ifdef caml_useprim_caml_int32_custom
#ifndef caml_useprim_caml_int32_compare
#define caml_useprim_caml_int32_compare
#endif
#ifndef caml_useprim_caml_int32_to_int
#define caml_useprim_caml_int32_to_int
#endif
	org	(caml_externals - 0x20)	; WARNING: copy in bc2asm/constants.ml
caml_int32_custom:
	dcfsnz	PRODL, F
	goto	caml_int32_compare 	; compare
	dcfsnz	PRODL, F
	goto	caml_int32_to_int 	; hash
	goto	_STOP
#endif

#ifdef caml_useprim_caml_int64_custom
#ifndef caml_useprim_caml_int64_compare
#define caml_useprim_caml_int64_compare
#endif
#ifndef caml_useprim_caml_int64_to_int
#define caml_useprim_caml_int64_to_int
#endif
	org	(caml_externals - 0x10)	; WARNING: copy in bc2asm/constants.ml
caml_int64_custom:
	dcfsnz	PRODL, F
	goto	caml_int64_compare 	; compare
	dcfsnz	PRODL, F
	goto	caml_int64_to_int 	; hash
	goto	_STOP
#endif

	org caml_custom_skip_addr

#ifdef caml_useprim_caml_int32_to_int
#ifndef caml_useprim_caml_int64_or_int32_to_int
#define caml_useprim_caml_int64_or_int32_to_int
#endif
caml_int32_to_int:
#endif
#ifdef caml_useprim_caml_int64_to_int
#ifndef caml_useprim_caml_int64_or_int32_to_int
#define caml_useprim_caml_int64_or_int32_to_int
#endif
caml_int64_to_int:
#endif
#ifdef caml_useprim_caml_int64_or_int32_to_int
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	bsf	STATUS, C
	rlcf	POSTINC0, W
	movwf	ACCUL
	rlcf	INDF0, W
	movwf	ACCUH
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;             COMPARE               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_equal
#ifndef caml_useprim_caml_compare
#define caml_useprim_caml_compare
#endif
#ifndef caml_useprim_caml_compare_to_eq
#define caml_useprim_caml_compare_to_eq
#endif
caml_equal:
	rcall	caml_compare
	bra	caml_compare_to_eq
#endif

#ifdef caml_useprim_caml_eq_float
#ifndef caml_useprim_caml_float_compare
#define caml_useprim_caml_float_compare
#endif
#ifndef caml_useprim_caml_compare_to_eq
#define caml_useprim_caml_compare_to_eq
#endif
caml_eq_float:
	rcall	caml_float_compare
	bra	caml_compare_to_eq
#endif

#ifdef caml_useprim_caml_notequal
#ifndef caml_useprim_caml_compare
#define caml_useprim_caml_compare
#endif
#ifndef caml_useprim_caml_compare_to_neq
#define caml_useprim_caml_compare_to_neq
#endif
caml_notequal:
	rcall	caml_compare
	bra	caml_compare_to_neq
#endif

#ifdef caml_useprim_caml_neq_float
#ifndef caml_useprim_caml_float_compare
#define caml_useprim_caml_float_compare
#endif
#ifndef caml_useprim_caml_compare_to_neq
#define caml_useprim_caml_compare_to_neq
#endif
caml_neq_float:
	rcall	caml_float_compare
	bra	caml_compare_to_neq
#endif

#ifdef caml_useprim_caml_lessthan
#ifndef caml_useprim_caml_compare
#define caml_useprim_caml_compare
#endif
#ifndef caml_useprim_caml_compare_to_lt
#define caml_useprim_caml_compare_to_lt
#endif
caml_lessthan:
	rcall	caml_compare
	bra	caml_compare_to_lt
#endif

#ifdef caml_useprim_caml_string_lessthan
#ifndef caml_useprim_caml_string_compare
#define caml_useprim_caml_string_compare
#endif
#ifndef caml_useprim_caml_compare_to_lt
#define caml_useprim_caml_compare_to_lt
#endif
caml_string_lessthan:
	rcall	caml_string_compare
	bra	caml_compare_to_lt
#endif

#ifdef caml_useprim_caml_lt_float
#ifndef caml_useprim_caml_float_compare
#define caml_useprim_caml_float_compare
#endif
#ifndef caml_useprim_caml_compare_to_lt
#define caml_useprim_caml_compare_to_lt
#endif
caml_lt_float:
	rcall	caml_float_compare
	bra	caml_compare_to_lt
#endif

#ifdef caml_useprim_caml_lessequal
#ifndef caml_useprim_caml_compare
#define caml_useprim_caml_compare
#endif
#ifndef caml_useprim_caml_compare_to_le
#define caml_useprim_caml_compare_to_le
#endif
caml_lessequal:
	rcall	caml_compare
	bra	caml_compare_to_le
#endif

#ifdef caml_useprim_caml_string_lessequal
#ifndef caml_useprim_caml_string_compare
#define caml_useprim_caml_string_compare
#endif
#ifndef caml_useprim_caml_compare_to_le
#define caml_useprim_caml_compare_to_le
#endif
caml_string_lessequal:
	rcall	caml_string_compare
	bra	caml_compare_to_le
#endif

#ifdef caml_useprim_caml_le_float
#ifndef caml_useprim_caml_float_compare
#define caml_useprim_caml_float_compare
#endif
#ifndef caml_useprim_caml_compare_to_le
#define caml_useprim_caml_compare_to_le
#endif
caml_le_float:
	rcall	caml_float_compare
	bra	caml_compare_to_le
#endif

#ifdef caml_useprim_caml_greaterthan
#ifndef caml_useprim_caml_compare
#define caml_useprim_caml_compare
#endif
#ifndef caml_useprim_caml_compare_to_gt
#define caml_useprim_caml_compare_to_gt
#endif
caml_greaterthan:
	rcall	caml_compare
	bra	caml_compare_to_gt
#endif

#ifdef caml_useprim_caml_string_greaterthan
#ifndef caml_useprim_caml_string_compare
#define caml_useprim_caml_string_compare
#endif
#ifndef caml_useprim_caml_compare_to_gt
#define caml_useprim_caml_compare_to_gt
#endif
caml_string_greaterthan:
	rcall	caml_string_compare
	bra	caml_compare_to_gt
#endif

#ifdef caml_useprim_caml_gt_float
#ifndef caml_useprim_caml_float_compare
#define caml_useprim_caml_float_compare
#endif
#ifndef caml_useprim_caml_compare_to_gt
#define caml_useprim_caml_compare_to_gt
#endif
caml_gt_float:
	rcall	caml_float_compare
	bra	caml_compare_to_gt
#endif

#ifdef caml_useprim_caml_greaterequal
#ifndef caml_useprim_caml_compare
#define caml_useprim_caml_compare
#endif
#ifndef caml_useprim_caml_compare_to_ge
#define caml_useprim_caml_compare_to_ge
#endif
caml_greaterequal:
	rcall	caml_compare
	bra	caml_compare_to_ge
#endif

#ifdef caml_useprim_caml_string_greaterequal
#ifndef caml_useprim_caml_string_compare
#define caml_useprim_caml_string_compare
#endif
#ifndef caml_useprim_caml_compare_to_ge
#define caml_useprim_caml_compare_to_ge
#endif
caml_string_greaterequal:
	rcall	caml_string_compare
	bra	caml_compare_to_ge
#endif

#ifdef caml_useprim_caml_ge_float
#ifndef caml_useprim_caml_float_compare
#define caml_useprim_caml_float_compare
#endif
#ifndef caml_useprim_caml_compare_to_ge
#define caml_useprim_caml_compare_to_ge
#endif
caml_ge_float:
	rcall	caml_float_compare
	bra	caml_compare_to_ge
#endif

;;;

#ifdef caml_useprim_caml_compare
#ifndef caml_useprim_caml_int_compare
#define caml_useprim_caml_int_compare
#endif
#ifndef caml_useprim_caml_string_compare
#define caml_useprim_caml_string_compare
#endif
#ifndef caml_useprim_caml_float_compare
#define caml_useprim_caml_float_compare
#endif
#ifndef caml_useprim_caml_float_array_compare
#define caml_useprim_caml_float_array_compare
#endif
#ifndef caml_useprim_caml_custom_compare
#define caml_useprim_caml_custom_compare
#endif
#ifndef caml_useprim_caml_raise_ia_compare
#define caml_useprim_caml_raise_ia_compare
#endif
caml_compare:
	;; ACCU = o1
	;; [0x2]:[0x1] = o2
	
;;; Fast traitment in simple cases
	movf	ACCUL, W		; == ?
	xorwf	[0x1], W
	bnz	caml_compare_A_phydiff
	movf	ACCUH, W
	xorwf	[0x2], W
	bz	caml_compare_A_1eq2
caml_compare_A_phydiff:
	btfss	ACCUL, 0		; is_int(o1) ?
	bra	caml_compare_A_addr_x	; no
	btfss	[0x1], 0	  	; yes ; is_int(o2) ?
	bra	caml_compare_A_1lt2	; no  ; compare int addr -> return -1
	bra	caml_int_compare 	; yes ; compare int int
caml_compare_A_addr_x:
	btfsc	[0x1], 0		; is_int(o2) ?
	bra	caml_compare_A_1gt2	; yes ; compare addr int -> return 1
	
	movff	FSR1L, TMP_REG_6	; save FSR1
	movff	FSR1H, TMP_REG_7
	movff	ACCUL, FSR1L		; FSR1 <- o1
	movff	ACCUH, FSR1H
	movsf	[0x1], FSR0L		; FSR0 <- o2
	movsf	[0x2], FSR0H
	subfsr	FSR1, 0x2		; FSR1 -= 2
	subfsr	FSR0, 0x2		; FSR0 -= 2
	
	movf	POSTINC1, W		; read tag(o1) ; FSR1 ++
	subwf	INDF0, W		; tag(o2) - tag(o1)
	bnc	caml_compare_B_1gt2
	bnz	caml_compare_B_1lt2

	movf	POSTINC0, W
	xorlw	STRING_TAG		; strings ?
	bz	caml_compare_B_string_compare
	xorlw	(STRING_TAG^DOUBLE_TAG) ; float ?
	bz	caml_compare_B_float_compare
	xorlw	(DOUBLE_TAG^DOUBLE_ARRAY_TAG) ; float[] ?
	bz	caml_compare_B_float_array_compare
	xorlw	(DOUBLE_ARRAY_TAG^ABSTRACT_TAG)	; abstract ?
	bz	caml_compare_B_raise_ia_compare
	xorlw	(ABSTRACT_TAG^CLOSURE_TAG) ; closure ?
	bz	caml_compare_B_raise_ia_compare
	xorlw	(CLOSURE_TAG^INFIX_TAG) ; infix ?
	bz	caml_compare_B_raise_ia_compare
	xorlw	(INFIX_TAG^CUSTOM_TAG)	; custom ?
	bz	caml_compare_B_custom_compare
	
	movf	POSTINC1, W		; size equals ? FSR1 ++
	movwf	TMP_REG_A		; TMP_REG_A <- size

	subwf	POSTINC0, W		; size(o2) - size(o1)
	bnc	caml_compare_B_1gt2
	bnz	caml_compare_B_1lt2

;;; Not a simple case -> go through data trees
	movff	FSR2L, TMP_REG_8 	; save FSR2
	movff	FSR2H, TMP_REG_9
	bra	caml_compare_loop

;;; Tools
caml_compare_raise_stack_overflow:
	movff	TMP_REG_6, FSR1L 	; restore FSR1
	movff	TMP_REG_7, FSR1H
	movff	TMP_REG_8, FSR2L 	; restore FSR2
	movff	TMP_REG_9, FSR2H
	goto	caml_raise_stack_overflow ; raise Stack_overflow
caml_compare_B_string_compare:
	movff	TMP_REG_6, FSR1L	; restore FSR1
	movff	TMP_REG_7, FSR1H
	bra	caml_string_compare 	; goto string_compare
caml_compare_B_float_compare:
	movff	TMP_REG_6, FSR1L	; restore FSR1
	movff	TMP_REG_7, FSR1H
	bra	caml_float_compare 	; goto float_compare
caml_compare_B_float_array_compare:
	movff	TMP_REG_6, FSR1L	; restore FSR1
	movff	TMP_REG_7, FSR1H
	bra	caml_float_array_compare; goto float_array_compare
caml_compare_B_custom_compare:
	movff	TMP_REG_6, FSR1L	; restore FSR1
	movff	TMP_REG_7, FSR1H
	bra	caml_custom_compare	; goto custom_compare
caml_compare_CD_raise_ia_compare:
	movff	TMP_REG_8, FSR2L 	; restore FSR2
	movff	TMP_REG_9, FSR2H
caml_compare_B_raise_ia_compare:
	movff	TMP_REG_6, FSR1L 	; restore FSR1
	movff	TMP_REG_7, FSR1H
	goto	caml_raise_ia_compare 	; raise Invalid_argument "compare"
caml_compare_CD_1lt2:
	movff	TMP_REG_8, FSR2L 	; restore FSR2
	movff	TMP_REG_9, FSR2H
caml_compare_B_1lt2:
	movff	TMP_REG_6, FSR1L 	; restore FSR1
	movff	TMP_REG_7, FSR1H
caml_compare_A_1lt2:
	setf	ACCUL			; return -1
	setf	ACCUH
	return
caml_compare_CD_1gt2:
	movff	TMP_REG_8, FSR2L 	; restore FSR2
	movff	TMP_REG_9, FSR2H
caml_compare_B_1gt2:
	movff	TMP_REG_6, FSR1L 	; restore FSR1
	movff	TMP_REG_7, FSR1H
caml_compare_A_1gt2:
	M_CONST	1			; return 1
	return
caml_compare_CD_1ne2:
	movff	TMP_REG_8, FSR2L 	; restore FSR2
	movff	TMP_REG_9, FSR2H
	movff	TMP_REG_6, FSR1L 	; restore FSR1
	movff	TMP_REG_7, FSR1H
	return
caml_compare_CD_1eq2:
	movff	TMP_REG_6, FSR1L 	; restore FSR1
	movff	TMP_REG_7, FSR1H
caml_compare_A_1eq2:
	M_CONST	0			; return 0
	return
	
;;; Loop
caml_compare_forward:
	movff	POSTINC0, TMP_REG_A	; TMP_REG_A <- size ; FSR0 ++
caml_compare_loop:
	movf	POSTINC0, W		; stack[0] <- *FSR0++
	movwf	[0x1]
	movf	POSTINC0, W
	movwf	[0x2]
	movff	POSTINC1, ACCUL		; ACCU <- *FSR1++
	movff	POSTINC1, ACCUH

;;; Simple cases
	movf	ACCUL, W		; == ?
	xorwf	[0x1], W
	bnz	caml_compare_C_phydiff
	movf	ACCUH, W
	xorwf	[0x2], W
	bz	caml_compare_C_1eq2 	; if e1 == e2 -> branch
caml_compare_C_phydiff:
	btfss	ACCUL, 0		; is_int(e1) ?
	bra	caml_compare_C_addr_x	; no
	btfss	[0x1], 0	  	; yes ; is_int(e2) ?
	bra	caml_compare_CD_1lt2	; no  ; compare int addr -> return -1
	rcall	caml_int_compare 	; yes ; compare int int
	bra	caml_compare_C_compared
caml_compare_C_addr_x:
	btfsc	[0x1], 0		; is_int(e2) ?
	bra	caml_compare_CD_1gt2	; yes ; compare addr int -> return 1

	movff	FSR0L, TMP_REG_B 	; save FSR0 and FSR1
	movff	FSR0H, TMP_REG_C
	movff	FSR1L, TMP_REG_D
	movff	FSR1H, TMP_REG_E
	movsf	[0x1], FSR0L		; FSR0 <- stack[0]
	movsf	[0x2], FSR0H
	movff	ACCUL, FSR1L		; FSR1 <- ACCU
	movff	ACCUH, FSR1H
	subfsr	FSR1, 0x2		; FSR1 -= 2
	subfsr	FSR0, 0x2		; FSR0 -= 2

	movf	POSTINC1, W		; read tag(e1) ; FSR1 ++
	subwf	INDF0, W		; tag(e2) - tag(e1)
	bnc	caml_compare_CD_1gt2
	bnz	caml_compare_CD_1lt2

	movf	POSTINC0, W
	xorlw	STRING_TAG		; strings ?
	bz	caml_compare_D_string_compare
	xorlw	(STRING_TAG^DOUBLE_TAG) ; float ?
	bz	caml_compare_D_float_compare
	xorlw	(DOUBLE_TAG^DOUBLE_ARRAY_TAG) ; float[] ?
	bz	caml_compare_D_float_array_compare
	xorlw	(DOUBLE_ARRAY_TAG^CUSTOM_TAG) ; custom ?
	bz	caml_compare_D_custom_compare
	xorlw	(CUSTOM_TAG^ABSTRACT_TAG) ; abstract ?
	bz	caml_compare_CD_raise_ia_compare
	xorlw	(ABSTRACT_TAG^CLOSURE_TAG) ; closure ?
	bz	caml_compare_CD_raise_ia_compare
	xorlw	(CLOSURE_TAG^INFIX_TAG) ; infix ?
	bz	caml_compare_CD_raise_ia_compare
	
	movf	POSTINC1, W		; compare sizes
	subwf	INDF0, W		; size(e2) - size(e1)
	bnc	caml_compare_CD_1gt2
	bnz	caml_compare_CD_1lt2

;;; Complex case -> forward
	dcfsnz	TMP_REG_A, W		; if counter = 1
	bra	caml_compare_forward	; then skip push
caml_compare_push_forward:
	movlw	STACK_END		; check stack overflow
	cpfseq	FSR2H
	bra	caml_compare_canpush3
	movlw	-0x6
	addwf	FSR2L, W
	btfss	STATUS, C
	bra	caml_compare_raise_stack_overflow
caml_compare_canpush3:
	movf	TMP_REG_A, W		; stack[0] <- counter
	movwf	[0x1]
	movff	TMP_REG_C, POSTDEC2 	; push save(FSR0)
	movff	TMP_REG_B, POSTDEC2
	movff	TMP_REG_E, POSTDEC2 	; push save(FSR1)
	movff	TMP_REG_D, POSTDEC2
	subfsr	FSR2, 0x2	  	; push X
	bra	caml_compare_forward	; forward

caml_compare_C_compared:
	btfsc	ACCUL, 1		; sub-rountine returned 0 ?
	bra	caml_compare_CD_1ne2
	bra	caml_compare_C_1eq2
	
caml_compare_D_string_compare:
	rcall	caml_string_compare 	; compare string string
	bra	caml_compare_D_compared
caml_compare_D_float_compare:
	rcall	caml_float_compare 	; compare float float
	bra	caml_compare_D_compared
caml_compare_D_float_array_compare:
	rcall	caml_float_array_compare; compare float[] float[]
	bra	caml_compare_D_compared
caml_compare_D_custom_compare:		; compare custom custom
	rcall	caml_custom_compare
	bra	caml_compare_D_compared

caml_compare_D_compared:
	btfsc	ACCUL, 1		; sub-rountine returned 0 ?
	bra	caml_compare_CD_1ne2	; no -> finished
caml_compare_D_1eq2:
	movff	TMP_REG_B, FSR0L 	; restore FSR0 and FSR1
	movff	TMP_REG_C, FSR0H
	movff	TMP_REG_D, FSR1L
	movff	TMP_REG_E, FSR1H
caml_compare_C_1eq2:
	decfsz	TMP_REG_A, F		; decr counter ; counter = 0 ?
	bra	caml_compare_loop	; no -> loop
	movf	FSR2L, W		; FSR2 = save(FSR2) ?
	xorwf	TMP_REG_8, W
	bnz	caml_compare_goback
	movf	FSR2H, W
	xorwf	TMP_REG_9, W
	bz	caml_compare_CD_1eq2
caml_compare_goback:
	addfsr	FSR2, 0x2		; pop X
	movff	PREINC2, FSR1L		; pop FSR1
	movff	PREINC2, FSR1H
	movff	PREINC2, FSR0L		; pop FSR0
	movff	PREINC2, FSR0H
	decf	[0x1], W		; peek counter--
	movwf	TMP_REG_A
	bra	caml_compare_loop	; loop
#endif

;;;
	
#ifdef caml_useprim_caml_string_compare
caml_string_compare:
	;; ACCU = str1
	;; [0x2]:[0x1] = str2
	movff	FSR2L, TMP_REG_1	; save stack top
	movff	FSR2H, TMP_REG_2
	movsf	[0x1], TMP_REG_3	; FSR2 <- str2
	movsf	[0x2], TMP_REG_4
	movff	TMP_REG_3, FSR2L
	movff	TMP_REG_4, FSR2H
	movff	ACCUL, FSR0L		; FSR0 <- str1
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1		; FSR0 --
	subfsr	FSR2, 0x1		; FSR2 --
	decf	POSTINC0, W		; TMP_REG_{3,5} <- blk_size(str1) - 1
	movwf	TMP_REG_3
	movwf	TMP_REG_5
	decf	POSTINC2, W		; TMP_REG_4 <- blk_size(str2) - 1
	movwf	TMP_REG_4
	subwf	TMP_REG_5, W		; blk_size(str1) - blk_size(str2)
	bnc	caml_string_compare_L0
	movff	TMP_REG_4, TMP_REG_5	; TMP_REG_5 = min(blk_sizes) - 1
caml_string_compare_L0:
	movf	TMP_REG_5, F
	bz	caml_string_compare_loop_end
caml_string_compare_loop:
	movf	POSTINC0, W
	subwf	POSTINC2, W		; str2.[i] - str1.[i]
	bnz	caml_string_compare_diff
	movf	POSTINC0, W
	subwf	POSTINC2, W
	bnz	caml_string_compare_diff
	decfsz	TMP_REG_5, F		; loop min(blk_size) - 1 times
	bra	caml_string_compare_loop
caml_string_compare_loop_end:
	movf	TMP_REG_4, W
	subwf	TMP_REG_3, W		; blk_size(str1) - blk_size(str2)
	bz	caml_string_compare_blen_eq
	bnc	caml_string_compre_bl1_lt_bl2
caml_string_compare_bl1_gt_bl2:
	movf	[0x1], W
	bz	caml_string_compare_cont1
	bra	caml_string_compare_1gt2
caml_string_compre_bl1_lt_bl2:
	addfsr	FSR0, 0x1
	movf	POSTDEC0, W
	bz	caml_string_compare_cont1
	bra	caml_string_compare_1lt2
caml_string_compare_blen_eq:
	movf	[0x1], W
	bz	caml_string_compare_l2odd
	addfsr	FSR0, 0x1
	movf	POSTDEC0, W
	bz	caml_string_compare_1gt2
	M_CONST 0		 	; str1 = str2 ; return 0
	movff	TMP_REG_1, FSR2L 	; restore stack top
	movff	TMP_REG_2, FSR2H
	return
caml_string_compare_l2odd:
	addfsr	FSR0, 0x1
	movf	POSTDEC0, W
	bnz	caml_string_compare_1lt2
	movf	POSTINC0, W
	subwf	POSTINC2, W
	bnz	caml_string_compare_diff
	M_CONST 0		 	; str1 = str2 ; return 0
	movff	TMP_REG_1, FSR2L 	; restore stack top
	movff	TMP_REG_2, FSR2H
	return
caml_string_compare_cont1:
	movf	POSTINC0, W
	subwf	POSTINC2, W
caml_string_compare_diff:
	bc	caml_string_compare_1lt2
caml_string_compare_1gt2
	M_CONST	1		 	; str1 > str2 ; return 1
	movff	TMP_REG_1, FSR2L 	; restore stack top
	movff	TMP_REG_2, FSR2H
	return
caml_string_compare_1lt2:		; str1 < str2
	setf	ACCUL			; return -1
	setf	ACCUH
	movff	TMP_REG_1, FSR2L 	; restore stack top
	movff	TMP_REG_2, FSR2H
	return
#endif

#ifdef caml_useprim_caml_string_notequal
#ifndef caml_useprim_caml_string_equal
#define caml_useprim_caml_string_equal
#endif
caml_string_notequal:
	rcall	caml_string_equal
	btg	ACCUL, 1
	return
#endif

#ifdef caml_useprim_caml_string_equal
caml_string_equal:
	;; ACCU = str1
	;; [0x2]:[0x1] = str2
	movff	FSR2L, TMP_REG_1	; save stack top
	movff	FSR2H, TMP_REG_2
	movsf	[0x1], TMP_REG_3	; FSR2 <- str2
	movsf	[0x2], TMP_REG_4
	movff	TMP_REG_3, FSR2L
	movff	TMP_REG_4, FSR2H
	movff	ACCUL, FSR0L		; FSR0 <- str1
	movff	ACCUH, FSR0H
	subfsr	FSR0, 0x1		; FSR0 --
	subfsr	FSR2, 0x1		; FSR2 --
	movf	POSTINC0, W		; compare size
	xorwf	INDF2, W
	bnz	caml_string_equal_diff  ; if different sizes -> return false
	movff	POSTINC2, TMP_REG_3 	; TMP_REG_3 <- size
caml_string_equal_loop:
	movf	POSTINC0, W
	xorwf	POSTINC2, W
	bnz	caml_string_equal_diff
	movf	POSTINC0, W
	xorwf	POSTINC2, W
	bnz	caml_string_equal_diff
	decfsz	TMP_REG_3, F
	bra	caml_string_equal_loop
	M_CONST	1		 	; return 1 = true
	movff	TMP_REG_1, FSR2L 	; restore stack top
	movff	TMP_REG_2, FSR2H
	return
caml_string_equal_diff:
	M_CONST	0		 	; return 0 = false
	movff	TMP_REG_1, FSR2L 	; restore stack top
	movff	TMP_REG_2, FSR2H
	return
#endif

;;;

#ifdef caml_useprim_caml_int_compare
caml_int_compare:
	;; ACCU = v1
	;; [0x2]:[0x1] = v2
	movlw	0x80		; ACCUH = v1H + 0x80
	addwf	ACCUH, F
	addwf	[0x2], W	; W <- v2H + 0x80
	subwf	ACCUH, W	; (v1H + 0x80) - (v2H + 0x80)
	bnz	caml_int_compare_L0
	movf	[0x1], W
	subwf	ACCUL, W	; v1L - v2L
	bz	caml_int_compare_1eq2
caml_int_compare_L0:
	bnc	caml_int_compare_1lt2
	M_CONST	1		; v1 > v2 => return 1
	return
caml_int_compare_1lt2:
	setf	ACCUH		; v1 < v2 => return -1
	setf	ACCUL
	return
caml_int_compare_1eq2:
	M_CONST 0		; v1 = v2 => return 0
	return
#endif

;;;

#ifdef caml_useprim_caml_float_array_compare
#ifndef caml_useprim_caml_float_compare_1xx2
#define caml_useprim_caml_float_compare_1xx2
#endif
caml_float_array_compare:
	btfsc	ACCUL, 0
	bra	caml_float_array_compare_0_x
	btfsc	[0x1], 0
	bra	caml_float_compare_1gt2
	movff	FSR1L, TMP_REG_1 	; save FSR1
	movff	FSR1H, TMP_REG_2
	movff	ACCUL, FSR0L		; FSR0 <- @f2[]
	movff	ACCUH, FSR0H
	movsf	[0x1], FSR1L		; FSR1 <- @f1[]
	movsf	[0x2], FSR1H
	subfsr	FSR0, 0x1
	subfsr	FSR1, 0x1
	movf	INDF0, W
	subwf	POSTINC1, W		; size1 - size2
	bnc	caml_float_compare_1gt2
	bnz	caml_float_compare_1lt2
	movf	POSTINC0, W		; size = 0 ?
	bz	caml_float_compare_1eq2
	movwf	TMP_REG_4		; TMP_REG_4 <- size
	addfsr	FSR0, 0x1
	addfsr	FSR1, 0x1
caml_float_array_compare_loop:
	btfsc	INDF0, 7		; test sign
	bra	caml_float_array_compare_XN
	btfss	INDF1, 7
	bra	caml_float_array_compare_PP
	bra	caml_float_compare_1lt2
caml_float_array_compare_XN:
	btfss	INDF1, 7
	bra	caml_float_compare_1gt2
caml_float_array_compare_NN:
	movf	FSR1L, W		; FSR1 <-> FSR0
	movff	FSR0L, FSR1L
	movwf	FSR0L
	movf	FSR1H, W
	movff	FSR0H, FSR1H
	movwf	FSR0H
caml_float_array_compare_PP:
	movf	INDF1, W		; TMP_REG_3 <- e1 + 0x80
	andlw	B'00011111'
	btfsc	WREG, 4
	iorlw	B'11100000'
	addlw	0x80
	movwf	TMP_REG_3
	movf	INDF0, W		; W <- e2 + 0x80
	andlw	B'00011111'
	btfsc	WREG, 4
	iorlw	B'11100000'
	addlw	0x80
	subwf	TMP_REG_3, W		; (e1 + 0x80) - (e2 + 0x80)
	bnc	caml_float_compare_1gt2
	bnz	caml_float_compare_1lt2
	swapf	POSTDEC1, W		; TMP_REG_3 <- m1H
	rrncf	WREG, F
	andlw	B'00000011'
	movwf	TMP_REG_3
	swapf	POSTDEC0, W		; W <- m2H
	rrncf	WREG, F
	andlw	B'00000011'
	subwf	TMP_REG_3, W		; m1H - m2H
	bnc	caml_float_compare_1gt2
	bnz	caml_float_compare_1lt2
	movf	POSTINC0, W
	subwf	POSTINC1, W		; m1L - m2L
	bnc	caml_float_compare_1gt2
	bnz	caml_float_compare_1lt2
	dcfsnz	TMP_REG_4, F		; loop size times
	bra	caml_float_compare_1eq2
	btfss	INDF1, 7		; NN ?
	bra	caml_float_array_compare_continue
	movf	FSR1L, W		; FSR1 <-> FSR0
	movff	FSR0L, FSR1L
	movwf	FSR0L
	movf	FSR1H, W
	movff	FSR0H, FSR1H
	movwf	FSR0H
caml_float_array_compare_continue:
	addfsr	FSR0, 0x2
	addfsr	FSR1, 0x2
	bra	caml_float_array_compare_loop
caml_float_array_compare_0_x:
	btfsc	[0x1], 0
	bra	caml_float_compare_1eq2
	bra	caml_float_compare_1lt2
#endif

#ifdef caml_useprim_caml_float_compare
#ifndef caml_useprim_caml_float_compare_1xx2
#define caml_useprim_caml_float_compare_1xx2
#endif
#endif
#ifdef caml_useprim_caml_float_compare_1xx2
caml_float_compare_1eq2:
	movff	TMP_REG_1, FSR1L 	; restore FSR1
	movff	TMP_REG_2, FSR1H
	M_CONST	0			; return 0
	return
caml_float_compare_1lt2:
	movff	TMP_REG_1, FSR1L 	; restore FSR1
	movff	TMP_REG_2, FSR1H
	setf	ACCUL
	setf	ACCUH
	return
caml_float_compare_1gt2:
	movff	TMP_REG_1, FSR1L 	; restore FSR1
	movff	TMP_REG_2, FSR1H
	M_CONST	1		 	; return 1
	return
#endif

#ifdef caml_useprim_caml_float_compare
#ifndef caml_useprim_caml_float_compare_1xx2
#define caml_useprim_caml_float_compare_1xx2
#endif
caml_float_compare:
	movff	FSR1L, TMP_REG_1 	; save FSR1
	movff	FSR1H, TMP_REG_2
	movff	ACCUL, FSR1L		; FSR0 <- @f2
	movff	ACCUH, FSR1H
	movsf	[0x1], FSR0L		; FSR1 <- @f1
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x2
	addfsr	FSR1, 0x2
	btfsc	INDF0, 7		; test sign
	bra	caml_float_compare_XN
	btfss	INDF1, 7
	bra	caml_float_compare_PP
	bra	caml_float_compare_1lt2
caml_float_compare_XN:
	btfss	INDF1, 7
	bra	caml_float_compare_1gt2
caml_float_compare_NN:
	movf	FSR1L, W		; FSR1 <-> FSR0
	movff	FSR0L, FSR1L
	movwf	FSR0L
	movf	FSR1H, W
	movff	FSR0H, FSR1H
	movwf	FSR0H
caml_float_compare_PP:
	movf	PREINC1, W		; TMP_REG_3 <- e1 + 0x80
        addlw   0x2
	movwf	TMP_REG_3
	movf	PREINC0, W		; W <- e2 + 0x80
	addlw	0x2
	subwf	TMP_REG_3, W		; (e1 + 0x80) - (e2 + 0x80)
	bnc	caml_float_compare_1lt2
	bnz	caml_float_compare_1gt2
        subfsr  FSR0, 0x1
        subfsr  FSR1, 0x1
	movf	POSTDEC1, W		; TMP_REG_3 <- m1H
	andlw	B'01111111'
	movwf	TMP_REG_3
	movf	POSTDEC0, W		; W <- m2U
	andlw	B'01111111'
	subwf	TMP_REG_3, W		; m1U - m2U
	bnc	caml_float_compare_1lt2
	bnz	caml_float_compare_1gt2
	movf	POSTDEC0, W
	subwf	POSTDEC1, W		; m1H - m2H
	bnc	caml_float_compare_1lt2
	bnz	caml_float_compare_1gt2
	movf	INDF0, W
	subwf	INDF1, W		; m1L - m2L
	bnc	caml_float_compare_1lt2
	bnz	caml_float_compare_1gt2
	bra	caml_float_compare_1eq2
#endif

;;;

#ifdef caml_useprim_caml_custom_compare
caml_custom_compare:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	movlw	0x1
	movwf	PRODL			; PRODL <- compare custom index (1)
	movf	POSTINC0, W
	movff	INDF0, PCLATH		; WARNING: write PCLATH
	movwf	PCL			; goto custom indirection table
#endif

#ifdef caml_useprim_caml_int32_compare
caml_int32_compare:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	INDF0, TMP_REG_4
	movsf	[0x1], FSR0L
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x5
	movlw	0x80
	addwf	TMP_REG_4, F
	addwf	POSTDEC0, W
	subwf	TMP_REG_4, W
	bnz	caml_int32_compare_1neq2
	movf	POSTDEC0, W
	subwf	TMP_REG_3, W
	bnz	caml_int32_compare_1neq2
	movf	POSTDEC0, W
	subwf	TMP_REG_2, W
	bnz	caml_int32_compare_1neq2
	movf	INDF0, W
	subwf	TMP_REG_1, W
	bnz	caml_int32_compare_1neq2
	M_CONST	0
	return
caml_int32_compare_1neq2:
	bnc	caml_int32_compare_1lt2
	M_CONST	1
	return
caml_int32_compare_1lt2:
	setf	ACCUH
	setf	ACCUL
	return
#endif

#ifdef caml_useprim_caml_int64_compare
caml_int64_compare:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	addfsr	FSR0, 0x2
	movff	POSTINC0, TMP_REG_1
	movff	POSTINC0, TMP_REG_2
	movff	POSTINC0, TMP_REG_3
	movff	POSTINC0, TMP_REG_4
	movff	POSTINC0, ACCUL
	movff	POSTINC0, ACCUH
	movff	POSTINC0, PRODL
	movff	INDF0, PRODH
	movsf	[0x1], FSR0L
	movsf	[0x2], FSR0H
	addfsr	FSR0, 0x9
	movlw	0x80
	addwf	PRODH, F
	addwf	POSTDEC0, W
	subwf	PRODH, W
	bnz	caml_int64_compare_1neq2
	movf	POSTDEC0, W
	subwf	PRODL, W
	bnz	caml_int64_compare_1neq2
	movf	POSTDEC0, W
	subwf	ACCUH, W
	bnz	caml_int64_compare_1neq2
	movf	POSTDEC0, W
	subwf	ACCUL, W
	bnz	caml_int64_compare_1neq2
	movf	POSTDEC0, W
	subwf	TMP_REG_4, W
	bnz	caml_int64_compare_1neq2
	movf	POSTDEC0, W
	subwf	TMP_REG_3, W
	bnz	caml_int64_compare_1neq2
	movf	POSTDEC0, W
	subwf	TMP_REG_2, W
	bnz	caml_int64_compare_1neq2
	movf	INDF0, W
	subwf	TMP_REG_1, W
	bnz	caml_int64_compare_1neq2
	M_CONST	0
	return
caml_int64_compare_1neq2:
	bnc	caml_int64_compare_1lt2
	M_CONST	1
	return
caml_int64_compare_1lt2:
	setf	ACCUH
	setf	ACCUL
	return
#endif

;;;
	
#ifdef caml_useprim_caml_compare_to_eq
caml_compare_to_eq:
	btg	ACCUL, 1		; 00 -> 1
	movlw	0x03			; 01 -> 0
	andwf	ACCUL, F		; 11 -> 0
	clrf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_compare_to_neq
caml_compare_to_neq:
	movlw	0x03			; 00 -> 0
	andwf	ACCUL, F		; 01 -> 1
	clrf	ACCUH			; 11 -> 1
	return
#endif

#ifdef caml_useprim_caml_compare_to_lt
caml_compare_to_lt:
	rrncf	ACCUL, F		; 00 -> 0
	bsf	ACCUL, 0		; 01 -> 0
	movlw	0x03			; 11 -> 1
	andwf	ACCUL, F
	clrf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_compare_to_le
caml_compare_to_le:
	btg	ACCUL, 1		; 00 -> 1
	btfsc	ACCUL, 2		; 01 -> 0
	bsf	ACCUL, 1		; 11 -> 1
	movlw	0x03
	andwf	ACCUL, F
	clrf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_compare_to_gt
caml_compare_to_gt:
	btfsc	ACCUL, 2		; 00 -> 0
	bcf	ACCUL, 1		; 01 -> 1
	movlw	0x03			; 11 -> 0
	andwf	ACCUL, F
	clrf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_compare_to_ge
caml_compare_to_ge:
	rrncf	ACCUL, F		; 00 -> 1
	btg	ACCUL, 1		; 01 -> 1
	bsf	ACCUL, 0		; 11 -> 0
	movlw	0x03
	andwf	ACCUL, F
	clrf	ACCUH
	return
#endif

#ifdef caml_useprim_caml_raise_ia_compare
#ifndef caml_useprim_caml_raise_ia
#define caml_useprim_caml_raise_ia
#endif
caml_raise_ia_compare:
	movlw	0x1			; erase ACCU and stack[0]
	movwf	ACCUL
	movwf	[0x1]
	clrf	ACCUH
	clrf	[0x2]
	M_CHECK_UNFULL_HEAP 0x7		; check allocation of string and exn
	M_WRITE_BYTE STRING_TAG		; write string tag
	M_WRITE_BYTE 0x4		; write string size
	movff	FSR1L, TMP_REG_1	; mem string addr
	movff	FSR1H, TMP_REG_2
	M_WRITE_BYTE 'c'		; write string content
	M_WRITE_BYTE 'o'
	M_WRITE_BYTE 'm'
	M_WRITE_BYTE 'p'
	M_WRITE_BYTE 'a'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 0x0		; write string \0
	goto	caml_raise_ia
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;          INTERRUPTIONS            ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_set_interruption_handler

caml_set_interruption_handler:
	movff	ACCUL, INT_FUN_L
	movff	ACCUH, INT_FUN_H
	return

asm_interrupt_handler:
	movf	INTCON, W		; INT_FLAGS_3:2:1 |= *IF
	andlw	B'00000111'
	iorwf	INT_FLAGS_3, F
	movf	INTCON, W
	rlcf	WREG, W
	swapf	WREG, W
	iorlw	B'11111000'
	andwf	INT_FLAGS_3, F
	swapf	INTCON3, W
	andlw	B'00110000'
	iorwf	INT_FLAGS_3, F
	movf	INTCON3, W
	rlcf	WREG, W
	iorlw	B'11001111'
	andwf	INT_FLAGS_3, F
	movf	PIR1, W
	iorwf	INT_FLAGS_1, F
	movf	PIE1, W
	andwf	INT_FLAGS_1, F
	movf	PIR2, W
	iorwf	INT_FLAGS_2, F
	movf	PIE2, W
	andwf	INT_FLAGS_2, F
	clrf	PIR1			; *IF <- 0
	clrf	PIR2
	movlw	B'11111000'
	andwf	INTCON, F
	movlw	B'11111100'
	andwf	INTCON3, F
	bsf	INT_FLAGS_3, 7		; set interrupt_flag
	retfie	FAST

caml_interrupt_handler:
	btfss	INT_FUN_L, 0		; handler defined ?
	bra	caml_interrupt_handler_l0
	clrf	INT_FLAGS_1		; no -> clear flags
	clrf	INT_FLAGS_2
	clrf	INT_FLAGS_3
	goto	_THE_BIG_LOOP
caml_interrupt_handler_l0:		; yes
	clrf	TMP_REG_1
	clrf	TMP_REG_2
	clrf	TMP_REG_3
	comf	INT_RUNS_1, W
	andwf	INT_FLAGS_1, W
	bnz	caml_interrupt_handler_1
	comf	INT_RUNS_2, W                ;; TMP_REG_1 = argL
	andwf	INT_FLAGS_2, W               ;; TMP_REG_2 = argH
	bnz	caml_interrupt_handler_2     ;; TMP_REG_3 = running_flag
	comf	INT_RUNS_3, W
	andwf	INT_FLAGS_3, W
	andlw	B'00110111'
	bnz	caml_interrupt_handler_3
	bcf	INT_FLAGS_3, 7		; clear interrupt_flag
	goto	_THE_BIG_LOOP		; nothing to do
caml_interrupt_handler_1:		; PIR1 interruption
	bsf	TMP_REG_3, 5
	rcall	caml_interrupt_handler_compute_mask
	iorwf	INT_RUNS_1, F
	xorwf	INT_FLAGS_1, F
	movlw	(((2 * PIR1) & 0xFF) + 1)
	movwf	TMP_REG_1
	bra	caml_interrupt_handler_run
caml_interrupt_handler_2:		; PIR2 interruption
	bsf	TMP_REG_3, 6
	rcall	caml_interrupt_handler_compute_mask
	iorwf	INT_RUNS_2, F
	xorwf	INT_FLAGS_2, F
	movlw	(((2 * PIR2) & 0xFF) + 1)
	movwf	TMP_REG_1
	bra	caml_interrupt_handler_run
caml_interrupt_handler_3:
	bsf	TMP_REG_3, 7
	rcall	caml_interrupt_handler_compute_mask
	iorwf	INT_RUNS_3, F
	xorwf	INT_FLAGS_3, F
	movf	TMP_REG_2, W
	andlw	0x0F
	bz	caml_interrupt_handler_3_INTCON3
	movlw	(((2 * INTCON) & 0xFF) + 1)
	movwf	TMP_REG_1
	bra	caml_interrupt_handler_run
caml_interrupt_handler_3_INTCON3:
	movlw	(((2 * INTCON3) & 0xFF) + 1)
	movwf	TMP_REG_1
	swapf	TMP_REG_2, F
caml_interrupt_handler_run:
	movlw	STACK_END 		; check stack overflow (8 levels)
	cpfseq	FSR2H
	bra	caml_interrupt_handler_l1
	movlw	-0x16
	addwf	FSR2L, W
	btfss	STATUS, C
	goto	caml_raise_stack_overflow
caml_interrupt_handler_l1:
	bcf	INT_FLAGS_3, 7		; clear interrupt_flag
	movff	ACCUH, POSTDEC2		; push accu
	movff	ACCUL, POSTDEC2
	movff	TBLPTRH, POSTDEC2 	; push PC
	movff	TBLPTRL, POSTDEC2
	movff	TRAP_SPH, POSTDEC2 	; push trapSp
	movff	TRAP_SPL, POSTDEC2
	movlw	high (caml_interrupt_handler_raise_instr) ; push @raise_instr
	movwf	POSTDEC2
	movlw	low (caml_interrupt_handler_raise_instr)
	movwf	POSTDEC2
	movff	FSR2H, TRAP_SPH		; trapSp <- Sp
	movff	FSR2L, TRAP_SPL
	movff	TMP_REG_3, POSTDEC2  	; (1)push running_flag
	movff	EXTRA_ARGS, POSTDEC2	; (1)push extra_args
	movff	ENVH, POSTDEC2		; push env
	movff	ENVL, POSTDEC2
	movlw	high (caml_interrupt_handler_return_instr) ; push @return_instr
	movwf	POSTDEC2
	movlw	low (caml_interrupt_handler_return_instr)
	movwf	POSTDEC2
	movff	TMP_REG_2, POSTDEC2	; push argument
	movff	TMP_REG_1, POSTDEC2
	movf	INT_FUN_L, W		; ACCU, ENV, FSR0 <- interrupt_handler
	movwf	ACCUL
	movwf	ENVL
	movwf	FSR0L
	movf	INT_FUN_H, W
	movwf	ACCUH
	movwf	ENVH
	movwf	FSR0H
	movff	POSTINC0, TBLPTRL 	; PC <- interrupt_handler[0]
	movff	INDF0, TBLPTRH
	movlw	0x1			; extraArgs <- int_val(0)
	movwf	EXTRA_ARGS
	goto	_THE_BIG_LOOP
caml_interrupt_handler_compute_mask:
	movwf	PRODL
	movlw	0x00
	bsf	STATUS, C
caml_interrupt_handler_compute_mask_loop:
	rlcf	WREG, W
	rrcf	PRODL, F
	bnc	caml_interrupt_handler_compute_mask_loop
	movwf	TMP_REG_2
	andlw	0x0F
	bnz	caml_interrupt_handler_compute_mask_l
	bsf	TMP_REG_3, 4
	swapf	TMP_REG_2, W
	iorwf	TMP_REG_3, F
	movf	TMP_REG_2, W
	return
caml_interrupt_handler_compute_mask_l:
	iorwf	TMP_REG_3, F
	return

caml_interrupt_handler_return_instr:
	db	.146
	
caml_interrupt_handler_raise_instr:
	db	.147

caml_interrupt_handler_return:
	bsf	INT_FLAGS_3, 7		; set interrupt_flag
	rcall	caml_interrupt_restore_runs
	addfsr	FSR2, 0x4		; pop [ @raise_instr ; trapSp ]
	movff	PREINC2, TBLPTRL	; PC <- pop()
	movff	PREINC2, TBLPTRH
	movff	PREINC2, ACCUL		; ACCU <- pop()
	movff	PREINC2, ACCUH
	return

caml_interrupt_handler_raise:
	bsf	INT_FLAGS_3, 7		; set interrupt_flag
	subfsr	FSR2, 0x8		; FSR2 <- @running_flag
	rcall	caml_interrupt_restore_runs
	addfsr	FSR2, 0x8		; pop [ @ri ; tSp ; PC ; ACCU ]
	goto	_RAISE			; raise

caml_interrupt_restore_runs:
	movf	[0x0], W		; W <- running_flag
	movwf	PRODL			; PRODL <- running_flag
	andlw	0x0F			; compute mask
	btfsc	PRODL, 4
	swapf	WREG, W
	btfsc	PRODL, 5		; clear run bit
	xorwf	INT_RUNS_1, F
	btfsc	PRODL, 6
	xorwf	INT_RUNS_2, F
	btfsc	PRODL, 7
	xorwf	INT_RUNS_3, F
	return

#else

asm_interrupt_handler:
	retfie	FAST

caml_interrupt_handler:
	goto	_THE_BIG_LOOP

#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               SLEEP               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_sleep_millis
caml_sleep_millis:
	rlcf	ACCUH, W	; ACCU < 0 ?
	bc	caml_sleep_millis_end
	rrcf	ACCUH, F	; ACCU <- val_int(ACCU)
	rrcf	ACCUL, F
	movf	ACCUL, W	; ACCU = 0 ?
	iorwf	ACCUH, W
	bz	caml_sleep_millis_end
	incf	ACCUH, F
	movlw	.234
	movwf	TMP_REG_1
	movlw	.13
	movwf	TMP_REG_2
caml_sleep_millis_loop:
	decfsz	TMP_REG_1, F
	bra	caml_sleep_millis_loop
	decfsz	TMP_REG_2, F
	bra	caml_sleep_millis_loop
	movlw	.250
	movwf	TMP_REG_1
	movlw	.13
	movwf	TMP_REG_2
	decfsz	ACCUL, F
	incf	ACCUH, F
	decfsz	ACCUH, F
	bra	caml_sleep_millis_loop
caml_sleep_millis_end:
	return
#endif
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;               TOOL                ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_raise_ia
caml_raise_ia:
	M_WRITE_BYTE 0x0		; write exn tag
	M_WRITE_BYTE 0x2		; write exn size
	movff	FSR1L, ACCUL		; ACCU <- exn
	movff	FSR1H, ACCUH
	movlw	(0x2 * INVALID_ARG_IND + 0x1) ; write INVALID_ARG_IND
	movwf	POSTINC1
	clrf	POSTINC1
	movff	TMP_REG_1, POSTINC1 	; write string addr
	movff	TMP_REG_2, POSTINC1
	goto	caml_extern_raise
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;           ALPHA SERIAL            ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#define SERIAL_TICTAC	IPR1,RCIP

SERIAL_HASH_INITL	EQU	0x6B
SERIAL_HASH_INITH	EQU	0x39
SERIAL_FLAG_BASE	EQU	0xA6
SERIAL_NEWTIC_FLAG	EQU	(SERIAL_FLAG_BASE ^ B'00000000')
SERIAL_NEWTAC_FLAG	EQU	(SERIAL_FLAG_BASE ^ B'11111111')
SERIAL_ENDTIC_FLAG	EQU	(SERIAL_FLAG_BASE ^ B'00001111')
SERIAL_ENDTAC_FLAG	EQU	(SERIAL_FLAG_BASE ^ B'11110000')
SERIAL_RECEIVE_FLAG	EQU	(SERIAL_FLAG_BASE ^ B'00111100')
SERIAL_ERROR_FLAG	EQU	(SERIAL_FLAG_BASE ^ B'11000011')
SERIAL_GET_FLAG		EQU	(SERIAL_FLAG_BASE ^ B'00110011')
SERIAL_OOM_FLAG		EQU	(SERIAL_FLAG_BASE ^ B'11001100')
SERIAL_ESC_FLAG		EQU	0xFF
SERIAL_RES1_FLAG	EQU	0x03
SERIAL_RES2_FLAG	EQU	0x1A
SERIAL_RES3_FLAG	EQU	0x1C

#ifdef caml_useprim_caml_serial_open_channel
caml_serial_open_channel:
	movlw	B'00100100'
	movwf	TXSTA
	movlw	B'10010000'
	movwf	RCSTA
	bcf	SERIAL_TICTAC
	rrcf	ACCUH, W	; STATUS.C ignored
	rrcf	ACCUL, W
	movwf	SPBRG
	btfsc	ACCUH, 1
	bsf	TXSTA, BRGH
	return
#endif
	
#ifdef caml_useprim_caml_serial_receive
#ifndef caml_useprim_caml_serial_receive_byte
#define caml_useprim_caml_serial_receive_byte
#endif
#ifndef caml_useprim_caml_serial_send_byte
#define caml_useprim_caml_serial_send_byte
#endif
#ifndef caml_useprim_caml_serial_receive_flag
#define caml_useprim_caml_serial_receive_flag
#endif
#ifndef caml_useprim_caml_serial_send_flag
#define caml_useprim_caml_serial_send_flag
#endif
#ifndef caml_useprim_caml_compute_hash_init
#define caml_useprim_caml_compute_hash_init
#endif
#ifndef caml_useprim_caml_compute_hash_step
#define caml_useprim_caml_compute_hash_step
#endif
caml_serial_receive:
	rcall	caml_serial_receive_flag 	; receive flag
	xorlw	SERIAL_NEWTIC_FLAG		; NEWTIC ?
	bnz	caml_serial_receive_notnewtic
	btfsc	SERIAL_TICTAC			; TIC ?
	bra	caml_serial_receive_nottic
	movlw	SERIAL_RECEIVE_FLAG 		; send RECEIVE
	rcall	caml_serial_send_flag
	bra	caml_serial_receive_start
caml_serial_receive_nottic:
	movlw	SERIAL_ENDTIC_FLAG 		; send ENDTIC
	rcall	caml_serial_send_flag
	bra	caml_serial_receive
caml_serial_receive_notnewtic:
	xorlw	(SERIAL_NEWTIC_FLAG ^ SERIAL_NEWTAC_FLAG) ; NEWTAC ?
	bnz	caml_serial_receive_notnewtac
	btfss	SERIAL_TICTAC			; TAC ?
	bra	caml_serial_receive_nottac
	movlw	SERIAL_RECEIVE_FLAG 		; send RECEIVE
	rcall	caml_serial_send_flag
	bra	caml_serial_receive_start
caml_serial_receive_nottac:
	movlw	SERIAL_ENDTAC_FLAG
	rcall	caml_serial_send_flag
	bra	caml_serial_receive
caml_serial_receive_notnewtac:
	xorlw	(SERIAL_NEWTAC_FLAG ^ SERIAL_ENDTIC_FLAG) ; ENDTIC ?
	bz	caml_serial_receive_nottic
	xorlw	(SERIAL_ENDTIC_FLAG ^ SERIAL_ENDTAC_FLAG) ; ENDTAC ?
	bz	caml_serial_receive_nottac
	xorlw	(SERIAL_ENDTAC_FLAG ^ SERIAL_RECEIVE_FLAG) ; RECEIVE ?
	bnz	caml_serial_receive
	movlw	SERIAL_RECEIVE_FLAG 		; send RECEIVE
	rcall	caml_serial_send_flag
	bra	caml_serial_receive

caml_serial_receive_start:
	rcall	caml_compute_hash_init
	rcall	caml_serial_receive_byte 	; receive sizeL
	movwf	ACCUL			 	; ACCUL <- sizeL or accuL
	rcall	caml_compute_hash_step
	rcall	caml_serial_receive_byte 	; receive sizeH
	movwf	ACCUH			 	; ACCUH <- sizeH or accuH
	rcall	caml_compute_hash_step
	rcall	caml_serial_receive_byte 	; receive HASH
	movwf	TMP_REG_6
	rcall	caml_serial_receive_byte
	cpfseq	TMP_REG_3		 	; check HASH
	bra	caml_serial_receive
	movf	TMP_REG_6, W
	cpfseq	TMP_REG_2
	bra	caml_serial_receive
	btfsc	ACCUL, 0
	bra	caml_serial_receive_end
	movlw	low ATOM0_ADR
	cpfseq	ACCUL
	bra	caml_serial_receive_check_oom
	movlw	high ATOM0_ADR
	xorwf	ACCUH, W
	bz	caml_serial_receive_end

caml_serial_receive_check_oom:
	movf	ACCUL, W
	movwf	TMP_REG_A			; TMP_REG_A <- sizeL
	addwf	FSR1L, W			; FSR1L + sizeL
	movf	ACCUH, W
	movwf	TMP_REG_B			; TMP_REG_B <- sizeH
	addwfc	FSR1H, W			; FSR1H + sizeH + carry
	cpfslt	CUR_HEAP_END			; check heap overflow
	bra	caml_serial_receive_heap 	; ok
	movlw	0x1				; ACCUL <- ()
	movwf	ACCUL
	clrf	ACCUH
	call	caml_gc_exec			; run GC
	movf	TMP_REG_A, W			; check heap overflow
	addwf	FSR1L, W
	movf	TMP_REG_B, W
	addwfc	FSR1H, W
	cpfslt	CUR_HEAP_END
	bra	caml_serial_receive_heap	; ok
	bra	caml_serial_receive_oom 	; overflow

caml_serial_receive_heap:
	movlw	SERIAL_GET_FLAG
	rcall	caml_serial_send_flag
	rcall	caml_compute_hash_init
	movf	FSR1L, W 	 		; send FSR1
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	movf	FSR1H, W
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	movf	TMP_REG_2, W			; send HASH
	rcall	caml_serial_send_byte
	movf	TMP_REG_3, W
	rcall	caml_serial_send_byte
	
caml_serial_receive_data:
	bcf	STATUS, C			; TMP_REG_5:4 <- SIZE / 2
	rrcf	TMP_REG_B, W
	movwf	TMP_REG_5
	rrcf	TMP_REG_A, W
	movwf	TMP_REG_4
	incf	TMP_REG_5, F			; TMP_REG_5 ++
	addfsr	FSR1, 0x2			; ACCU <- @ first block
	movff	FSR1L, ACCUL
	movff	FSR1H, ACCUH
	subfsr	FSR1, 0x2
	rcall	caml_compute_hash_init

caml_serial_receive_data_loop:
	rcall	caml_serial_receive_byte	; fill heap
	movwf	POSTINC1
	rcall	caml_compute_hash_step
	rcall	caml_serial_receive_byte
	movwf	POSTINC1
	rcall	caml_compute_hash_step
	dcfsnz	TMP_REG_4, F
	decfsz	TMP_REG_5, F
	bra	caml_serial_receive_data_loop	; loop
	rcall	caml_serial_receive_byte	; receive HASH
	movwf	TMP_REG_6
	rcall	caml_serial_receive_byte
	cpfseq	TMP_REG_3			; check HASH
	bra	caml_serial_receive_data_error
	movf	TMP_REG_6, W
	cpfseq	TMP_REG_2
	bra	caml_serial_receive_data_error

caml_serial_receive_end:
	movlw	SERIAL_ENDTIC_FLAG
	btfsc	SERIAL_TICTAC
	movlw	SERIAL_ENDTAC_FLAG
	btg	SERIAL_TICTAC
	bra	caml_serial_send_flag

caml_serial_receive_oom:
	movlw	SERIAL_OOM_FLAG 	    	; send SERIAL_OOM_FLAG
	rcall	caml_serial_send_flag
	bra	caml_serial_receive 		; restart

caml_serial_receive_data_error:
	movf	TMP_REG_A, W			; restore FSR1
	subwf	FSR1L, F
	movf	TMP_REG_B, W
	subwfb	FSR1H, F
	bra	caml_serial_receive
#endif

#ifdef caml_useprim_caml_serial_send
#ifndef caml_useprim_caml_serial_receive_byte
#define caml_useprim_caml_serial_receive_byte
#endif
#ifndef caml_useprim_caml_serial_send_byte
#define caml_useprim_caml_serial_send_byte
#endif
#ifndef caml_useprim_caml_serial_receive_flag
#define caml_useprim_caml_serial_receive_flag
#endif
#ifndef caml_useprim_caml_serial_send_flag
#define caml_useprim_caml_serial_send_flag
#endif
#ifndef caml_useprim_caml_compute_hash_init
#define caml_useprim_caml_compute_hash_init
#endif
#ifndef caml_useprim_caml_compute_hash_step
#define caml_useprim_caml_compute_hash_step
#endif

caml_serial_send:
	movlw	SERIAL_NEWTIC_FLAG 		; send NEW_FLAG
	btfsc	SERIAL_TICTAC
	movlw	SERIAL_NEWTAC_FLAG
	rcall	caml_serial_send_flag
	rcall	caml_serial_receive_flag	; receive
	xorlw	SERIAL_RECEIVE_FLAG		; RECEIVE_FLAG ?
	bnz	caml_serial_send_notrec
	rcall	caml_compute_hash_init
	movf	[0x1], W			; send stack top
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	movf	[0x2], W
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	movf	TMP_REG_2, W			; send HASH
	rcall	caml_serial_send_byte
	movf	TMP_REG_3, W
	rcall	caml_serial_send_byte
	bra	caml_serial_send_continue	; -> continue
caml_serial_send_notrec:
	xorlw	(SERIAL_RECEIVE_FLAG ^ SERIAL_ENDTIC_FLAG) ; ENDTIC_FLAG ?
	bnz	caml_serial_send_nottic
	btfsc	SERIAL_TICTAC			; TICTAC = TIC ?
	bra	caml_serial_send
	movlw	SERIAL_ENDTIC_FLAG		; send ENDTIC_FLAG
	rcall	caml_serial_send_flag
	bra	caml_serial_send_end		; -> end
caml_serial_send_nottic:
	xorlw	(SERIAL_ENDTIC_FLAG ^ SERIAL_ENDTAC_FLAG) ; ENDTAC_FLAG ?
	bnz	caml_serial_send
	btfss	SERIAL_TICTAC 			; TICTAC = TAC ?
	bra	caml_serial_send
	movlw	SERIAL_ENDTAC_FLAG		; send ENDTAC_FLAG
	rcall	caml_serial_send_flag
	bra	caml_serial_send_end		; -> end

caml_serial_send_block:
	rcall	caml_compute_hash_init
	rcall	caml_serial_receive_byte	; receive adr
	movwf	FSR0L
	rcall	caml_compute_hash_step
	rcall	caml_serial_receive_byte
	movwf	FSR0H
	rcall	caml_compute_hash_step
	rcall	caml_serial_receive_byte	; receive HASH
	movwf	TMP_REG_6
	rcall	caml_serial_receive_byte
	cpfseq	TMP_REG_3			; check HASH
	bra	caml_serial_send
	movf	TMP_REG_6, W
	cpfseq	TMP_REG_2
	bra	caml_serial_send

caml_serial_send_block_tagsize:
	rcall	caml_compute_hash_init
	subfsr	FSR0, 0x2
	movf	POSTINC0, W			; send tag
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	movf	POSTINC0, W			; send size
	movwf	TMP_REG_A			; TMP_REG_A <- size
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	movf	TMP_REG_2, W			; send HASH
	rcall	caml_serial_send_byte
	movf	TMP_REG_3, W
	rcall	caml_serial_send_byte

caml_serial_send_block_data_start:
	rcall	caml_compute_hash_init

caml_serial_send_block_data_loop:
	movf	POSTINC0, W			; send data
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	movf	POSTINC0, W
	rcall	caml_serial_send_byte
	rcall	caml_compute_hash_step
	decfsz	TMP_REG_A, F
	bra	caml_serial_send_block_data_loop
	movf	TMP_REG_2, W			; send HASH
	rcall	caml_serial_send_byte
	movf	TMP_REG_3, W
	rcall	caml_serial_send_byte

caml_serial_send_continue:
	rcall	caml_serial_receive_flag	; receive flag
	xorlw	SERIAL_GET_FLAG			; GET_FLAG ?
	bz	caml_serial_send_block
caml_serial_send_continue_notget:
	xorlw	(SERIAL_GET_FLAG ^ SERIAL_ENDTIC_FLAG)	; ENDTIC_FLAG ?
	bnz	caml_serial_send_continue_nottic
	btfsc	SERIAL_TICTAC			; TICTAC = TIC ?
	bra	caml_serial_send
	movlw	SERIAL_ENDTIC_FLAG
	rcall	caml_serial_send_flag
	bra	caml_serial_send_end
caml_serial_send_continue_nottic:
	xorlw	(SERIAL_ENDTIC_FLAG ^ SERIAL_ENDTAC_FLAG) ; ENDTAC_FLAG ?
	bnz	caml_serial_send
	btfss	SERIAL_TICTAC 			; TICTAC = TAC ?
	bra	caml_serial_send
	movlw	SERIAL_ENDTAC_FLAG
	rcall	caml_serial_send_flag

caml_serial_send_end:
	btg	SERIAL_TICTAC			; inverse tictac
	return					; ACCU = () already
#endif
	
;;; flags

#ifdef caml_useprim_caml_serial_receive_flag
#ifndef caml_useprim_caml_serial_receive_byte
#define caml_useprim_caml_serial_receive_byte
#endif
caml_serial_receive_flag:
	rcall	caml_serial_receive_byte
	movwf	TMP_REG_C
	rcall	caml_serial_receive_byte
	cpfseq	TMP_REG_C
	movlw	SERIAL_ERROR_FLAG
	return
#endif

#ifdef caml_useprim_caml_serial_send_flag
#ifndef caml_useprim_caml_serial_send_byte
#define caml_useprim_caml_serial_send_byte
#endif
caml_serial_send_flag:
	rcall	caml_serial_send_byte
	bra	caml_serial_send_byte
#endif

;;; routines

#ifdef caml_useprim_caml_serial_receive_byte
caml_serial_receive_byte:
	btfss	PIR1, RCIF	; wait buffer full
	bra	caml_serial_receive_byte
	movf	RCREG, W	; W <- data
	bcf	RCSTA, CREN
	bsf	RCSTA, CREN
	xorlw	SERIAL_ESC_FLAG
	bz	caml_serial_receive_byte_l0
	xorlw	SERIAL_ESC_FLAG
	return
caml_serial_receive_byte_l0:
	btfss	PIR1, RCIF
	bra	caml_serial_receive_byte_l0
	movf	RCREG, W
	bcf	RCSTA, CREN
	bsf	RCSTA, CREN
	xorlw	SERIAL_ESC_FLAG
	return
#endif

#ifdef caml_useprim_caml_serial_send_byte
caml_serial_send_byte:		; WARNING: do not overwrite W
	movwf	TMP_REG_E
	xorlw	SERIAL_RES1_FLAG
	bz	caml_serial_send_byte_ESC
	xorlw	(SERIAL_RES1_FLAG ^ SERIAL_RES2_FLAG)
	bz	caml_serial_send_byte_ESC
	xorlw	(SERIAL_RES2_FLAG ^ SERIAL_RES3_FLAG)
	bz	caml_serial_send_byte_ESC
	xorlw	(SERIAL_RES3_FLAG ^ SERIAL_ESC_FLAG)
	bz	caml_serial_send_byte_ESC
	movf	TMP_REG_E, W
caml_serial_send_byte_l0:
	btfss	PIR1, TXIF
	bra	caml_serial_send_byte_l0
	movwf	TXREG		; send DATA (from W)
	return
caml_serial_send_byte_ESC:
	movlw	SERIAL_ESC_FLAG
caml_serial_send_byte_ESC_l0:
	btfss	PIR1, TXIF
	bra	caml_serial_send_byte_ESC_l0
	movwf	TXREG
	xorwf	TMP_REG_E, W
caml_serial_send_byte_ESC_l1:
	btfss	PIR1, TXIF
	bra	caml_serial_send_byte_ESC_l1
	movwf	TXREG
	movf	TMP_REG_E, W
	return
#endif

;;; HASH

#ifdef caml_useprim_caml_compute_hash_init
caml_compute_hash_init:
	movlw	SERIAL_HASH_INITL	 ; TMP_REG_3:2 <- magic
	movwf	TMP_REG_2
	movlw	SERIAL_HASH_INITH
	movwf	TMP_REG_3
	return
#endif

#ifdef caml_useprim_caml_compute_hash_step
caml_compute_hash_step:
	movwf	TMP_REG_1		; TMP_REG_1 <- X
	movf	TMP_REG_2, W		; TMP_REG_2 <- TMP_REG_2 * TMP_REG_3
	mulwf	TMP_REG_3
	movf	PRODL, W
	movwf	TMP_REG_2
	xorwf	TMP_REG_3, F		; TMP_REG_3 <- TMP_REG_2 ^ TMP_REG_3
	movf	TMP_REG_1, W
	xorwf	TMP_REG_2, F		; TMP_REG_2 <- TMP_REG_2 ^ X
	xorwf	TMP_REG_3, F		; TMP_REG_3 <- TMP_REG_3 ^ X
	return
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              EEPROM               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef caml_useprim_caml_eeprom_get_size
caml_eeprom_get_size:
	movlw	0xFF			; search for unimplemented bits of EEADR
	movwf	EEADR
	movwf	EEADRH
	movff	EEADR, ACCUL
	movff	EEADRH, ACCUH
	infsnz	ACCUL, F
	incf	ACCUH, F
	bsf	STATUS, C
	rlcf	ACCUL, F
	rlcf	ACCUH, F
	return
#endif

#ifdef caml_useprim_caml_eeprom_read
caml_eeprom_read:
	bcf	STATUS, C	; EEADRH:EEADR <- Long_val(ACCU)
	rrcf	ACCUH, W
	movwf	EEADRH
	rrcf	ACCUL, W
	movwf	EEADR
	bcf	EECON1, EEPGD	; Point to DATA memory
	bcf	EECON1, CFGS	; Access EEPROM
	bsf	EECON1, RD	; EEPROM Read
	clrf	ACCUH		; ACCU <- Val_long(EEDATA)
	bsf	STATUS, C
	rlcf	EEDATA, W
	movwf	ACCUL
	rlcf	ACCUH, F
	return
#endif

#ifdef caml_useprim_caml_eeprom_write
caml_eeprom_write:
	bcf	STATUS, C	; EEADRH:EEADR <- address
	rrcf	ACCUH, W
	movwf	EEADRH
	rrcf	ACCUL, W
	movwf	EEADR
	rrcf	[0x2], W	; STATUS.C ignored
	rrcf	[0x1], W	; EEDATA <- value
	movwf	EEDATA
	movwf	TMP_REG_2	; TMP_REG_2 <- value
	bcf	EECON1, EEPGD	; Point to DATA memory
	bcf	EECON1, CFGS	; Access EEPROM
	bsf	EECON1, WREN	; Enable writes
	bcf	TMP_REG_1, 0	; TMP_REG_1.0 <- INTCON.GIE
	btfsc	INTCON, GIE
	bsf	TMP_REG_1, 0
	bcf	INTCON, GIE	; Disable Interrupts
	movlw	0x55		; Write init sequence
	movwf	EECON2
	movlw	0xAA
	movwf	EECON2
	bsf	EECON1, WR	; Begin write
caml_eeprom_write_loop:
	btfsc	EECON1, WR	; Wait write complete
	bra	caml_eeprom_write_loop
	btfsc	TMP_REG_1, 0	; Restore INTCON.GIE
	bsf	INTCON, GIE
	btfsc	EECON1, WRERR
	bra	caml_eeprom_write_error
	incf	EEDATA, F
	bcf	EECON1, WREN	; Disable writes
	bsf	EECON1, RD	; Read
	movf	TMP_REG_2, W
	cpfseq	EEDATA, W
	bra	caml_eeprom_write_error
	M_CONST	0		; Return ()
	return
caml_eeprom_write_error:
	M_CHECK_UNFULL_HEAP 0xA		; check allocation of string and exn
	M_WRITE_BYTE STRING_TAG		; write string tag
	M_WRITE_BYTE 0x7		; write string size
	movff	FSR1L, TMP_REG_1	; mem string addr
	movff	FSR1H, TMP_REG_2
	M_WRITE_BYTE 'E'		; write string content
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 'p'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'o'
	M_WRITE_BYTE 'm'
	M_WRITE_BYTE '.'
	M_WRITE_BYTE 'w'
	M_WRITE_BYTE 'r'
	M_WRITE_BYTE 'i'
	M_WRITE_BYTE 't'
	M_WRITE_BYTE 'e'
	M_WRITE_BYTE 0x0		; write string \0\1
	M_WRITE_BYTE 0x1
	M_WRITE_BYTE 0x0		; write exn tag
	M_WRITE_BYTE 0x2		; write exn size
	movff	FSR1L, ACCUL		; ACCU <- exn
	movff	FSR1H, ACCUH
	movlw	(FAILURE_IND * 0x2 + 0x1) ; write FAILURE_IND
	movwf	POSTINC1
	clrf	POSTINC1
	movff	TMP_REG_1, POSTINC1 	; write string addr
	movff	TMP_REG_2, POSTINC1
	goto	caml_extern_raise
#endif

#ifdef caml_useprim_caml_eeprom_refresh
caml_eeprom_refresh:
	clrf	EEADR		; Clear address
	clrf	EEADRH
	bcf	EECON1, CFGS	; Set for memory
	bcf	EECON1, EEPGD	; Set for Data EEPROM
	bcf	TMP_REG_1, 0	; TMP_REG_1.0 <- INTCON.GIE
	btfsc	INTCON, GIE
	bsf	TMP_REG_1, 0
	bcf	INTCON, GIE	; Disable interrupts
	bsf	EECON1, WREN	; Enable writes
caml_eeprom_refresh_loop:
	bsf	EECON1, RD	; Read current address
	movlw	0x55		; Write init sequence
	movwf	EECON2
	movlw	0xAA
	movwf	EECON2
	bsf	EECON1, WR	; Set WR bit to begin write
caml_eeprom_refresh_subloop:
	btfsc	EECON1, WR
	bra	caml_eeprom_refresh_subloop
	incfsz	EEADR, F	; Loop
	bra	caml_eeprom_refresh_loop
	incfsz	EEADRH, F
	bra	caml_eeprom_refresh_loop
	bcf	EECON1, WREN	; Disable writes
	btfsc	TMP_REG_1, 0
	bsf	INTCON, GIE
	return			; ACCU = () already
#endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;              FLOATS               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

M_ALLOC_FLOAT macro
		M_CHECK_UNFULL_HEAP 0x2
		movlw	DOUBLE_TAG		; write tag
		movwf	POSTINC1
		movlw	0x2			; write size
		movwf	POSTINC1
	endm
	
#ifdef caml_useprim_caml_neg_float
caml_neg_float:
	M_ALLOC_FLOAT
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	movff	FSR1L, ACCUL		; ACCU <- @result
	movff	FSR1H, ACCUH
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movf	POSTINC0, W
	xorlw	0x80
	movwf	POSTINC1
	movff	POSTINC0, POSTINC1
	return
#endif

#ifdef caml_useprim_caml_abs_float
caml_abs_float:
	M_ALLOC_FLOAT
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	movff	FSR1L, ACCUL		; ACCU <- @result
	movff	FSR1H, ACCUH
	movff	POSTINC0, POSTINC1
	movff	POSTINC0, POSTINC1
	movf	POSTINC0, W
	andlw	0x7F
	movwf	POSTINC1
	movff	POSTINC0, POSTINC1
	return
#endif

#ifdef caml_useprim_caml_float_of_int
#ifndef caml_useprim_caml_float_operation_finalize
#define caml_useprim_caml_float_operation_finalize
#endif
#ifndef caml_useprim_FLO2432
#define caml_useprim_FLO2432
#endif
caml_float_of_int:
	rlcf	ACCUH, W        ; ignore STATUS.C
	rrcf	ACCUH, W
	movwf	AARGB1
	rrcf	ACCUL, W
	movwf	AARGB2
	clrf	AARGB0
	btfsc	AARGB1, 7
	setf	AARGB0
	M_ALLOC_FLOAT
        movff   FSR1L, ACCUL    ; ACCU <- @result
        movff   FSR1H, ACCUH
	rcall	FLO2432
	bra	caml_float_operation_finalize
#endif

#ifdef caml_useprim_caml_int_of_float
#ifndef caml_useprim_INT3224
#define caml_useprim_INT3224
#endif
caml_int_of_float:
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	movff	POSTINC0, AARGB2
	movff	POSTINC0, AARGB1
	movff	POSTINC0, AARGB0
	movff	INDF0, AEXP
	rcall	INT3224
	bsf	STATUS, C
	rlcf	AARGB2, W
	movwf	ACCUL
	rlcf	AARGB1, W
	movwf	ACCUH
	return
#endif
	
#ifdef caml_useprim_caml_add_float
#ifndef caml_useprim_caml_float_operation_initialize
#define caml_useprim_caml_float_operation_initialize
#endif
#ifndef caml_useprim_FPA32
#define caml_useprim_FPA32
#endif
#ifndef caml_useprim_caml_float_operation_finalize
#define caml_useprim_caml_float_operation_finalize
#endif
caml_add_float:
	rcall	caml_float_operation_initialize
	rcall	FPA32
	bra	caml_float_operation_finalize
#endif

#ifdef caml_useprim_caml_sub_float
#ifndef caml_useprim_caml_float_operation_initialize
#define caml_useprim_caml_float_operation_initialize
#endif
#ifndef caml_useprim_FPS32
#define caml_useprim_FPS32
#endif
#ifndef caml_useprim_caml_float_operation_finalize
#define caml_useprim_caml_float_operation_finalize
#endif
caml_sub_float:
	rcall	caml_float_operation_initialize
	rcall	FPS32
	bra	caml_float_operation_finalize
#endif

#ifdef caml_useprim_caml_mul_float
#ifndef caml_useprim_caml_float_operation_initialize
#define caml_useprim_caml_float_operation_initialize
#endif
#ifndef caml_useprim_FPM32
#define caml_useprim_FPM32
#endif
#ifndef caml_useprim_caml_float_operation_finalize
#define caml_useprim_caml_float_operation_finalize
#endif
caml_mul_float:
	rcall	caml_float_operation_initialize
	rcall	FPM32
	bra	caml_float_operation_finalize
#endif

#ifdef caml_useprim_caml_div_float
#ifndef caml_useprim_caml_float_operation_initialize
#define caml_useprim_caml_float_operation_initialize
#endif
#ifndef caml_useprim_FPD32
#define caml_useprim_FPD32
#endif
#ifndef caml_useprim_caml_float_operation_finalize
#define caml_useprim_caml_float_operation_finalize
#endif
caml_div_float:
	rcall	caml_float_operation_initialize
	rcall	FPD32
	bra	caml_float_operation_finalize
#endif

#ifdef caml_useprim_caml_float_operation_initialize
caml_float_operation_initialize:
	M_ALLOC_FLOAT
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
	movff	FSR1L, ACCUL		; ACCU <- @result
	movff	FSR1H, ACCUH
	movff	POSTINC0, AARGB2
	movff	POSTINC0, AARGB1
	movff	POSTINC0, AARGB0
	movff	INDF0, AEXP
	movsf	[0x1], FSR0L
	movsf	[0x2], FSR0H
	movff	POSTINC0, BARGB2
	movff	POSTINC0, BARGB1
	movff	POSTINC0, BARGB0
	movff	INDF0, BEXP
	return
#endif

#ifdef caml_useprim_caml_float_operation_finalize
caml_float_operation_finalize:
	movff	AARGB2, POSTINC1
	movff	AARGB1, POSTINC1
	movff	AARGB0, POSTINC1
	movff	AEXP, POSTINC1
	return
#endif

#ifdef caml_useprim_FPA32
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_FPS32
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_FPD32
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_FPM32
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_INT3232
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_INT3224
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_NRM4032
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_FLO3232
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_NRM3232
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif
#ifdef caml_useprim_FLO2432
#ifndef caml_use_float_library
#define caml_use_float_library
#endif
#endif

#ifdef caml_use_float_library

#ifdef caml_useprim_FPS32
#ifndef caml_useprim_FPA32
#define caml_useprim_FPA32
#endif
#endif

#ifdef caml_useprim_FPA32
#ifndef caml_useprim_NRM4032
#define caml_useprim_NRM4032
#endif
#ifndef caml_useprim_FPM32
#define caml_useprim_FPM32
#endif
#endif
        
#ifdef caml_useprim_FLO2432
#ifndef caml_useprim_NRM3232
#define caml_useprim_NRM3232
#endif
#endif

#ifdef caml_useprim_FLO3232
#ifndef caml_useprim_NRM4032
#define caml_useprim_NRM4032
#endif
#endif

#ifdef caml_useprim_NRM4032
#ifndef caml_useprim_SETFUN32
#define caml_useprim_SETFUN32
#endif
#ifndef caml_useprim_NRM3232
#define caml_useprim_NRM3232
#endif
#ifndef caml_useprim_FPM32
#define caml_useprim_FPM32
#endif
#endif

#ifdef caml_useprim_NRM3232
#ifndef caml_useprim_SETFUN32
#define caml_useprim_SETFUN32
#endif
#endif
        
#ifdef caml_useprim_FPD32
#ifndef caml_useprim_SETFUN32
#define caml_useprim_SETFUN32
#endif
#ifndef caml_useprim_FPM32
#define caml_useprim_FPM32
#endif
#endif

#ifdef caml_useprim_FPM32
#ifndef caml_useprim_SETFUN32
#define caml_useprim_SETFUN32
#endif
#endif

;-------------------------------------------;
;                                           ;
;   File MATH16.INC (modified) from AN575   ;
;                                           ;
;-------------------------------------------;
	
;;	STATUS bit definitions

#define		_C	STATUS,0
#define		_Z	STATUS,2

;;	define assembler constants

B0		equ	0
B1		equ	1
B2		equ	2
B3		equ	3
B4		equ	4
B5		equ	5
B6		equ	6
B7		equ	7

MSB		equ	7
LSB		equ	0

;;	general register variables

ACCB7           equ     GC_TMP_REG_1
ACCB6           equ     GC_TMP_REG_2
ACCB5           equ     GC_TMP_REG_3
ACCB4           equ     GC_TMP_REG_4
ACCB3           equ     GC_TMP_REG_5
ACCB2           equ     TMP_REG_1
ACCB1           equ     TMP_REG_2
ACCB0           equ     TMP_REG_3
ACC             equ     ACCB0        ; most significant byte of contiguous
				     ; 8 byte accumulator
SIGN            equ     PRODL        ; save location for sign in MSB

TEMPB3          equ     TMP_REG_A
TEMPB2          equ     TMP_REG_B
TEMPB1          equ     TMP_REG_C
TEMPB0          equ     TMP_REG_D
TEMP            equ     TEMPB0    ; temporary storage

;;       binary operation arguments

AARGB7          equ     ACCB7
AARGB6          equ     ACCB6
AARGB5          equ     ACCB5
AARGB4          equ     ACCB4
AARGB3          equ     ACCB3
AARGB2          equ     ACCB2
AARGB1          equ     ACCB1
AARGB0          equ     ACCB0
AARG            equ     AARGB0       ; most significant byte of argument A

BARGB3          equ     TMP_REG_5
BARGB2          equ     TMP_REG_6
BARGB1          equ     TMP_REG_7
BARGB0          equ     TMP_REG_8
BARG            equ     BARGB0       ; most significant byte of argument B

; Note that AARG and ACC reference the same storage locations

;;	literal constants

EXPBIAS         equ     D'127'

;;	biased exponents

EXP             equ     TMP_REG_4    ; 8 bit biased exponent
AEXP            equ    	TMP_REG_4    ; 8 bit biased exponent for argument A
BEXP            equ     TMP_REG_9    ; 8 bit biased exponent for argument B

;;	floating point library exception flags

FPFLAGS         equ     PRODH ; floating point library exception flags
IOV             equ     0     ; bit0 = integer overflow flag
FOV             equ     1     ; bit1 = floating point overflow flag
FUN             equ     2     ; bit2 = floating point underflow flag
FDZ             equ     3     ; bit3 = floating point divide by zero flag
NAN		equ	4     ; bit4 = not-a-number exception flag
DOM		equ	5     ; bit5 = domain error exception flag
RND             equ     6     ; bit6 = floating point rounding flag,
			      ; 0 = truncation
                              ; 1 = unbiased rounding to nearest LSB
SAT             equ     7     ; bit7 = floating point saturate flag,
			      ; 0 = terminate on exception without saturation,
			      ; 1 = terminate on exception with saturation
			      ; to appropriate value
#endif

;-----------------------------------------;
;                                         ;
;   File FP32.A16 (modified) from AN575   ;
;                                         ;
;-----------------------------------------;
	
;	RCS Header $Id: fp32.a16 2.8 1996/10/07 13:50:59 F.J.Testa Exp $

;	$Revision: 2.8 $

;       PIC16 32 BIT FLOATING POINT LIBRARY
;
;       Unary operations: both input and output are in AEXP,AARG
;
;       Binary operations: input in AEXP,AARG and BEXP,BARG with output in AEXP,AARG
;
;       All routines return WREG = 0x00 for successful completion, and WREG = 0xFF
;       for an error condition specified in FPFLAGS.
;
;       All timings are worst case cycle counts
;
;         Routine               Function
;
;       FLO2432         24 bit integer to 32 bit floating point conversion
;       FLO32
;
;               Timing:            RND
;                               0       1
;
;                       0       104     104
;                  SAT
;                       1       110     110
;
;       NRM3232   32 bit normalization of unnormalized 32 bit floating point numbers
;       NRM32
;
;               Timing:            RND
;                               0       1
;
;                       0       90      90
;                  SAT
;                       1       96      96
;
;
;       INT3224         32 bit floating point to 24 bit integer conversion
;       INT32
;
;
;               Timing:            RND
;                               0       1
;
;                       0       104      112
;                  SAT
;                       1       104      114
;
;       FLO3232 32 bit integer to 32 bit floating point conversion
;
;               Timing:            RND
;                               0       1
;
;                       0       129     145
;                  SAT
;                       1       129     152
;
;       NRM4032 32 bit normalization of unnormalized 40 bit floating point numbers
;
;               Timing:            RND
;                               0       1
;
;                       0       112     128
;                  SAT
;                       1       112     135
;
;
;       INT3232         32 bit floating point to 32 bit integer conversion
;
;
;               Timing:            RND
;                               0       1
;
;                       0       130     137
;                  SAT
;                       1       130     137
;
;       FPA32           32 bit floating point add
;
;               Timing:            RND
;                               0       1
;
;                       0       251     265
;                  SAT
;                       1       251     271
;
;       FPS32           32 bit floating point subtract
;
;               Timing:            RND
;                               0       1
;
;                       0       253     267
;                  SAT
;                       1       253     273
;
;       FPM32           32 bit floating point multiply
;
;               Timing:            RND
;                               0       1
;
;                       0       574     588
;                  SAT
;                       1       574     591
;
;       FPD32           32 bit floating point divide
;
;               Timing:            RND
;                               0       1
;
;                       0       932     968
;                  SAT
;                       1       932     971
;
;
;**********************************************************************************************
;**********************************************************************************************
;
;       32 bit floating point representation
;
;       EXPONENT        8 bit biased exponent
;
;                       It is important to note that the use of biased exponents produces
;                       a unique representation of a floating point 0, given by
;                       EXP = HIGHBYTE = MIDBYTE = LOWBYTE = 0x00, with 0 being
;                       the only number with EXP = 0.
;
;       HIGHBYTE        8 bit most significant byte of fraction in sign-magnitude representation,
;                       with SIGN = MSB, implicit MSB = 1 and radix point to the right of MSB
;
;       MIDBYTE         8 bit middle significant byte of sign-magnitude fraction
;
;       LOWBYTE         8 bit least significant byte of sign-magnitude fraction
;
;       EXPONENT        HIGHBYTE        MIDBYTE         LOWBYTE
;
;       xxxxxxxx        S.xxxxxxx       xxxxxxxx        xxxxxxxx
;
;                        |
;                      RADIX
;                      POINT
;
;
;**********************************************************************************************
;**********************************************************************************************

;       Integer to float conversion

;       Input:  24 bit 2's complement integer right justified in AARGB0, AARGB1, AARGB2

;       Use:    CALL    FLO2432 or      CALL    FLO32

;       Output: 32 bit floating point number in AEXP, AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  FLOAT( AARG )

;       Max Timing:     14+90 = 104 clks                SAT = 0
;                       14+96 = 110 clks                SAT = 1

;       Min Timing:     6+28 = 34 clks                  AARG = 0
;                       6+18 = 24 clks

;       PM: 14+38 = 52                                  DM: 7

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_FLO2432
FLO2432
FLO32           MOVLW           D'23'+EXPBIAS		; initialize exponent and add bias
                MOVWF           EXP
                CLRF            SIGN
                BTFSS           AARGB0,MSB		; test sign
                BRA             NRM3232
                COMF            AARGB2,F                ; if < 0, negate and set MSB in SIGN
                COMF            AARGB1,F
                COMF            AARGB0,F
                INCF            AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F
                BSF             SIGN,MSB
#endif

;**********************************************************************************************

;       Normalization routine

;       Input:  32 bit unnormalized floating point number in AEXP, AARGB0, AARGB1,
;               AARGB2, with sign in SIGN,MSB

;       Use:    CALL    NRM3232 or      CALL    NRM32

;       Output: 32 bit normalized floating point number in AEXP, AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  NORMALIZE( AARG )

;       Max Timing:     21+6+7*8+7 = 90 clks            SAT = 0
;                       21+6+7*8+1+12 = 96 clks SAT = 1

;       Min Timing:     22+6 = 28 clks                  AARG = 0
;                       5+9+4 = 18 clks

;       PM: 38                                          DM: 7

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_NRM3232
NRM3232

NRM32           CLRF            TEMP			; clear exponent decrement
                MOVF            AARGB0,W		; test if highbyte=0
                BTFSS           _Z
                BRA             NORM3232
                MOVF            AARGB1,W		; if so, shift 8 bits by move
                MOVWF           AARGB0
                MOVF            AARGB2,W
                MOVWF           AARGB1
                CLRF            AARGB2
                BSF             TEMP,3                  ; increase decrement by 8

                MOVF            AARGB0,W		; test if highbyte=0
                BTFSS           _Z
                BRA             NORM3232
                MOVF            AARGB1,W		; if so, shift 8 bits by move
                MOVWF           AARGB0
                CLRF            AARGB1
                BCF             TEMP,3                  ; increase decrement by 8
                BSF             TEMP,4
        
                MOVF            AARGB0,W		; if highbyte=0, result=0
                BTFSC           _Z
                BRA             RES032

NORM3232        MOVF            TEMP,W
                SUBWF           EXP,F
                BTFSS           _Z
                BTFSS           _C
                BRA             SETFUN32

                BCF             _C                      ; clear carry bit

NORM3232A       BTFSC           AARGB0,MSB		; if MSB=1, normalization done
                BRA             FIXSIGN32
                RLCF            AARGB2,F                ; otherwise, shift left and 
                RLCF            AARGB1,F                ; decrement EXP
                RLCF            AARGB0,F
                DECFSZ          EXP,F
                BRA             NORM3232A

                BRA             SETFUN32                ; underflow if EXP=0

FIXSIGN32       BTFSS           SIGN,MSB
                BCF             AARGB0,MSB              ; clear explicit MSB if positive
                RETLW           0

RES032          CLRF            AARGB0                  ; result equals zero
                CLRF            AARGB1
                CLRF            AARGB2
		CLRF		AARGB3
                CLRF            EXP
                RETLW           0
#endif

;**********************************************************************************************
;**********************************************************************************************

;       Integer to float conversion

;       Input:  32 bit 2's complement integer right justified in AARGB0, AARGB1, AARGB2,
;               AARGB3

;       Use:    CALL    FLO3232

;       Output: 32 bit floating point number in AEXP, AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  FLOAT( AARG )

;       Max Timing:     17+112 = 129 clks               RND = 0
;                       17+128 = 145 clks               RND = 1, SAT = 0
;                       17+135 = 152 clks               RND = 1, SAT = 1

;       Min Timing:     6+39 = 45 clks                  AARG = 0
;                       6+22 = 28 clks

;       PM: 17+66 = 83                                  DM: 8

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_FLO3232
FLO3232         MOVLW           D'31'+EXPBIAS		; initialize exponent and add bias
                MOVWF           EXP
                CLRF            SIGN
                BTFSS           AARGB0,MSB		; test sign
                BRA             NRM4032
                COMF            AARGB3,F                ; if < 0, negate and set MSB in SIGN
                COMF            AARGB2,F
                COMF            AARGB1,F
                COMF            AARGB0,F
                INCF            AARGB3,F
                BTFSC           _Z
                INCF            AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F
                BSF             SIGN,MSB
#endif

;**********************************************************************************************

;       Normalization routine

;       Input:  40 bit unnormalized floating point number in AEXP, AARGB0, AARGB1,
;               AARGB2, AARGB3 with sign in SIGN,MSB

;       Use:    CALL    NRM4032

;       Output: 32 bit normalized floating point number in AEXP, AARGB0, AARGB1, AARGB2,
;               AARGB3

;       Result: AARG  <--  NORMALIZE( AARG )

;       Max Timing:     38+6*9+12+8 = 112 clks  RND = 0
;                       38+6*9+12+24 = 128 clks RND = 1, SAT = 0
;                       38+6*9+12+31 = 135 clks RND = 1, SAT = 1

;       Min Timing:     33+6 = 39 clks                  AARG = 0
;                       5+9+8 = 22 clks

;       PM: 66                                          DM: 8

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_NRM4032
NRM4032         CLRF            TEMP			; clear exponent decrement
                MOVF            AARGB0,W		; test if highbyte=0
                BTFSS           _Z
                BRA             NORM4032
                MOVF            AARGB1,W		; if so, shift 8 bits by move
                MOVWF           AARGB0
                MOVF            AARGB2,W
                MOVWF           AARGB1
                MOVF            AARGB3,W
                MOVWF           AARGB2
                CLRF            AARGB3
                BSF             TEMP,3                  ; increase decrement by 8

                MOVF            AARGB0,W		; test if highbyte=0
                BTFSS           _Z
                BRA             NORM4032
                MOVF            AARGB1,W		; if so, shift 8 bits by move
                MOVWF           AARGB0
                MOVF            AARGB2,W
                MOVWF           AARGB1
                CLRF            AARGB2
                BCF             TEMP,3                  ; increase decrement by 8
                BSF             TEMP,4
        
                MOVF            AARGB0,W		; test if highbyte=0
                BTFSS           _Z
                BRA             NORM4032
                MOVF            AARGB1,W		; if so, shift 8 bits by move
                MOVWF           AARGB0
                CLRF            AARGB1
                BSF             TEMP,3                  ; increase decrement by 8
        
                MOVF            AARGB0,W		; if highbyte=0, result=0
                BTFSC           _Z
                BRA             RES032

NORM4032        MOVF            TEMP,W
                SUBWF           EXP,F
                BTFSS           _Z
                BTFSS           _C
                BRA             SETFUN32

                BCF             _C                      ; clear carry bit

NORM4032A       BTFSC           AARGB0,MSB		; if MSB=1, normalization done
                BRA             NRMRND4032
                RLCF            AARGB3,F                ; otherwise, shift left and 
                RLCF            AARGB2,F                ; decrement EXP
                RLCF            AARGB1,F
                RLCF            AARGB0,F
                DECFSZ          EXP,F
                BRA             NORM4032A

                BRA             SETFUN32                ; underflow if EXP=0

NRMRND4032      BTFSC           FPFLAGS,RND
                BTFSS           AARGB2,LSB
                BRA             FIXSIGN32
		BTFSS		AARGB3,MSB		; round if next bit is set
                BRA             FIXSIGN32
		INCF		AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F

                BTFSS           _Z                      ; has rounding caused carryout?
                BRA             FIXSIGN32
                RRCF            AARGB0,F                ; if so, right shift
                RRCF            AARGB1,F
                RRCF            AARGB2,F
                INCF            EXP,F
                BTFSC           _Z                      ; check for overflow
                BRA             SETFOV32
                BRA             FIXSIGN32
#endif

;**********************************************************************************************
;**********************************************************************************************

;       Float to integer conversion

;       Input:  32 bit floating point number in AEXP, AARGB0, AARGB1, AARGB2

;       Use:    CALL    INT3224         or      CALL    INT32

;       Output: 24 bit 2's complement integer right justified in AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  INT( AARG )

;       Max Timing:     40+6*7+6+16 = 104 clks		RND = 0
;                       40+6*7+6+24 = 112 clks		RND = 1, SAT = 0
;                       40+6*7+6+26 = 114 clks  	RND = 1, SAT = 1

;       Min Timing:     4 clks

;       PM: 82                                          DM: 6

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_INT3224
INT3224
INT32
		MOVF		EXP,W			; test for zero argument
		BTFSC		_Z
		RETLW		0x00

		MOVF            AARGB0,W		; save sign in SIGN
                MOVWF           SIGN
                BSF             AARGB0,MSB		; make MSB explicit

                MOVLW           EXPBIAS+D'23'		; remove bias from EXP
                SUBWF           EXP,F
                BTFSS           EXP,MSB
                BRA             SETIOV3224
		COMF		EXP,F
		INCF		EXP,F

                MOVLW           8                       ; do byte shift if EXP >= 8
                SUBWF           EXP,W
                BTFSS           _C
                BRA             TSHIFT3224
                MOVWF           EXP
                RLCF            AARGB2,F                ; rotate next bit for rounding
                MOVF            AARGB1,W
                MOVWF           AARGB2
                MOVF            AARGB0,W
                MOVWF           AARGB1
                CLRF            AARGB0

                MOVLW           8                       ; do another byte shift if EXP >= 8
                SUBWF           EXP,W
                BTFSS           _C
                BRA             TSHIFT3224
                MOVWF           EXP
                RLCF            AARGB2,F		; rotate next bit for rounding
                MOVF            AARGB1,W
                MOVWF           AARGB2
                CLRF            AARGB1

                MOVLW           8                       ; do another byte shift if EXP >= 8
                SUBWF           EXP,W
                BTFSS           _C
                BRA             TSHIFT3224
                MOVWF           EXP
                RLCF            AARGB2,F                ; rotate next bit for rounding
                CLRF            AARGB2
		MOVF		EXP,W
		BTFSS		_Z
		BCF		_C
		BRA 		SHIFT3224OK

TSHIFT3224      MOVF            EXP,W                   ; shift completed if EXP = 0
                BTFSC           _Z
                BRA             SHIFT3224OK

SHIFT3224       BCF             _C
                RRCF            AARGB0,F                ; right shift by EXP
                RRCF            AARGB1,F
                RRCF            AARGB2,F
                DECFSZ          EXP,F
                BRA             SHIFT3224

SHIFT3224OK     BTFSC           FPFLAGS,RND
                BTFSS           AARGB2,LSB
                BRA             INT3224OK
                BTFSS           _C
                BRA             INT3224OK
                INCF            AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F
                BTFSC           AARGB0,MSB		; test for overflow
                BRA             SETIOV3224

INT3224OK       BTFSS           SIGN,MSB                ; if sign bit set, negate               
                RETLW           0
                COMF            AARGB0,F
                COMF            AARGB1,F
                COMF            AARGB2,F
                INCF            AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F
                RETLW           0

IRES03224	CLRF            AARGB0			; integer result equals zero
                CLRF            AARGB1
                CLRF            AARGB2
                RETLW           0

SETIOV3224	BSF             FPFLAGS,IOV             ; set integer overflow flag
                BTFSS           FPFLAGS,SAT             ; test for saturation
                RETLW           0xFF                    ; return error code in WREG

                CLRF            AARGB0			; saturate to largest two's
                BTFSS           SIGN,MSB                ; complement 24 bit integer
                MOVLW           0xFF
                MOVWF           AARGB0			; SIGN = 0, 0x 7F FF FF
                MOVWF           AARGB1			; SIGN = 1, 0x 80 00 00
                MOVWF           AARGB2
                RLCF            SIGN,F
                RRCF            AARGB0,F
                RETLW           0xFF                    ; return error code in WREG
#endif

;**********************************************************************************************
;**********************************************************************************************

;       Float to integer conversion

;       Input:  32 bit floating point number in AEXP, AARGB0, AARGB1, AARGB2

;       Use:    CALL    INT3232

;       Output: 32 bit 2's complement integer right justified in AARGB0, AARGB1, AARGB2,
;               AARGB3

;       Result: AARG  <--  INT( AARG )

;       Max Timing:     54+6*8+7+21 = 130 clks          RND = 0
;                       54+6*8+7+29 = 137 clks          RND = 1, SAT = 0
;                       54+6*8+7+29 = 137 clks          RND = 1, SAT = 1

;       Min Timing:     5 clks

;       PM: 102                                                 DM: 7

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_INT3232
INT3232
                CLRF            AARGB3
		MOVF		EXP,W			; test for zero argument
		BTFSC		_Z
		RETLW		0x00

		MOVF            AARGB0,W		; save sign in SIGN
                MOVWF           SIGN
                BSF             AARGB0,MSB		; make MSB explicit

                MOVLW           EXPBIAS+D'31'		; remove bias from EXP
                SUBWF           EXP,F
                BTFSS           EXP,MSB
                BRA             SETIOV32
		COMF		EXP,F
		INCF		EXP,F        

                MOVLW           8                       ; do byte shift if EXP >= 8
                SUBWF           EXP,W
                BTFSS           _C
                BRA             TSHIFT3232
                MOVWF           EXP
                RLCF            AARGB3,F                ; rotate next bit for rounding
                MOVF            AARGB2,W
                MOVWF           AARGB3
                MOVF            AARGB1,W
                MOVWF           AARGB2
                MOVF            AARGB0,W
                MOVWF           AARGB1
                CLRF            AARGB0

                MOVLW           8                       ; do another byte shift if EXP >= 8
                SUBWF           EXP,W
                BTFSS           _C
                BRA             TSHIFT3232
                MOVWF           EXP
                RLCF            AARGB3,F                ; rotate next bit for rounding
                MOVF            AARGB2,W
                MOVWF           AARGB3
                MOVF            AARGB1,W
                MOVWF           AARGB2
                CLRF            AARGB1

                MOVLW           8                       ; do another byte shift if EXP >= 8
                SUBWF           EXP,W
                BTFSS           _C
                BRA             TSHIFT3232
                MOVWF           EXP
                RLCF            AARGB3,F                ; rotate next bit for rounding
                MOVF            AARGB2,W
                MOVWF           AARGB3
                CLRF            AARGB2

                MOVLW           8                       ; do another byte shift if EXP >= 8
                SUBWF           EXP,W
                BTFSS           _C
                BRA             TSHIFT3232
                MOVWF           EXP
                RLCF            AARGB3,F                ; rotate next bit for rounding
                CLRF            AARGB3
		MOVF		EXP,W
		BTFSS		_Z
		BCF		_C
		BRA 		SHIFT3232OK

TSHIFT3232      MOVF            EXP,W                   ; shift completed if EXP = 0
                BTFSC           _Z
                BRA             SHIFT3232OK

SHIFT3232       BCF             _C
                RRCF            AARGB0,F                ; right shift by EXP
                RRCF            AARGB1,F
                RRCF            AARGB2,F
                RRCF            AARGB3,F
                DECFSZ          EXP,F
                BRA             SHIFT3232

SHIFT3232OK     BTFSC           FPFLAGS,RND
                BTFSS           AARGB3,LSB
                BRA             INT3232OK
                BTFSS           _C
                BRA             INT3232OK
                INCF            AARGB3,F
                BTFSC           _Z
                INCF            AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F
                BTFSC           AARGB0,MSB		; test for overflow
                BRA             SETIOV3224

INT3232OK       BTFSS           SIGN,MSB                ; if sign bit set, negate               
                RETLW           0
                COMF            AARGB0,F
                COMF            AARGB1,F
                COMF            AARGB2,F
                COMF            AARGB3,F
                INCF            AARGB3,F
                BTFSC           _Z
                INCF            AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F
                RETLW           0

IRES032         CLRF            AARGB0			; integer result equals zero
                CLRF            AARGB1
                CLRF            AARGB2
                CLRF            AARGB3
                RETLW           0

SETIOV32        BSF             FPFLAGS,IOV             ; set integer overflow flag
                BTFSS           FPFLAGS,SAT             ; test for saturation
                RETLW           0xFF                    ; return error code in WREG

                CLRF            AARGB0			; saturate to largest two's
                BTFSS           SIGN,MSB                ; complement 32 bit integer
                MOVLW           0xFF
                MOVWF           AARGB0			; SIGN = 0, 0x 7F FF FF FF
                MOVWF           AARGB1			; SIGN = 1, 0x 80 00 00 00
                MOVWF           AARGB2
                MOVWF           AARGB3
                RLCF            SIGN,F
                RRCF            AARGB0,F
                RETLW           0xFF                    ; return error code in WREG
#endif

;**********************************************************************************************
;**********************************************************************************************

;       Floating Point Multiply

;       Input:  32 bit floating point number in AEXP, AARGB0, AARGB1, AARGB2
;               32 bit floating point number in BEXP, BARGB0, BARGB1, BARGB2

;       Use:    CALL    FPM32

;       Output: 32 bit floating point product in AEXP, AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  AARG * BARG

;       Max Timing:     26+23*22+21+21 = 574 clks       RND = 0
;                       26+23*22+21+35 = 588 clks       RND = 1, SAT = 0
;                       26+23*22+21+38 = 591 clks       RND = 1, SAT = 1

;       Min Timing:     6+6 = 12 clks                   AARG * BARG = 0
;                       24+23*11+21+17 = 315 clks

;       PM: 94                                          DM: 14

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_FPM32
FPM32           MOVF            AEXP,W                  ; test for zero arguments
                BTFSS           _Z
                MOVF            BEXP,W
                BTFSC           _Z
                BRA             RES032

M32BNE0         MOVF            AARGB0,W
                XORWF           BARGB0,W
                MOVWF           SIGN                    ; save sign in SIGN

                MOVF            BEXP,W
                ADDWF           EXP,F
                MOVLW           EXPBIAS-1
                BTFSS           _C
                BRA             MTUN32

                SUBWF           EXP,F
                BTFSC           _C
                BRA             SETFOV32                ; set multiply overflow flag
                BRA             MOK32

MTUN32          SUBWF           EXP,F
                BTFSS           _C
                BRA             SETFUN32

MOK32		MOVF		AARGB0,W
		MOVWF		AARGB3
		MOVF		AARGB1,W
		MOVWF		AARGB4
		MOVF		AARGB2,W
		MOVWF		AARGB5
		BSF             AARGB3,MSB              ; make argument MSB's explicit
                BSF             BARGB0,MSB
                BCF             _C
                CLRF            AARGB0			; clear initial partial product
                CLRF            AARGB1
                CLRF            AARGB2
                MOVLW           D'24'
                MOVWF           TEMP                    ; initialize counter

MLOOP32         BTFSS           AARGB5,LSB              ; test next bit
                BRA             MNOADD32

MADD32          MOVF            BARGB2,W
                ADDWF           AARGB2,F
                MOVF            BARGB1,W
                BTFSC           _C
                INCFSZ          BARGB1,W
                ADDWF           AARGB1,F

                MOVF            BARGB0,W
                BTFSC           _C
                INCFSZ          BARGB0,W
                ADDWF           AARGB0,F

MNOADD32        RRCF            AARGB0,F
                RRCF            AARGB1,F
                RRCF            AARGB2,F
                RRCF            AARGB3,F
                RRCF            AARGB4,F
                RRCF            AARGB5,F
                BCF             _C
                DECFSZ          TEMP,F
                BRA             MLOOP32

                BTFSC           AARGB0,MSB               ; check for postnormalization
                BRA             MROUND32
                RLCF            AARGB3,F
                RLCF            AARGB2,F
                RLCF            AARGB1,F
                RLCF            AARGB0,F
                DECF            EXP,F

MROUND32        BTFSC           FPFLAGS,RND
                BTFSS           AARGB2,LSB
                BRA             MUL32OK
		BTFSS		AARGB3,MSB
                BRA             MUL32OK
		INCF		AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F

                BTFSS           _Z                      ; has rounding caused carryout?
                BRA             MUL32OK
                RRCF            AARGB0,F                ; if so, right shift
                RRCF            AARGB1,F
                RRCF            AARGB2,F
                INCF            EXP,F
                BTFSC           _Z                      ; check for overflow
                BRA             SETFOV32

MUL32OK         BTFSS           SIGN,MSB
                BCF             AARGB0,MSB		; clear explicit MSB if positive

                RETLW           0  

SETFOV32        BSF             FPFLAGS,FOV             ; set floating point underflag
                BTFSS           FPFLAGS,SAT             ; test for saturation
                RETLW           0xFF                    ; return error code in WREG

                MOVLW           0xFF
                MOVWF           AEXP                    ; saturate to largest floating
                MOVWF           AARGB0                  ; point number = 0x FF 7F FF FF
                MOVWF           AARGB1                  ; modulo the appropriate sign bit
                MOVWF           AARGB2
                RLCF            SIGN,F
                RRCF            AARGB0,F
                RETLW           0xFF                    ; return error code in WREG
#endif

;**********************************************************************************************
;**********************************************************************************************

;       Floating Point Divide

;       Input:  32 bit floating point dividend in AEXP, AARGB0, AARGB1, AARGB2
;               32 bit floating point divisor in BEXP, BARGB0, BARGB1, BARGB2

;       Use:    CALL    FPD32

;       Output: 32 bit floating point quotient in AEXP, AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  AARG / BARG

;       Max Timing:     43+12+23*36+35+14 = 932 clks            RND = 0
;                       43+12+23*36+35+50 = 968 clks            RND = 1, SAT = 0
;                       43+12+23*36+35+53 = 971 clks            RND = 1, SAT = 1

;       Min Timing:     7+6 = 13 clks

;       PM: 155                                                 DM: 14

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_FPD32
FPD32           MOVF            BEXP,W                  ; test for divide by zero
                BTFSC           _Z
                BRA             SETFDZ32

                MOVF            AEXP,W
                BTFSC           _Z
                BRA             RES032

D32BNE0         MOVF            AARGB0,W
                XORWF           BARGB0,W
                MOVWF           SIGN                    ; save sign in SIGN
                BSF             AARGB0,MSB              ; make argument MSB's explicit
                BSF             BARGB0,MSB

TALIGN32        CLRF            TEMP                    ; clear align increment
                MOVF            AARGB0,W
                MOVWF           AARGB3			; test for alignment
                MOVF            AARGB1,W
                MOVWF           AARGB4
                MOVF            AARGB2,W
                MOVWF           AARGB5

                MOVF            BARGB2,W
                SUBWF           AARGB5,F
                MOVF            BARGB1,W
                BTFSS           _C
                INCFSZ          BARGB1,W

TS1ALIGN32      SUBWF           AARGB4,F
                MOVF            BARGB0,W
                BTFSS           _C
                INCFSZ          BARGB0,W

TS2ALIGN32      SUBWF           AARGB3,F

                CLRF            AARGB3
                CLRF            AARGB4
                CLRF            AARGB5

                BTFSS           _C
                BRA             DALIGN32OK

                BCF             _C                      ; align if necessary
                RRCF            AARGB0,F
                RRCF            AARGB1,F
                RRCF            AARGB2,F
                RRCF            AARGB3,F
                MOVLW           0x01
                MOVWF           TEMP                    ; save align increment          

DALIGN32OK      MOVF            BEXP,W                  ; compare AEXP and BEXP
                SUBWF           EXP,F
                BTFSS           _C
                BRA             ALTB32
        
AGEB32          MOVLW           EXPBIAS-1
                ADDWF           TEMP,W
                ADDWF           EXP,F
                BTFSC           _C
                BRA             SETFOV32
                BRA             DARGOK32                ; set overflow flag

ALTB32          MOVLW           EXPBIAS-1
                ADDWF           TEMP,W
                ADDWF           EXP,F
                BTFSS           _C
                BRA             SETFUN32                ; set underflow flag

DARGOK32        MOVLW           D'24'			; initialize counter
                MOVWF           TEMPB1

DLOOP32         RLCF            AARGB5,F                ; left shift
                RLCF            AARGB4,F
                RLCF            AARGB3,F
                RLCF            AARGB2,F
                RLCF            AARGB1,F
                RLCF            AARGB0,F
                RLCF            TEMP,F

                MOVF            BARGB2,W                ; subtract
                SUBWF           AARGB2,F
                MOVF            BARGB1,W
                BTFSS           _C
                INCFSZ          BARGB1,W
DS132           SUBWF           AARGB1,F

                MOVF            BARGB0,W
                BTFSS           _C
                INCFSZ          BARGB0,W
DS232           SUBWF           AARGB0,F

                RLCF            BARGB0,W
                IORWF           TEMP,F
                
                BTFSS           TEMP,LSB                ; test for restore
                BRA             DREST32

                BSF             AARGB5,LSB
                BRA             DOK32

DREST32         MOVF            BARGB2,W                ; restore if necessary
                ADDWF           AARGB2,F
                MOVF            BARGB1,W
                BTFSC           _C
                INCFSZ          BARGB1,W
DAREST32        ADDWF           AARGB1,F

                MOVF            BARGB0,W
                BTFSC           _C
                INCF            BARGB0,W
                ADDWF           AARGB0,F

                BCF             AARGB5,LSB

DOK32           DECFSZ          TEMPB1,F
                BRA             DLOOP32

DROUND32        BTFSC           FPFLAGS,RND
                BTFSS           AARGB5,LSB
                BRA             DIV32OK
                BCF             _C
                RLCF            AARGB2,F               ; compute next significant bit
                RLCF            AARGB1,F               ; for rounding
                RLCF            AARGB0,F
                RLCF            TEMP,F

                MOVF            BARGB2,W               ; subtract
                SUBWF           AARGB2,F
                MOVF            BARGB1,W
                BTFSS           _C
                INCFSZ          BARGB1,W
		SUBWF           AARGB1,F

                MOVF            BARGB0,W
                BTFSS           _C
                INCFSZ          BARGB0,W
		SUBWF           AARGB0,F

                RLCF            BARGB0,W
                IORWF           TEMP,W
                ANDLW           0x01            

                ADDWF           AARGB5,F
                BTFSC           _C
                INCF            AARGB4,F
                BTFSC           _Z
                INCF            AARGB3,F

                BTFSS           _Z                      ; test if rounding caused carryout
                BRA             DIV32OK
                RRCF            AARGB3,F
                RRCF            AARGB4,F
                RRCF            AARGB5,F
                INCF            EXP,F
                BTFSC           _Z                      ; test for overflow
                BRA             SETFOV32


DIV32OK         BTFSS           SIGN,MSB
                BCF             AARGB3,MSB		; clear explicit MSB if positive

                MOVF            AARGB3,W
                MOVWF           AARGB0                  ; move result to AARG
                MOVF            AARGB4,W
                MOVWF           AARGB1
                MOVF            AARGB5,W
                MOVWF           AARGB2

                RETLW           0

SETFDZ32        BSF             FPFLAGS,FDZ             ; set divide by zero flag
                RETLW           0xFF
#endif

#ifdef caml_useprim_SETFUN32
SETFUN32        BSF             FPFLAGS,FUN             ; set floating point underflag
                BTFSS           FPFLAGS,SAT             ; test for saturation
                RETLW           0xFF                    ; return error code in WREG

                MOVLW           0x01                    ; saturate to smallest floating
                MOVWF           AEXP                    ; point number = 0x 01 00 00 00
                CLRF            AARGB0                  ; modulo the appropriate sign bit
                CLRF            AARGB1
                CLRF            AARGB2
                RLCF            SIGN,F
                RRCF            AARGB0,F
                RETLW           0xFF                    ; return error code in WREG
#endif

;**********************************************************************************************
;**********************************************************************************************

;       Floating Point Subtract

;       Input:  32 bit floating point number in AEXP, AARGB0, AARGB1, AARGB2
;               32 bit floating point number in BEXP, BARGB0, BARGB1, BARGB2

;       Use:    CALL FPS32

;       Output: 32 bit floating point sum in AEXP, AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  AARG - BARG

;       Max Timing:     2+251 = 253 clks                RND = 0
;                       2+265 = 267 clks                RND = 1, SAT = 0
;                       2+271 = 273 clks                RND = 1, SAT = 1

;       Min Timing:     2+12 = 14 clks

;       PM: 2+146 = 148                         DM: 14

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_FPS32
FPS32           MOVLW           0x80
                XORWF           BARGB0,F
#endif

;**********************************************************************************************

;       Floating Point Add

;       Input:  32 bit floating point number in AEXP, AARGB0, AARGB1, AARGB2
;               32 bit floating point number in BEXP, BARGB0, BARGB1, BARGB2

;       Use:    CALL FPA32

;       Output: 32 bit floating point sum in AEXP, AARGB0, AARGB1, AARGB2

;       Result: AARG  <--  AARG - BARG

;       Max Timing:     31+41+6*7+6+41+90 = 251 clks            RND = 0
;                       31+41+6*7+6+55+90 = 265 clks            RND = 1, SAT = 0
;                       31+41+6*7+6+55+96 = 271 clks            RND = 1, SAT = 1

;       Min Timing:     8+4 = 12 clks

;       PM: 146                                                 DM: 14

;----------------------------------------------------------------------------------------------

#ifdef caml_useprim_FPA32
FPA32           MOVF            AARGB0,W                ; exclusive or of signs in TEMP
                XORWF           BARGB0,W
                MOVWF           TEMP

		CLRF		AARGB3			; clear extended byte
		CLRF		BARGB3

                MOVF            AEXP,W                  ; use AARG if AEXP >= BEXP
                SUBWF           BEXP,W
                BTFSS           _C
                BRA             USEA32

                MOVF            BEXP,W                  ; use BARG if AEXP < BEXP
                MOVWF           AARGB5			; therefore, swap AARG and BARG
                MOVF            AEXP,W
                MOVWF           BEXP
                MOVF            AARGB5,W
                MOVWF           AEXP

                MOVF            BARGB0,W
                MOVWF           AARGB5
                MOVF            AARGB0,W
                MOVWF           BARGB0
                MOVF            AARGB5,W
                MOVWF           AARGB0

                MOVF            BARGB1,W
                MOVWF           AARGB5
                MOVF            AARGB1,W
                MOVWF           BARGB1
                MOVF            AARGB5,W
                MOVWF           AARGB1

                MOVF            BARGB2,W
                MOVWF           AARGB5
                MOVF            AARGB2,W
                MOVWF           BARGB2
                MOVF            AARGB5,W
                MOVWF           AARGB2

USEA32          MOVF            BEXP,W                  ; return AARG if BARG = 0
                BTFSC           _Z
                RETLW           0x00

                MOVF            AARGB0,W
                MOVWF           SIGN                    ; save sign in SIGN
                BSF             AARGB0,MSB              ; make MSB's explicit
                BSF             BARGB0,MSB

                MOVF            BEXP,W                  ; compute shift count in BEXP
                SUBWF           AEXP,W
                MOVWF           BEXP
                BTFSC           _Z
                BRA             ALIGNED32

                MOVLW           8
                SUBWF           BEXP,W
                BTFSS           _C                      ; if BEXP >= 8, do byte shift
                BRA             ALIGNB32
                MOVWF           BEXP
                MOVF            BARGB2,W		; keep for postnormalization
		MOVWF		BARGB3
                MOVF            BARGB1,W
		MOVWF		BARGB2
                MOVF            BARGB0,W
		MOVWF		BARGB1
                CLRF            BARGB0

                MOVLW           8
                SUBWF           BEXP,W
                BTFSS           _C                      ; if BEXP >= 8, do byte shift
                BRA             ALIGNB32
                MOVWF           BEXP
                MOVF            BARGB2,W		; keep for postnormalization
		MOVWF		BARGB3
                MOVF            BARGB1,W
		MOVWF		BARGB2
                CLRF            BARGB1

                MOVLW           8
                SUBWF           BEXP,W
                BTFSS           _C                      ; if BEXP >= 8, BARG = 0 relative to AARG
                BRA             ALIGNB32
                MOVF            SIGN,W
                MOVWF           AARGB0
                RETLW           0x00

ALIGNB32        MOVF            BEXP,W                  ; already aligned if BEXP = 0
                BTFSC           _Z
                BRA             ALIGNED32

ALOOPB32        BCF             _C                      ; right shift by BEXP
                RRCF            BARGB0,F
                RRCF            BARGB1,F
		RRCF		BARGB2,F
		RRCF		BARGB3,F
                DECFSZ          BEXP,F
                BRA             ALOOPB32

ALIGNED32       BTFSS           TEMP,MSB                ; negate if signs opposite
                BRA             AOK32

		COMF		BARGB3,F
		COMF		BARGB2,F
                COMF            BARGB1,F
                COMF            BARGB0,F
                INCF            BARGB3,F
                BTFSC           _Z
                INCF            BARGB2,F
		BTFSC		_Z
		INCF		BARGB1,F
		BTFSC		_Z
		INCF		BARGB0,F

AOK32
                MOVF   		BARGB3,W
                ADDWF   	AARGB3,F
                MOVF            BARGB2,W
                BTFSC           _C
                INCFSZ          BARGB2,W
                ADDWF           AARGB2,F
                MOVF            BARGB1,W
                BTFSC           _C
                INCFSZ          BARGB1,W
                ADDWF           AARGB1,F
                MOVF            BARGB0,W
                BTFSC           _C
                INCFSZ          BARGB0,W
                ADDWF           AARGB0,F

                BTFSC           TEMP,MSB
                BRA             ACOMP32
                BTFSS           _C
                BRA             NRMRND4032

                RRCF            AARGB0,F               ; shift right and increment EXP
                RRCF            AARGB1,F
                RRCF            AARGB2,F
		RRCF		AARGB3,F
                INCFSZ          AEXP,F
                BRA             NRMRND4032
                BRA             SETFOV32

ACOMP32         BTFSC           _C
                BRA             NRM4032			; normalize and fix sign

		COMF		AARGB3,F
                COMF            AARGB2,F		; negate, toggle sign bit and
                COMF            AARGB1,F		; then normalize
                COMF            AARGB0,F
                INCF            AARGB3,F
                BTFSC           _Z
                INCF            AARGB2,F
                BTFSC           _Z
                INCF            AARGB1,F
                BTFSC           _Z
                INCF            AARGB0,F

                MOVLW           0x80
                XORWF           SIGN,F
                BRA             NRM32
#endif
