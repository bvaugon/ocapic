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
;;;;;;;;               DEBUG               ;;;;;;;;
;;;;;;;;                                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_DEBUG_CHK:
	movf	FSR1L, W
	rcall	_DEBUG
	movf	FSR1H, W
	rcall	_DEBUG
	return
	
_RS	EQU	RA1
_RW	EQU	RA3
_E	EQU	RA0

DEBUG1	EQU	PRODL
DEBUG2	EQU	PRODH
DEBUG3	EQU	EEDATA
DEBUG4	EQU	TRISC
	
_SLEEP:
	movlw	0x1
	movwf	DEBUG2
_SLEEP_LOOP:
	decfsz	DEBUG1, F
	bra	_SLEEP_LOOP
	decfsz	DEBUG2, F
	bra	_SLEEP_LOOP
	return
	
_DODO:
	movlw	0x1
	movwf	DEBUG3
_DODO_LOOP:
	decfsz	DEBUG1, F
	bra	_DODO_LOOP
	decfsz	DEBUG2, F
	bra	_DODO_LOOP
	decfsz	DEBUG3, F
	bra	_DODO_LOOP
	return
	
_SEND:
	bcf	LATA, _RW
	clrf	TRISB
	movwf	PORTB
	rcall	_SLEEP
	bsf	LATA, _E
	rcall	_SLEEP
	bcf	LATA, _E
	rcall	_SLEEP
	return

_SEND_DATA:
	bsf	LATA, _RS
	bra	_SEND

_SEND_INSTR:
	bcf	LATA, _RS
	bra	_SEND

_CLEAR:
	movlw	0x1
	bra	_SEND_INSTR

_HOME:
	movlw	0x2
	bra	_SEND_INSTR

_SEND_HEX:
	addlw	(-0x0A)
	bc	_SEND_HEX_L0
	addlw	(0x0A + '0')
	bra	_SEND_DATA
_SEND_HEX_L0:
	addlw	'A'
	bra	_SEND_DATA

_DEBUG:
	movwf	DEBUG4
	rcall	_CLEAR
	rcall	_HOME
	movlw	'D'
	rcall	_SEND_DATA
	movlw	'e'
	rcall	_SEND_DATA
	movlw	'b'
	rcall	_SEND_DATA
	movlw	'u'
	rcall	_SEND_DATA
	movlw	'g'
	rcall	_SEND_DATA
	rcall	_DODO
	rcall	_CLEAR
	rcall	_HOME
	movlw	'0'
	rcall	_SEND_DATA
	movlw	'x'
	rcall	_SEND_DATA
	swapf	DEBUG4, W
	andlw	0x0F
	rcall	_SEND_HEX
	movf	DEBUG4, W
	andlw	0x0F
	rcall	_SEND_HEX
	rcall	_DODO
	rcall	_CLEAR
	rcall	_HOME
	movf	DEBUG4, W
	return

_DEBUG_INIT:
	clrf	TRISA
	clrf	TRISB
	clrf	PORTA
	clrf	PORTB
	call	_DODO
	movlw	.56
	call	_SEND_INSTR
	movlw	.14
	call	_SEND_INSTR
	movlw	.6
	call	_SEND_INSTR
	return

_DEBUG_TEST:
	call	_DEBUG_INIT
	movlw	0x56
	call	_DEBUG
_DEBUG_LOOOP:
	bra	_DEBUG_LOOOP



;_DEBUG_PRINT:
;	movff	FSR0L, TMP_REG_A
;	movff	FSR0H, TMP_REG_B
;	lfsr	FSR0, (STACK_ANCHOR - .39 * 0x2)
;	movff	POSTDEC0, PRODH
;	movff	POSTDEC0, PRODL
;	movff	PRODL, FSR0L
;	movff	PRODH, FSR0H
;
;	movlw	0x38
;	rcall	_DEBUG
;	movf	FSR0L, W
;	rcall	_DEBUG
;	movf	FSR0H, W
;	rcall	_DEBUG
;	subfsr	FSR0, 0x2
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	subfsr	FSR0, 0xC
;	
;	addfsr	FSR0, 0x8
;	movff	POSTINC0, PRODL
;	movff	POSTINC0, PRODH
;	movff	PRODL, FSR0L
;	movff	PRODH, FSR0H
;
;	subfsr	FSR0, 0x2
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	movf	POSTINC0, W
;	rcall	_DEBUG
;	subfsr	FSR0, 0x2
;	
;	movff	TMP_REG_A, FSR0L
;	movff	TMP_REG_B, FSR0H
;	return
;	
;	
;_DEBUG_CHECK:
;	movff	FSR0L, TMP_REG_A
;	movff	FSR0H, TMP_REG_B
;	lfsr	FSR0, (STACK_ANCHOR - .39 * 0x2)
;	movff	POSTDEC0, PRODH
;	movff	POSTDEC0, PRODL
;	movff	PRODL, FSR0L
;	movff	PRODH, FSR0H
;	addfsr	FSR0, 0x8
;	movff	POSTINC0, PRODL
;	movff	POSTINC0, PRODH
;	movff	PRODL, FSR0L
;	movff	PRODH, FSR0H
;	movlw	0xB7
;	cpfseq	POSTINC0, W
;	bra	_DEBUG_ERROR
;	movlw	0x34
;	cpfseq	POSTINC0, W
;	bra	_DEBUG_ERROR
;	movff	TMP_REG_A, FSR0L
;	movff	TMP_REG_B, FSR0H
;	return
;_DEBUG_ERROR:
;	movf	TBLPTRL, W
;	rcall	_DEBUG
;	movf	TBLPTRH, W
;	rcall	_DEBUG
;	goto	_STOP
;
;
;_DEBUG_CCK:
;	movlw	0x00
;	cpfseq	[0x1]
;	bra	_DEBUG_CCK_L0
;	movlw	0x00
;	cpfseq	[0x2]
;	bra	_DEBUG_CCK_L0
;	bra	_DEBUG_CCK_ERR
;_DEBUG_CCK_L0:
;	movlw	0x00
;	cpfseq	[0x3]
;	bra	_DEBUG_CCK_L1
;	movlw	0x00
;	cpfseq	[0x4]
;	bra	_DEBUG_CCK_L1
;	bra	_DEBUG_CCK_ERR
;_DEBUG_CCK_L1:
;	movlw	0x00
;	cpfseq	[0x5]
;	bra	_DEBUG_CCK_L2
;	movlw	0x00
;	cpfseq	[0x6]
;	bra	_DEBUG_CCK_L2
;	bra	_DEBUG_CCK_ERR
;_DEBUG_CCK_L2:
;	movlw	0x00
;	cpfseq	[0x7]
;	return
;	movlw	0x00
;	cpfseq	[0x8]
;	return
;_DEBUG_CCK_ERR:
;	movlw	0xC4
;	call	_DEBUG
;	movf	TBLPTRL, W
;	call	_DEBUG
;	movf	TBLPTRH, W
;	call	_DEBUG
;	movf	FSR2L, W
;	call	_DEBUG
;	movf	FSR2H, W
;	call	_DEBUG
;	goto	_STOP

dodo:
	movwf	TMP_REG_3
dodo_loop:
	decfsz	TMP_REG_1, F
	bra	dodo_loop
	decfsz	TMP_REG_2, F
	bra	dodo_loop
	decfsz	TMP_REG_3, F
	bra	dodo_loop
	return

caml_debug_start:
	bsf	INT_RUNS_1, 0
	return
	
caml_debug:
	btfss	INT_RUNS_1, 0
	return
	btfsc	INT_RUNS_1, 1
	bcf	INT_RUNS_1, 0
	bsf	INT_RUNS_1, 1
	clrf	TRISB
	setf	PORTB
	movlw	0x10
	rcall	dodo
	clrf	PORTB
	movlw	0x10
	rcall	dodo
	movff	ACCUL, PORTB
	movlw	0x80
	rcall	dodo
	movff	ACCUH, PORTB
	movlw	0x80
	rcall	dodo
	return

dodo:
	clrf	TMP_REG_A
	bsf	TMP_REG_A, 7
dodo_loop:
	decfsz	TMP_REG_B, F
	bra	dodo_loop
	decfsz	TMP_REG_C, F
	bra	dodo_loop
	decfsz	TMP_REG_A, F
	bra	dodo_loop
	return

caml_debug_l:
	incf	LATA, F
	incf	LATA, F
	rrcf	WREG, W
	btfsc	STATUS, C
	bsf	PORTA, 0
	btfss	STATUS, C
	bcf	PORTA, 0
	rcall	dodo
	return
	
caml_debug:
	clrf	TRISA
	clrf	PORTA
	rcall	caml_debug_l
	rcall	caml_debug_l
	rcall	caml_debug_l
	rcall	caml_debug_l

	rcall	caml_debug_l
	rcall	caml_debug_l
	rcall	caml_debug_l
	rcall	caml_debug_l
	return
	
;;;

_RS	EQU	RD2
_RW	EQU	RD1
_E	EQU	RD0

DEBUG1	EQU	TMP_REG_A
DEBUG2	EQU	TMP_REG_B
DEBUG3	EQU	TMP_REG_C
DEBUG4	EQU	TMP_REG_D
	
_SLEEP:
	movlw	0x10
	movwf	DEBUG2
_SLEEP_LOOP:
	decfsz	DEBUG1, F
	bra	_SLEEP_LOOP
	decfsz	DEBUG2, F
	bra	_SLEEP_LOOP
	return
	
_DODO:
	movlw	0x80
	movwf	DEBUG3
_DODO_LOOP:
	decfsz	DEBUG1, F
	bra	_DODO_LOOP
	decfsz	DEBUG2, F
	bra	_DODO_LOOP
	decfsz	DEBUG3, F
	bra	_DODO_LOOP
	return
	
_SEND:
	bcf	LATD, _RW
	clrf	TRISC
	movwf	PORTC
	rcall	_SLEEP
	bsf	LATD, _E
	rcall	_SLEEP
	bcf	LATD, _E
	rcall	_SLEEP
	return

_SEND_DATA:
	bsf	LATD, _RS
	bra	_SEND

_SEND_HEX:
	addlw	(-0x0A)
	bc	_SEND_HEX_L0
	addlw	(0x0A + '0')
	bra	_SEND_DATA
_SEND_HEX_L0:
	addlw	'A'
	bra	_SEND_DATA

_DEBUG:
	movwf	DEBUG4
	movlw	' '
	rcall	_SEND_DATA
	swapf	DEBUG4, W
	andlw	0x0F
	rcall	_SEND_HEX
	movf	DEBUG4, W
	andlw	0x0F
	bra	_SEND_HEX
