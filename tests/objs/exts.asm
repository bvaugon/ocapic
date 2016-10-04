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

_E	EQU	RA0
_RS	EQU	RA1
_RW	EQU	RA3

VARLOC1	EQU	TMP_REG_E
VARLOC2	EQU	TMP_REG_D
VARLOC3	EQU	TMP_REG_C
VARLOC4	EQU	TMP_REG_B
	
_SLEEP:
	movlw	0x1
	movwf	VARLOC2
_SLEEP_LOOP:
	decfsz	VARLOC1, F
	bra	_SLEEP_LOOP
	decfsz	VARLOC2, F
	bra	_SLEEP_LOOP
	return
	
_DODO:
	movlw	0x1
	movwf	VARLOC3
_DODO_LOOP:
	decfsz	VARLOC1, F
	bra	_DODO_LOOP
	decfsz	VARLOC2, F
	bra	_DODO_LOOP
	decfsz	VARLOC3, F
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
	rcall	_SEND_INSTR
	bra	_DODO

_HOME:
	movlw	0x2
	rcall	_SEND_INSTR
	bra	_DODO

_SEND_HEX:
	andlw	0x0F
	addlw	(-0x0A)
	bc	_SEND_HEX_L0
	addlw	(0x0A + '0')
	bra	_SEND_DATA
_SEND_HEX_L0:
	addlw	'A'
	bra	_SEND_DATA

init_aff:
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

print_int:
	rcall	_CLEAR
	rcall	_HOME
	movlw	'0'
	rcall	_SEND_DATA
	movlw	'x'
	rcall	_SEND_DATA
	bcf	STATUS, C
	rrcf	ACCUH, F
	rrcf	ACCUL, F
	swapf	ACCUH, W
	rcall	_SEND_HEX
	movf	ACCUH, W
	rcall	_SEND_HEX
	swapf	ACCUL, W
	rcall	_SEND_HEX
	movf	ACCUL, W
	rcall	_SEND_HEX
	movlw	0x1
	movwf	ACCUL
	clrf	ACCUH
	return

print_string:
	rcall	_CLEAR
	rcall	_HOME
	movff	ACCUL, FSR0L
	movff	ACCUH, FSR0H
print_string_loop:
	movf	POSTINC0, W
	bz	print_string_end
	rcall	_SEND_DATA
	bra	print_string_loop
print_string_end:
	movlw	0x1
	movwf	ACCUL
	clrf	ACCUH
	return

_debug_dodo:
	movlw	0x40
	movwf	VARLOC3
_debug_dodo_loop:
	decfsz	VARLOC1, F
	bra	_debug_dodo_loop
	decfsz	VARLOC2, F
	bra	_debug_dodo_loop
	decfsz	VARLOC3, F
	bra	_debug_dodo_loop
	return
	
debug_print:
	movwf	VARLOC4
	rcall	_CLEAR
	rcall	_HOME
	movlw	'D'
	rcall	_SEND_DATA
	rcall	_debug_dodo
	movlw	' '
	rcall	_SEND_DATA
	swapf	VARLOC4, W
	rcall	_SEND_HEX
	movf	VARLOC4, W
	rcall	_SEND_HEX
	rcall	_debug_dodo
	rcall	_debug_dodo
	return

debug:
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	movf	POSTINC0, W
	rcall	debug_print
	subfsr	0, .20
	return
