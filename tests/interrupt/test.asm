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

        processor 18f4620

        include "p18f4620.inc"
	include "../../asm/config.asm"

SLEEP0	EQU	0xF79
SLEEP1	EQU	0xF78
DODO0	EQU	0xF77
DODO1	EQU	0xF76
DODO2	EQU	0xF75
TMP1	EQU	0xF74
	
BUS	EQU	PORTC
TBUS	EQU	TRISC
CONT	EQU	PORTD
TCONT	EQU	TRISD

RS	EQU	2
RW	EQU	1
E	EQU	0

	bra	_MAIN
	org	0x8
interrupt_low:
	rcall	_CLEAR
	rcall	_HOME
	movlw	'$'
	rcall	_SEND_DATA
	bcf	INTCON, INT0IF
	retfie	FAST

	org	0x18
interrupt_high:
	bra	interrupt_low
	
_MAIN:
	banksel	SLEEP0
	rcall	_INIT
	movlw	'a'
	movwf	TMP1
	bsf	INTCON, INT0IE
	bsf	INTCON, GIE

_LOOP:
	incf	TMP1, F
	movf	TMP1, W
	rcall	_DODO
	rcall	_SEND_DATA
	bra	_LOOP

	bra	_STOP

_DODO:
	clrf	DODO2
	bsf	DODO2, 5
_DODO_LOOP:
	decfsz	DODO0, F
	bra	_DODO_LOOP
	clrf	DODO0
	bsf	DODO0, 6
	decfsz	DODO1, F
	bra	_DODO_LOOP
	decfsz	DODO2, F
	bra	_DODO_LOOP
	return

_SLEEP:
	movlw	.4
	movwf	SLEEP1
_SLEEP_LOOP:
	decfsz	SLEEP0, F
	bra	_SLEEP_LOOP
	decfsz	SLEEP1, F
	bra	_SLEEP_LOOP
	return

_SEND:
	bcf	CONT, RW
	movwf	BUS
	rcall	_SLEEP
	bsf	CONT, E
	rcall	_SLEEP
	bcf	CONT, E
	return

_SEND_INSTR:
	bcf	CONT, RS
	bra	_SEND

_SEND_DATA:
	bsf	CONT, RS
	bra	_SEND

_CLEAR:
	movlw	B'1'
	bra	_SEND_INSTR

_HOME:
	movlw	B'10'
	bra	_SEND_INSTR

_INIT:
	clrf	TBUS
	clrf	TCONT
	movlw	B'110'		; cursor right
	rcall	_SEND_INSTR
	movlw	B'1100'		; display ON ; cursor invisible
	rcall	_SEND_INSTR
	movlw	B'111000'	; bus 8 ; 2 lines ; 5x8
	rcall	_SEND_INSTR
	rcall	_CLEAR
	bra	_HOME

_STOP:
	bra	_STOP

	END
