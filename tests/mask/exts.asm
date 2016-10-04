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

mask_portb:
	rrcf	ACCUH, W	; PORTB <- Long_val(ACCU)
	rrcf	ACCUL, W
	movwf	PORTB
	rrcf	[0x2], W	; PORTB <- PORTB ^ STACK[0]
	rrcf	[0x1], W
	xorwf	PORTB, F
	clrf	ACCUH		; return ()
	movlw	0x1
	movwf	ACCUL
	return
