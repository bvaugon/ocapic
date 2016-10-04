###########################################################################
##                                                                       ##
##                                OCaPIC                                 ##
##                                                                       ##
##                             Benoit Vaugon                             ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../../LICENSE-en.                                         ##
##                                                                       ##
###########################################################################

include ../../etc/TestsMakefile.conf

OCAMLFLAGS = -w Ae -warn-error A -safe-string -strict-formats -strict-sequence
OCAMLC = $(shell $(OCAPIC) -ocamlc)
BASE = objs
PIC = 18f4620

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: objs.cmo exts.o exts.asm
	$(OCAPIC) $(PIC) -stack-size 3 -heap-size 11 -gc MARK_AND_COMPACT \
	  exts.o objs.cmo $$($(OCAPIC) -where)/default-config.asm exts.asm

%.o: %.c
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmo: %.ml
	$(OCAPIC) $(PIC) -c $<

simul1: $(BASE)
	./$(BASE) 'ocapic_lcd_simulator 30x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

simul2: $(BASE).hex
	ocasim $(BASE).hex \
	  'ocapic_lcd_simulator 30x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog clean
