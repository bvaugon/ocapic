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

BASE = serial
PIC = 18f4620
OCAMLC = $(shell $(OCAPIC) -ocamlc)

all: $(BASE).hex sender

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) $(BASE).ml $$($(OCAPIC) -where)/default-config.asm

simul1: $(BASE)
	./$(BASE) ocapic_dip40_simulator \
	  'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

simul2: $(BASE).hex
	ocasim $(BASE).hex ocapic_dip40_simulator \
	  'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

prog: $(BASE).hex
	picprog $(BASE).hex

%: %.ml
	$(OCAMLC) $< -o $@

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE) sender

.PHONY: all simul1 simul2 prog clean
