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

BASE = floats
PIC = 18f4620
CONFIG = $$($(OCAPIC) -where)/default-config.asm

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) -stack-size 3 -heap-size 11 -gc MARK_AND_COMPACT $(BASE).ml $(CONFIG)

simul1: $(BASE)
	./$(BASE) \
	  'ocapic_lcd_simulator 16x1 e=RD0 rs=RD2 rw=RD1 bus=PORTC'

simul2: $(BASE).hex
	ocasim $(BASE).hex \
	  'ocapic_lcd_simulator 16x1 e=RD0 rs=RD2 rw=RD1 bus=PORTC'

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog clean
