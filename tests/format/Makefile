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

BASE = format
PIC = 18f4620
CONFIG = $$($(OCAPIC) -where)/default-config.asm
GC = MARK_AND_COMPACT

ifeq ($(GC),STOP_AND_COPY)
HEAP_SIZE = 6
STACK_SIZE = 3
else
HEAP_SIZE = 11
STACK_SIZE = 3
endif

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) -gc $(GC) -heap-size $(HEAP_SIZE) \
	  -stack-size $(STACK_SIZE) $(BASE).ml $(CONFIG)

simul1: $(BASE)
	./$(BASE) 'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTC'

simul2: $(BASE).hex
	ocasim $(BASE).hex \
	  'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTC'

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog clean
