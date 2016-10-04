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

BASE = timer
PIC = 18f4620
GC = STOP_AND_COPY

ifeq ($(GC),STOP_AND_COPY)
HEAP_SIZE = 6
STACK_SIZE = 3
else
HEAP_SIZE = 12
STACK_SIZE = 2
endif

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) -gc $(GC) -heap-size $(HEAP_SIZE) \
	  -stack-size $(STACK_SIZE) $(BASE).ml \
	   $$($(OCAPIC) -where)/default-config.asm

simul1: $(BASE)
	./$(BASE) 'ocapic_circuit_simulator circuit.txt'

simul2: $(BASE).hex
	ocasim $(BASE).hex 'ocapic_circuit_simulator circuit.txt'

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.cmx *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog clean
