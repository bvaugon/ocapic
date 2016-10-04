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

BASE = thermostat
PIC = 18f4620

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) -stack-size 3 -heap-size 11 -gc MARK_AND_COMPACT $(BASE).ml $$($(OCAPIC) -where)/default-config.asm

simul1: $(BASE)
	./$(BASE) 'ocapic_circuit_simulator circuit.txt'

simul2: $(BASE).hex
	ocasim $(BASE).hex 'ocapic_circuit_simulator circuit.txt'

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE) \
	   eeprom.data

.PHONY: all simul1 simul2 prog clean
