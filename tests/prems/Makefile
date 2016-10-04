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

BASE = prems
PIC = 18f4620
OCAMLC = $(shell $(OCAPIC) -ocamlc)

all: $(BASE).hex cprems

$(BASE) $(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) $(BASE).ml $$($(OCAPIC) -where)/default-config.asm

simul1: $(BASE)
	./$(BASE) -slow ocapic_dip40_simulator

simul2: $(BASE).hex
	ocasim $(BASE).hex -slow ocapic_dip40_simulator

prog: $(BASE).hex
	picprog $(BASE).hex

cprems: cprems.ml
	$(OCAMLC) $< -o $@

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE) cprems

.PHONY: all simul1 simul2 prog clean
