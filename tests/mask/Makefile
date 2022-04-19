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

OCAMLFLAGS = -w @a-4-70 -warn-error A -safe-string -strict-formats -strict-sequence
OCAMLC = $(shell $(OCAPIC) -ocamlc)
BASE = mask
PIC = 18f4620

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml exts.o exts.asm
	$(OCAPIC) $(PIC) $(BASE).ml $$($(OCAPIC) -where)/default-config.asm \
	  exts.o exts.asm

%.o: %.c
	$(OCAMLC) $(OCAMLFLAGS) -c $<

simul1: $(BASE)
	./$(BASE) ocapic_dip40_simulator

simul2: $(BASE).hex
	ocasim $(BASE).hex ocapic_dip40_simulator

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog clean
