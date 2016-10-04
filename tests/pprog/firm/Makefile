###########################################################################
##                                                                       ##
##                                OCaPIC                                 ##
##                                                                       ##
##                             Benoit Vaugon                             ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../../../LICENSE-en.                                      ##
##                                                                       ##
###########################################################################

include ../../../etc/TestsMakefile.conf

BASE = firm
TARGS = $(BASE) $(BASE).asm $(BASE).hex

all: $(TARGS)

$(BASE) $(BASE).asm: $(BASE).ml
	$(OCAPIC) 18f4620 $(BASE).ml ../../../src/asm/config.asm

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *.cmo *.cmi *.cmx *.o *.hex *.lst *.cod $(TARGS)

.PHONY: all prog clean
