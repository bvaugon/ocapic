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

BASE = htbl
PIC = 18f4620

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) $(BASE).ml $$($(OCAPIC) -where)/default-config.asm

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all prog clean
