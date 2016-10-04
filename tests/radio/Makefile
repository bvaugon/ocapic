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

PIC = 18f4620

all: transmitter.hex receiver.hex

%.hex: %.asm
	gpasm -y $<

%.asm: %.ml
	$(OCAPIC) $(PIC) $< $$($(OCAPIC) -where)/default-config.asm

prog-%: %.hex
	picprog $<

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst *.asm transmitter receiver

.PHONY: all clean
