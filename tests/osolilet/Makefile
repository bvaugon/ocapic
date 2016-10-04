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

TARGS = cosolilet osolilet.hex
PIC = 18f4620
OCAMLC = $(shell $(OCAPIC) -ocamlc)

all: $(TARGS)

%: %.ml
	$(OCAMLC) $< -o $@

osolilet osolilet.asm: osolilet.ml
	$(OCAPIC) $(PIC) -stack-size 7 -heap-size 4 $< \
	  $$($(OCAPIC) -where)/default-config.asm

osolilet.hex: osolilet.asm
	gpasm -y $<

simul1: osolilet
	./osolilet -slow ocapic_dip40_simulator

simul2: osolilet.hex
	ocasim osolilet.hex -slow ocapic_dip40_simulator

prog: osolilet.hex
	picprog osolilet.hex

clean:
	@rm -f *~ *.cmo *.cmo *.cmi *.o *.asm *.log *.cod *.lst *.hex
	@rm -f osolilet cosolilet

.PRESERVE: osolilet.asm

.PHONY: all simul1 simul2 prog clean
