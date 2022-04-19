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
EXTRALIB = $(shell $(OCAPIC) -where)/extra
OCAMLC = $(shell $(OCAPIC) -ocamlc)
OCAML = $(shell $(OCAPIC) -ocaml)
BASE = usart
PIC = 18f4620

all: $(BASE).hex sender.cmo simul1.cmo simul2.cmo

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml
	$(OCAPIC) $(PIC) -stack-size 3 -heap-size 6 $(BASE).ml \
	  $$($(OCAPIC) -where)/default-config.asm

prog: $(BASE).hex
	picprog $(BASE).hex

simul1: simul1.cmo $(BASE) $(BASE).hex
	rlwrap $(OCAML) -I +threads -I $(EXTRALIB) unix.cma threads.cma \
	  serial.cmo simul1.cmo

simul2: simul2.cmo $(BASE) $(BASE).hex
	rlwrap $(OCAML) -I +threads -I $(EXTRALIB) unix.cma threads.cma \
	  serial.cmo simul2.cmo

test: sender.cmo
	rlwrap $(OCAML) -I +threads -I $(EXTRALIB) unix.cma threads.cma \
	  serial.cmo sender.cmo

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -I $(EXTRALIB) -c $<

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog test clean
