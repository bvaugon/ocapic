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

BASE = plat
SRCS = $(wildcard *.ml)
OBJS = $(SRCS:.ml=.cmo)
PIC = 18f4620

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE).asm: $(OBJS)
	$(OCAPIC) $(PIC) $(OBJS) $$($(OCAPIC) -where)/default-config.asm -o plat.asm

%.cmo: %.ml
	$(OCAPIC) $(PIC) -c $<

plat.cmo: display.cmo

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all prog clean
