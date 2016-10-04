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

include ../../../etc/Makefile.conf

EXTRALIB = $(LIB)/extra
OCAML = $(shell dirname $(OCAMLC_UNSAFE))/ocaml
TMLS = hex.ml mem.ml com.ml
TCMOS = $(TMLS:.ml=.cmo)
PROG = pprog
TARGS = $(TCMOS) $(PROG)

all: $(TARGS)

%.cmi %.cmo: %.ml
	$(OCAMLC) -I $(EXTRALIB) -c $<

mem.cmi mem.cmo: hex.cmi
com.cmi com.cmo: hex.cmi mem.cmi
soft.cmi soft.cmo: com.cmi

$(PROG): $(TCMOS) soft.cmo
	$(OCAMLC) -I +threads -I $(EXTRALIB) unix.cma threads.cma serial.cmo \
	  $^ -o $@

top: $(TCMOS)
	rlwrap $(OCAML) -I +threads -I $(EXTRALIB) unix.cma threads.cma \
	  serial.cmo $(TCMOS)

clean:
	@rm -f *.cmo *.cmi *.cmx *.o $(TARGS)

.PHONY: all simul top clean
