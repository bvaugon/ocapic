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

include ../../etc/Makefile.conf

TARG = $(BIN)/ocapic
SRCS = config.ml ocapic.ml
CMXS = $(SRCS:.ml=.cmx)

ocapic: $(TARG)

$(TARG): $(CMXS)
	$(OCAMLOPT) $(CMXS) -o $@

config.ml: $(ETC)/config.ml
	cp $< $@

ocapic.cmx: config.cmx

%.cmx: %.ml
	$(OCAMLOPT) -c $<

clean:
	@rm -f *~ *.o *.cmi *.cmx config.ml $(TARG)

.PHONY: ocapic clean
