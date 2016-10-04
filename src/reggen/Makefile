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

reggen: $(BIN)/reggen

$(BIN)/reggen: reggen.ml
	$(OCAMLOPT) $< -o $@

clean:
	@rm -f *~ *.o *.cmi *.cmx $(BIN)/reggen

.PHONY: reggen clean
