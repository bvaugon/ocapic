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

rtgen: $(BIN)/rtgen

$(BIN)/rtgen: rtgen.ml
	$(OCAMLOPT) $< -o $@

clean:
	@rm -f *~ *.o *.cmi *.cmx $(BIN)/rtgen

.PHONY: rtgen clean
