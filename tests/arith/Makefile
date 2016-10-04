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

TARGETS = mul div mod

all: $(TARGETS)

%: %.ml
	ocamlopt -w Ae -warn-error A -safe-string -strict-formats -strict-sequence $< -o $@

clean:
	@rm -f *~ *.cmx *.cmi *.o $(TARGETS)

.PHONY: all clean
