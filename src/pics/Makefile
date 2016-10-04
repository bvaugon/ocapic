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

SRCS = $(SELECTED_PICS:=.reg)
TARGS = $(addprefix $(LIB)/,$(SRCS:.reg=/pic.ml))
HEADER_DIR = /usr/share/gputils/header
PIC_INCS = $(wildcard $(HEADER_DIR)/p18f*.inc)
PIC_REGS = $(PIC_INCS:$(HEADER_DIR)/p%.inc=pic%.reg)

pics: $(TARGS)

$(LIB)/%/pic.ml: pic%.reg
	mkdir -p $(LIB)/$*
	$(BIN)/rtgen < $< > $@

regs: $(PIC_REGS)

pic%.reg: $(HEADER_DIR)/p%.inc
	$(BIN)/reggen < $< > $@

clean:
	@rm -f *~ $(TARGS)

.PHONY: pics regs clean
