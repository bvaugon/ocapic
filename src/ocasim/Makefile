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

TARG = $(BIN)/ocasim
SRCS = $(wildcard *.ml)
BUILD = main.native
CSRCS = simu.c shared.c sf_regs.c ceeprom.c
CPATHS = $(foreach f, $(CSRCS), $(SRC)/simulator/$(f))
COPTS = $(foreach f, $(CPATHS), -lflag $(f))

ocasim: $(TARG)

$(TARG): _build/$(BUILD)
	cp $< $@

_build/$(BUILD): $(SRCS) $(CPATHS)
	@rm -f $@
	$(OCAMLBUILD) -cflags -thread,-g -lflags	  \
	  -thread,unix.cmxa,threads.cmxa,-g $(COPTS) $(BUILD)

test: $(TARG)
	$(TARG) ../../tests/int3264/int3264.hex 'ocapic_dip40_simulator' \
	  'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

clean:
	@$(OCAMLBUILD) -clean
	@rm -f *~ $(TARG)

.PHONY: ocasim clean
