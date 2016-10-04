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

BASE = lazies
SRCS = $(wildcard *.ml)
OBJS = $(SRCS:.ml=.cmo)
PIC = 18f4620

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(OBJS)
	$(OCAPIC) $(PIC) $(OBJS) $$($(OCAPIC) -where)/default-config.asm

%.cmo: %.ml
	$(OCAPIC) $(PIC) -c $<

lazies.cmo: display.cmo

simul1: $(BASE)
	./$(BASE) ocapic_dip40_simulator \
	  'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

simul2: $(BASE).hex
	ocasim $(BASE).hex ocapic_dip40_simulator \
	  'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog clean
