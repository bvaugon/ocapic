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

OBJS = display.cmo stacks.cmo grid.cmo ia.cmo goblet.cmo
PIC = 18f4620
GC = STOP_AND_COPY

ifeq ($(GC),STOP_AND_COPY)
HEAP_SIZE = 6
STACK_SIZE = 3
else
HEAP_SIZE = 12
STACK_SIZE = 2
endif

all: goblet.hex

goblet.cmo: ia.cmi display.cmi
ia.cmo: grid.cmi stacks.cmi types.cmi display.cmi
grid.cmo: types.cmi
stacks.cmo: types.cmi
display.cmo: types.cmi

goblet.hex: goblet.asm
	gpasm -y goblet.asm

goblet goblet.asm: $(OBJS)
	$(OCAPIC) $(PIC) -gc $(GC) -heap-size $(HEAP_SIZE) \
	  -stack-size $(STACK_SIZE) $(OBJS) $$($(OCAPIC) -where)/default-config.asm

%.cmo: %.ml
	$(OCAPIC) $(PIC) -c $<

%.cmi: %.mli
	$(OCAPIC) $(PIC) -c $<

simul1: goblet
	./goblet -slow 'ocapic_lcd_simulator 8x2 e=RA0 rs=RA1 rw=RA3 bus=PORTB'\
	  ocapic_goblet_simulator ocapic_dip40_simulator

simul2: goblet.hex
	ocasim goblet.hex -slow \
	  'ocapic_lcd_simulator 8x2 e=RA0 rs=RA1 rw=RA3 bus=PORTB' \
	  ocapic_goblet_simulator ocapic_dip40_simulator

prog: goblet.hex
	picprog goblet.hex

clean:
	@rm -f *~ *.cmo *.cmi *.o *.hex *.lst *.cod goblet goblet.asm

.PHONY: all simul1 simul2 prog clean
