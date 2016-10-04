###########################################################################
##                                                                       ##
##                                OCaPIC                                 ##
##                                                                       ##
##                             Benoit Vaugon                             ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../LICENSE-en.                                            ##
##                                                                       ##
###########################################################################

DIRS = arith mygc stdgc goblet hw1 hw2 lazies objs plat htbl int3264	\
prems osolilet format interrupt floats serial1 serial2 serial3 timer	\
seg7 eeprom led mask pprog chargen analog charger thermostat1           \
thermostat2 thermostat3 mac keyboard radio

all: clean run

clean:
	@rm -f *~
	@for d in $(DIRS); do \
	  make --no-print-directory -C $$d clean; \
	  if [ $$? -ne 0 ]; then exit 1; fi; \
	done

run:
	@for d in $(DIRS); do \
	  echo "*** $$d ***"; \
	  make --no-print-directory -C $$d; \
	  if [ $$? -ne 0 ]; then exit 1; fi; \
	done

.PHONY: all clean run
