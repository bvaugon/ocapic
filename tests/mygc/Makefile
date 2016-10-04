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

all: test

test: gc.o test.o
	gcc gc.o test.o -o test

%.o: %.c
	gcc -c -Wall $<

clean:
	@rm -f *~ *.o test

.PHONY: all clean
