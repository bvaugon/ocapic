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

FLAGS = -Wall -O
SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)

all: test

test: $(OBJS)
	gcc $(FLAGS) $(OBJS) -o test

%.o: %.c
	gcc $(FLAGS) -c $<

clean:
	@rm -f *~ *.o test

.PHONY: all clean
