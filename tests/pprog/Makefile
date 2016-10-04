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

all: soft firm

soft:
	@make --no-print-directory -C soft

firm:
	@make --no-print-directory -C firm

top:
	@make --no-print-directory -C soft top

prog:
	@make --no-print-directory -C firm prog

clean:
	@make --no-print-directory -C soft clean
	@make --no-print-directory -C firm clean

.PHONY: all soft firm top prog clean
