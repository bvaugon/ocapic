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

include ../etc/Makefile.conf

# IGNORED: callback digest filename format lexing marshal parsing printexc
#          scanf weak ephemeron arg complex

STD_BASES := \
  camlinternalFormatBasics stdlib                                   \
  stdlib__arrayLabels stdlib__array stdlib__char stdlib__list       \
  stdlib__bytesLabels stdlib__bytes stdlib__obj camlinternalLazy    \
  camlinternalMod stdlib__sys stdlib__map camlinternalOO            \
  stdlib__eeprom stdlib__gc stdlib__genlex stdlib__string           \
  stdlib__int32 stdlib__int64 stdlib__random stdlib__hashtbl        \
  stdlib__lazy stdlib__listLabels stdlib__moreLabels stdlib__nap    \
  stdlib__nativeint stdlib__oo stdlib__queue stdlib__serial         \
  stdlib__set stdlib__sort stdlib__stack std_exit stdlib__stdLabels \
  stdlib__stream stdlib__stringLabels stdlib__uchar stdlib__buffer  \
  camlinternalFormat stdlib__printf

SPIC_BASES := lcd

OBJS := shared.o sf_regs.o simu.o cserial.o ceeprom.o stds.o

ARCS := stdlib.cma libcamlrun.a

SPIC_DIR := spiclibs

PICS := $(SELECTED_PICS:=/pic)
PICS_MLS := $(PICS:=.ml)
PICS_CMOS := $(PICS:=.cmo)
PICS_CMIS := $(PICS:=.cmi)
PICLIBS_CMAS := $(PICS:=lib.cma)

SPIC_PATHS := $(foreach pic,$(SELECTED_PICS),$(addprefix $(pic)/,$(SPIC_BASES)))
SPIC_MLS := $(SPIC_PATHS:=.ml)
SPIC_MLIS := $(SPIC_PATHS:=.mli)
SPIC_CMIS := $(SPIC_PATHS:=.cmi)
SPIC_CMOS := $(SPIC_PATHS:=.cmo)
SPIC_MANS := $(foreach base, $(SPIC_BASES), \
	       $(addprefix $(ONE_PIC)/, $(call capitalize, $(base)).3o))

STD_CMIS := $(STD_BASES:=.cmi)
STD_CMOS := $(STD_BASES:=.cmo)
STD_MANS := $(foreach base, $(STD_BASES), $(call capitalize, $(call rmprefix,stdlib__,$(base))).3o)

ALL_MANS := $(SPIC_MANS) $(STD_MANS) Stdlib.3o

TARGETS := $(STD_CMIS) $(STD_CMOS) $(PICS_CMIS) $(PICS_CMOS) $(PICLIBS_CMAS) \
	   $(SPIC_CMIS) $(SPIC_CMOS) $(ALL_MANS) $(ARCS)

lib: $(TARGETS)

##

camlinternalFormatBasics.cmo: camlinternalFormatBasics.ml camlinternalFormatBasics.cmi
	$(OCAMLC) -nostdlib -nopervasives -c $<

camlinternalFormatBasics.cmi: camlinternalFormatBasics.mli
	$(OCAMLC) -nostdlib -nopervasives -c $<

stdlib.cmo: stdlib.ml stdlib.cmi
	$(OCAMLC) -absname -no-alias-deps -nostdlib -nopervasives -c -w -49 $<

stdlib.cmi: stdlib.mli camlinternalFormatBasics.cmi
	$(OCAMLC) -absname -no-alias-deps -nostdlib -nopervasives -c -w -49 $<

stdlib.cma: $(STD_CMOS)
	$(OCAMLC) -nostdlib -a $(STD_CMOS) -o stdlib.cma

libcamlrun.a: $(OBJS)
	cp $(OCAMLSTDLIB)/libcamlrun.a ./
	ar rs libcamlrun.a $(OBJS)

##

$(SELECTED_PICS:=/lcd.ml): $(SPIC_DIR)/lcd.ml
	cp $< $@

$(SELECTED_PICS:=/lcd.mli): $(SPIC_DIR)/lcd.mli
	cp $< $@

%/lcd.cmo: %/lcd.ml %/lcd.cmi
	$(OCAMLC_UNSAFE) -nostdlib -I $* -c $< -o $@

%/lcd.cmi: %/lcd.mli %/pic.cmi stdlib.cmi stdlib__string.cmi stdlib__obj.cmi
	$(OCAMLC_UNSAFE) -nostdlib -I $* -c $<

%/pic.cmo: %/pic.ml stdlib.cmi
	$(OCAMLC_UNSAFE) -nostdlib -c $<

%/pic.cmi: %/pic.ml stdlib.cmi
	$(OCAMLC_UNSAFE) -nostdlib -c $<

%/piclib.cma: %/pic.cmo $(addprefix %/, $(SPIC_BASES:=.cmo))
	$(OCAMLC_UNSAFE) -a $^ -o $@

stdlib__%Labels.cmo: %Labels.ml stdlib__%Labels.cmi
	$(OCAMLC_UNSAFE) -nostdlib -nolabels -c -w -3 $< -o $@

stdlib__%Labels.cmi: %Labels.mli
	$(OCAMLC_UNSAFE) -nostdlib -nolabels -c $< -o $@

stdlib__%.cmo: %.ml stdlib__%.cmi
	$(OCAMLC_UNSAFE) -absname -nostdlib -c $< -o $@

stdlib__%.cmi: %.mli
	$(OCAMLC_UNSAFE) -absname -nostdlib -c $< -o $@

stdlib__genlex.cmi: stdlib__stream.cmi
stdlib__moreLabels.cmi: stdlib__hashtbl.cmi stdlib__map.cmi stdlib__set.cmi
stdlib__oo.cmi: camlinternalOO.cmi
stdlib__lazy.cmi: camlinternalLazy.cmi
stdlib__printf.cmi: camlinternalFormat.cmi
stdlib__arrayLabels.cmi: stdlib__array.cmi
camlinternalMod.cmi: stdlib__obj.cmi
camlinternalFormat.cmi: stdlib__buffer.cmi

camlinternal%.cmi: camlinternal%.mli
	$(OCAMLC_UNSAFE) -absname -nostdlib -c $<

camlinternal%.cmo: camlinternal%.ml camlinternal%.cmi
	$(OCAMLC_UNSAFE) -absname -nostdlib -c $<

std_exit.cmi: std_exit.mli
	$(OCAMLC_UNSAFE) -absname -nostdlib -c $<

std_exit.cmo: std_exit.ml
	$(OCAMLC_UNSAFE) -absname -nostdlib -c $<

%.o: %.c
	$(CC) -Wall --warn-error -O3 -c $< -o $@

$(ONE_PIC)/%.3o:
	cd $(ONE_PIC); $(OCAMLDOC) -hide-warnings -man -man-mini	\
	  -I ../ $(call uncapitalize, $*).mli

%.3o:
	$(OCAMLDOC) -hide-warnings -man -man-mini $(call uncapitalize, $*).mli

##

depend:
	ocamldep *.ml *.mli > .depend

##

clean:
	@rm -f *~ *.cmi *.cmo *.cma *.o *.3o */*.3o libcamlrun.a
	@rm -f $(PICS_MLS) $(PICS_CMOS) $(PICS_CMIS)
	@for p in $(ALL_PICS); do \
	  if [ -d $$p ]; then     \
	    rm -f $$p/*;          \
	    rmdir $$p;            \
	  fi;                     \
	done

.PHONY: lib depend clean
