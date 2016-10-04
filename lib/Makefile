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

STD_BASES := arrayLabels array char list bytesLabels bytes		\
	obj camlinternalLazy						\
	camlinternalMod sys map camlinternalOO eeprom			\
	gc genlex string int32 int64 random hashtbl lazy		\
	listLabels moreLabels nap nativeint				\
	oo queue							\
	serial set sort stack std_exit stdLabels stream stringLabels	\
	uchar buffer camlinternalFormat printf

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
STD_MANS := $(foreach base, $(STD_BASES), $(call capitalize, $(base)).3o)

ALL_MANS := $(SPIC_MANS) $(STD_MANS) Pervasives.3o

TARGETS := $(STD_CMIS) $(STD_CMOS) $(PICS_CMIS) $(PICS_CMOS) $(PICLIBS_CMAS) \
	   $(SPIC_CMIS) $(SPIC_CMOS) $(ALL_MANS) $(ARCS)

lib: $(TARGETS)

##

$(STD_CMOS) $(STD_CMIS): pervasives.cmi

##

camlinternalFormatBasics.cmo: camlinternalFormatBasics.ml camlinternalFormatBasics.cmi
	$(OCAMLC) -nostdlib -nopervasives -c $<

camlinternalFormatBasics.cmi: camlinternalFormatBasics.mli
	$(OCAMLC) -nostdlib -nopervasives -c $<

pervasives.cmo: pervasives.ml pervasives.cmi camlinternalFormatBasics.cmo camlinternalFormatBasics.cmi
	$(OCAMLC) -nostdlib -nopervasives -c $<

pervasives.cmi: pervasives.mli
	$(OCAMLC) -nostdlib -nopervasives -c $<

stdlib.cma: $(STD_CMOS) camlinternalFormatBasics.cmo pervasives.cmo
	$(OCAMLC) -nostdlib -a camlinternalFormatBasics.cmo pervasives.cmo $(STD_CMOS) -o stdlib.cma

libcamlrun.a: $(OBJS)
	cp $(OCAMLSTDLIB)/libcamlrun.a ./
	ar rs libcamlrun.a $(OBJS)

##

$(SELECTED_PICS:=/lcd.ml): $(SPIC_DIR)/lcd.ml
	cp $< $@

$(SELECTED_PICS:=/lcd.mli): $(SPIC_DIR)/lcd.mli
	cp $< $@

%/lcd.cmo: %/lcd.ml %/lcd.cmi %/pic.cmi pervasives.cmi
	$(OCAMLC_UNSAFE) -nostdlib -I $* -c $<

%/lcd.cmi: %/lcd.mli %/pic.cmi pervasives.cmi
	$(OCAMLC_UNSAFE) -nostdlib -I $* -c $<

%/pic.cmo: %/pic.ml pervasives.cmi
	$(OCAMLC_UNSAFE) -nostdlib -c $<

%/pic.cmi: %/pic.ml pervasives.cmi
	$(OCAMLC_UNSAFE) -nostdlib -c $<

%/piclib.cma: %/pic.cmo $(addprefix %/, $(SPIC_BASES:=.cmo))
	$(OCAMLC_UNSAFE) -a $^ -o $@

%Labels.cmo: %Labels.ml %Labels.cmi
	$(OCAMLC_UNSAFE) -nostdlib -nolabels -c $<

%Labels.cmi: %Labels.mli
	$(OCAMLC_UNSAFE) -nostdlib -nolabels -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC_UNSAFE) -nostdlib -c $<

%.cmi: %.mli
	$(OCAMLC_UNSAFE) -nostdlib -c $<

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

include .depend
