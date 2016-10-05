###########################################################################
##                                                                       ##
##                                OCaPIC                                 ##
##                                                                       ##
##                             Benoit Vaugon                             ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file LICENSE-en.                                               ##
##                                                                       ##
###########################################################################

include etc/Makefile.conf

MAN_3P_BASES = Int64 MoreLabels Lazy MoreLabels.Hashtbl.S Map.S		\
MoreLabels.Set.OrderedType StdLabels.String Random StdLabels Obj	\
CamlinternalOO Set StdLabels.List Oo Eeprom Lcd.S Lcd.Connect Lcd	\
ListLabels MoreLabels.Set.Make MoreLabels.Hashtbl.HashedType Queue	\
Hashtbl.Make MoreLabels.Hashtbl.Make StringLabels Stack Map		\
StdLabels.Array MoreLabels.Set.S Char CamlinternalMod MoreLabels.Set	\
List Hashtbl.HashedType Sort Serial Gc MoreLabels.Map.OrderedType	\
Array Hashtbl.S Map.Make Printf MoreLabels.Map.S CamlinternalLazy	\
MoreLabels.Hashtbl Nap String Stream Hashtbl Set.S MoreLabels.Map	\
Set.OrderedType ArrayLabels Buffer Std_exit Set.Make			\
MoreLabels.Map.Make Int32 Map.OrderedType Sys Pic

MAN_3O_BASES = Serial Simul

all: config
	$(call compile, src/bc2asm)
	$(call compile, src/reggen)
	$(call compile, src/rtgen)
	$(call compile, src/pics)
	$(call compile, lib/extra)
	$(call compile, src/simulator)
	$(call compile, src/ocapic)
	$(call compile, src/ocasim)
	$(call compile, lib)
	@if [ $(INSTALL_OCAMLCLEAN) = true ]; then \
            cd $(OCAMLCLEAN_DIRECTORY)/;           \
            make --no-print-directory;             \
        fi

config:
	@if [ $(ETC)/Makefile.conf -ot VERSION -o                     \
             $(ETC)/Makefile.conf -ot configure ]; then               \
          echo 'Configuration files are not up to date.' 1>&2;        \
	  echo 'Please run `./configure` (with right options).' 1>&2; \
          exit 1;                                                     \
	fi

install: all
	mkdir -p "$(LIBDIR)"
	mkdir -p "$(BINDIR)"
	mkdir -p "$(MAN1DIR)"
	mkdir -p "$(MAN3DIR)"
	cp bin/bc2asm "$(BINDIR)/bc2asm"
	cp bin/ocasim "$(BINDIR)/ocasim"
	cp bin/ocapic "$(BINDIR)/ocapic"
	cp bin/ocapic_lcd_simulator "$(BINDIR)/ocapic_lcd_simulator"
	cp bin/ocapic_goblet_simulator "$(BINDIR)/ocapic_goblet_simulator"
	cp bin/ocapic_dip40_simulator "$(BINDIR)/ocapic_dip40_simulator"
	cp bin/ocapic_circuit_simulator "$(BINDIR)/ocapic_circuit_simulator"
	cp bin/ocapic_keyboard_simulator "$(BINDIR)/ocapic_keyboard_simulator"
	cp man/ocapic.1.gz "$(MAN1DIR)/ocapic.1.gz"
	cp man/bc2asm.1.gz "$(MAN1DIR)/bc2asm.1.gz"
	cp src/asm/interp.asm "$(LIBDIR)/interp.asm"
	cp src/asm/runtime.asm "$(LIBDIR)/runtime.asm"
	cp src/asm/stdlib.asm "$(LIBDIR)/stdlib.asm"
	cp src/asm/config.asm "$(LIBDIR)/default-config.asm"
	cp lib/lcd_cgrom.txt "$(LIBDIR)/lcd_cgrom.txt"
	cp lib/stdlib.cma "$(LIBDIR)/stdlib.cma"
	cp lib/libcamlrun.a "$(LIBDIR)/libcamlrun.a"
	cp lib/*.ml "$(LIBDIR)/"
	cp lib/*.mli "$(LIBDIR)/"
	cp lib/*.cmo "$(LIBDIR)/"
	cp lib/*.cmi "$(LIBDIR)/"
	cp man/Pic.3p "$(MAN3DIR)/"
	cp lib/extra/*.3o "$(MAN3DIR)/"
	@for f in lib/*.3o lib/$(ONE_PIC)/*.3o; do	\
	  cp $$f "$(MAN3DIR)/$$(basename $$f .3o).3p";	\
	done
	@mkdir -p "$(LIBDIR)/extra"
	@cd lib/extra ; cp *.ml *.mli *.cmo *.cmi *.cmx *.o "$(LIBDIR)/extra/"
	@for pic in $(SELECTED_PICS); do         \
	  mkdir -p "$(LIBDIR)/$$pic";            \
	  cp lib/$$pic/* "$(LIBDIR)/$$pic/";     \
	done
	@if [ $(INSTALL_OCAMLCLEAN) = true ]; then \
            cd $(OCAMLCLEAN_DIRECTORY)/;           \
            make --no-print-directory install;     \
        fi

uninstall:
	-rm -f "$(BINDIR)/bc2asm"
	-rm -f "$(BINDIR)/ocasim"
	-rm -f "$(BINDIR)/ocapic"
	-rm -f "$(BINDIR)/ocapic_lcd_simulator"
	-rm -f "$(BINDIR)/ocapic_goblet_simulator"
	-rm -f "$(BINDIR)/ocapic_dip40_simulator"
	-rm -f "$(BINDIR)/ocapic_circuit_simulator"
	-rm -f "$(MAN1DIR)/ocapic.1.gz"
	-rm -f "$(MAN1DIR)/bc2asm.1.gz"
	-rm -f "$(LIBDIR)/interp.asm"
	-rm -f "$(LIBDIR)/runtime.asm"
	-rm -f "$(LIBDIR)/stdlib.asm"
	-rm -f "$(LIBDIR)/default-config.asm"
	-rm -f "$(LIBDIR)/stdlib.cma"
	-rm -f "$(LIBDIR)/libcamlrun.a"
	-rm -f "$(LIBDIR)/lcd_cgrom.txt"
	-rm -f "$(LIBDIR)/opam.config"
	-rm -f "$(LIBDIR)/"*.ml
	-rm -f "$(LIBDIR)/"*.mli
	-rm -f "$(LIBDIR)/"*.cmi
	-rm -f "$(LIBDIR)/"*.cmo
	-rm -Rf "$(LIBDIR)/extra"
	@for mod in $(MAN_3P_BASES); do \
	  rm -f "$(MAN3DIR)/"$$mod.3p;	\
	done
	@for mod in $(MAN_3O_BASES); do \
	  rm -f "$(MAN3DIR)/"$$mod.3o;	\
	done
	@for pic in $(ALL_PICS); do         \
	  if [ -d "$(LIBDIR)"/$$pic ]; then \
	    rm -f "$(LIBDIR)"/$$pic/*;      \
	    rmdir "$(LIBDIR)"/$$pic;        \
	  fi;                               \
	done
	@if [ -d "$(LIBDIR)" ]; then rmdir "$(LIBDIR)"; fi
	@if [ $(INSTALL_OCAMLCLEAN) = true ]; then \
            cd $(OCAMLCLEAN_DIRECTORY)/;           \
            make --no-print-directory uninstall;   \
        fi

etc/Makefile.conf:
	@echo "You must run ./configure before" 1>&2
	@exit 1

dist: clean
	dist/distgen

tests: install
	@make --no-print-directory -C tests

clean:
	@rm -f *~ */*~ */*/*~
	$(call clean, tests)
	$(call clean, src/bc2asm)
	$(call clean, src/reggen)
	$(call clean, src/rtgen)
	$(call clean, src/pics)
	$(call clean, src/simulator)
	$(call clean, src/ocapic)
	$(call clean, src/ocasim)
	$(call clean, lib/extra)
	$(call clean, lib)
	@if [ $(INSTALL_OCAMLCLEAN) = true ]; then \
            cd $(OCAMLCLEAN_DIRECTORY)/;           \
            make --no-print-directory clean;       \
        fi

.PHONY: all config install uninstall dist tests clean
