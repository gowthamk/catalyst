## Copyright (C) 2009,2011,2013 Matthew Fluet.
 # Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := $(shell pwd)
BUILD := $(ROOT)/build
SRC := $(ROOT)
BIN := $(BUILD)/bin
LIB := $(BUILD)/lib
INC := $(LIB)/include
COMP := $(SRC)/mlton
RUN := $(SRC)/runtime
MLTON := $(BIN)/mlton
AOUT := mlton-compile
ifeq (mingw, $(TARGET_OS))
EXE := .exe
else
EXE :=
endif
MLBPATHMAP := $(LIB)/mlb-path-map
SPEC := package/rpm/mlton.spec
LEX := mllex
PROF := mlprof
YACC := mlyacc
NLFFIGEN := mlnlffigen
PATH := $(BIN):$(SRC)/bin:$(shell echo $$PATH)
CP := /bin/cp -fpR
GZIP := gzip --force --best
RANLIB := ranlib

# If we're compiling with another version of MLton, then we want to do
# another round of compilation so that we get a MLton built without
# stubs.
ifeq (other, $(shell if [ ! -x "$(BIN)/mlton" ]; then echo other; fi))
	BOOTSTRAP_OTHER:=true
else
	BOOTSTRAP_OTHER:=false
endif

ifeq ($(origin VERSION), undefined)
	VERSION := $(shell date +%Y%m%d)
endif
ifeq ($(origin RELEASE), undefined)
	RELEASE := 1
endif

.PHONY: all
all:
	$(MAKE) all-no-docs

.PHONY: all-no-docs
all-no-docs:
	$(MAKE) dirs compiler basis-no-check
# Remove $(AOUT) so that the $(MAKE) compiler below will remake MLton.
# We also want to re-run the just-built tools (mllex and mlyacc)
# because they may be better than those that were used for the first
# round of compilation.  So, we clean out the front end.
$(MAKE) compiler basis
@echo 'Build of MLton succeeded.'

.PHONY: basis-no-check
basis-no-check:
	mkdir -p "$(LIB)/sml"
	rm -rf "$(LIB)/sml/basis"
	$(CP) "$(SRC)/basis-library/." "$(LIB)/sml/basis"
	find "$(LIB)/sml/basis" -name .gitignore | xargs rm -rf

.PHONY: basis
basis:
	$(MAKE) basis-no-check
	@echo 'Type checking basis.'
	"$(MLTON)" -disable-ann deadCode \
		-stop tc \
		'$$(SML_LIB)/basis/libs/all.mlb' \
		>/dev/null

.PHONY: clean
clean:
	bin/clean

.PHONY: clean-git
clean-git:
	find . -type d -name .git | xargs rm -rf

.PHONY: compiler
compiler:
	$(MAKE) -C "$(COMP)"
	$(CP) "$(COMP)/$(AOUT)$(EXE)" "$(LIB)/"

.PHONY: dirs
dirs:
	mkdir -p "$(BIN)" "$(INC)"
	mkdir -p "$(LIB)/targets/$(TARGET)/include"
	mkdir -p "$(LIB)/targets/$(TARGET)/sml"

LIBRARIES := ckit-lib cml mllpt-lib mlnlffi-lib mlrisc-lib mlyacc-lib smlnj-lib

.PHONY: mlbpathmap
mlbpathmap:
	touch "$(MLBPATHMAP)"
	( echo 'MLTON_ROOT $$(LIB_MLTON_DIR)/sml';	\
	  echo 'SML_LIB $$(LIB_MLTON_DIR)/sml'; )	\
		>>"$(MLBPATHMAP).tmp"
	mv "$(MLBPATHMAP).tmp" "$(MLBPATHMAP)"

.PHONY: version
version:
	@echo 'Instantiating version numbers.'
	for f in							\
		"$(SPEC)"						\
		package/freebsd/Makefile				\
		mlton/control/version_sml.src				\
		doc/guide/conf/asciidoc-mlton.flags			\
	; do								\
		if grep -q 'MLTONVERSION' "$$f"; then			\
			sed "s/\(.*\)MLTONVERSION\(.*\)/\1$(VERSION)\2/" <"$$f" >z && 	\
			mv z "$$f";							\
		fi;							\
	done
	if grep -q '^Release:$$' "$(SPEC)"; then			\
		sed <"$(SPEC)" >z "/^Release:/s;.*;Release: $(RELEASE);"; 		\
		mv z "$(SPEC)";								\
	fi

.PHONY: check
check:
	./bin/regression

# The TBIN and TLIB are where the files are going to be after installing.
# The DESTDIR and is added onto them to indicate where the Makefile actually
# puts them.
DESTDIR := $(CURDIR)/install
PREFIX := /usr
ifeq ($(findstring $(TARGET_OS), darwin freebsd solaris), $(TARGET_OS))
PREFIX := /usr/local
endif
ifeq ($(TARGET_OS), mingw)
PREFIX := /mingw
endif
prefix := $(PREFIX)
MAN_PREFIX_EXTRA :=
TBIN := $(DESTDIR)$(prefix)/bin
ULIB := lib/mlton
TLIB := $(DESTDIR)$(prefix)/$(ULIB)
TMAN := $(DESTDIR)$(prefix)$(MAN_PREFIX_EXTRA)/man/man1
TDOC := $(DESTDIR)$(prefix)/share/doc/mlton
ifeq ($(findstring $(TARGET_OS), solaris mingw), $(TARGET_OS))
TDOC := $(DESTDIR)$(prefix)/doc/mlton
endif
TEXM := $(TDOC)/examples

GZIP_MAN := true
ifeq ($(TARGET_OS), solaris)
GZIP_MAN := false
endif

.PHONY: install
install: install-no-strip install-strip

.PHONY: install-no-strip
install-no-strip: install-docs install-no-docs move-docs 

.PHONY: install-no-docs
install-no-docs:
	mkdir -p "$(TLIB)" "$(TBIN)" "$(TMAN)"
	$(CP) "$(LIB)/." "$(TLIB)/"
	sed "/^lib=/s;.*;lib='$(prefix)/$(ULIB)';"			\
		<"$(BIN)/mlton" >"$(TBIN)/mlton"
	chmod a+x "$(TBIN)/mlton"
	if [ -x "$(BIN)/mlton.trace" ]; then                            \
		sed "/^lib=/s;.*;lib='$(prefix)/$(ULIB)';"		\
			<"$(BIN)/mlton.trace" >"$(TBIN)/mlton.trace";   \
		chmod a+x "$(TBIN)/mlton.trace";                        \
	fi
	if [ -x "$(BIN)/mlton.debug" ]; then                            \
		sed "/^lib=/s;.*;lib='$(prefix)/$(ULIB)';"		\
			<"$(BIN)/mlton.debug" >"$(TBIN)/mlton.debug";   \
		chmod a+x "$(TBIN)/mlton.debug";                        \
	fi
	cd "$(BIN)" && $(CP) "$(LEX)$(EXE)" "$(NLFFIGEN)$(EXE)"		\
		 "$(PROF)$(EXE)" "$(YACC)$(EXE)" "$(TBIN)/"
	( cd "$(SRC)/man" && tar cf - $(MAN_PAGES)) | \
		( cd "$(TMAN)/" && tar xf - )
	if $(GZIP_MAN); then						\
		cd "$(TMAN)" && $(GZIP) $(MAN_PAGES);			\
	fi
