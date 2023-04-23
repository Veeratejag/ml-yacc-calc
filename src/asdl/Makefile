# Makefile for ASDL
#
# This code is part of the SML/NJ system (http://smlnj.org)
#
# COPYRIGHT (c) 2018 The Fellowship of SML/NJ
# All rights reserved.
#
# Makefile.  Generated from Makefile.in by configure.
#

SHELL =			/bin/bash


INSTALL =		/usr/bin/install -c

BUILD_DIRS =		src doc

INSTALL_DIR =		/workspaces/ml-yacc-calc/src
INSTALL_BINDIR =	$(PREFIX)/bin
INSTALL_HEAPDIR =	$(INSTALL_BINDIR)/.heap
INSTALL_WRAPPER =	/workspaces/ml-yacc-calc/src/asdl/bin/install-sml-wrapper.sh
SML =			/workspaces/ml-yacc-calc/src/bin/sml
ML_BUILD =		/workspaces/ml-yacc-calc/src/bin/ml-build

.PHONY:		help build local-install install \
		clean local-distclean local-devclean

help:
	@echo "This Makefile supports the following targets:"
	@echo "  help          -- print this message."
	@echo "  build         -- build asdlgen tool and ASDL libraries"
	@echo "  local-install -- install asdlgen tool, libraries, and headers in "
	@echo "                   /workspaces/ml-yacc-calc/src/asdl/{bin,lib,include}"
	@echo "  install       -- install asdlgen tool, libraries, and headers in "
	@echo "                   /workspaces/ml-yacc-calc/src/{bin,lib,include}"
	@echo "  clean         -- remove intermediate files generated during building"
	@echo "  distclean     -- remove files generated during configuration"
	@echo "                   and building; the resulting tree has the same"
	@echo "                   files as the distribution."
	@echo "The following additional targets are primarily for developers:"
	@echo "  devclean      -- remove everything that is not part of the SVN"
	@echo "                   repository."

local-install:	$(BUILD_DIRS)
	for dir in $(BUILD_DIRS); do \
	  (cd $$dir && $(MAKE) local-install) || exit $$?; \
	done

install:	$(BUILD_DIRS)
	mkdir -p $(INSTALL_DIR)
	for dir in $(BUILD_DIRS); do \
	  (cd $$dir && $(MAKE) install) || exit $$?; \
	done

build:		$(BUILD_DIRS) examples doc
	(cd src && $(MAKE) build) || exit $$?

doc:
	(cd doc; $(MAKE) pdf)

examples:
	(cd examples; $(MAKE) build)

#################### Cleanup ####################

CLEAN_SUBDIRS =		$(BUILD_DIRS) examples

CLEAN_FILES =

DISTCLEAN_FILES +=	Makefile config.status config.log \
			autom4te*.cache \
			src/gen/fragments/Makefile \
			src/gen/fragments/mkfrags.sh \
			src/gen/fragments/mkmk.sh \
			src/lib/cxx/include/asdl/config.h \
			bin \
			lib \
			include

DEVCLEAN_FILES =	configure \
			config/config_h.in

include /workspaces/ml-yacc-calc/src/asdl/mk/clean-rules.gmk
