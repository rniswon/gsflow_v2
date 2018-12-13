#
# Makefile --
#
# Top-level makefile for the GSFLOW
#
#-------------------------------------------------------------------
# $Id: Makefile 8348 2016-07-26 20:37:09Z rsregan $
#-------------------------------------------------------------------

include ./makelist
MMF      = ./mmf
PRMS     = ./prms
GSFLOW   = ./gsflow
MODFLOW  = ./modflow

#
# Standard Targets for Users
#

all: standard

standard:
	cd $(MMF); $(MAKE);
	cd $(MODFLOW); $(MAKE);
	cd $(PRMS); $(MAKE);
	cd $(GSFLOW); $(MAKE)

clean:
	cd $(MMF); $(MAKE) clean;
	cd $(MODFLOW); $(MAKE) clean;
	cd $(PRMS); $(MAKE) clean;
	cd $(GSFLOW); $(MAKE) clean;
	cd $(LIB); rm -f *.a;
	cd $(BIN); rm -f gsflow

