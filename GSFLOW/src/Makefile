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
LIB		 = ./lib

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
	cd $(MMF); $(RM) *.o *.mod *~;
	cd $(MODFLOW); $(RM) *.o *.mod *~;
	cd $(PRMS); $(RM) *.o *.mod *~;
	cd $(GSFLOW); $(RM) *.o *.mod *~;
	cd $(LIB); rm -f *.a;

