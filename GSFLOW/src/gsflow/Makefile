###################################################
# $Id: Makefile 7497 2015-07-17 16:38:50Z rsregan $
###################################################

include ../makelist
TARGET		= $(BINDIR)/gsflow
MFNWTLIB	= $(LIBDIR)/libmfnwt.a
LIBS 		= -L$(LIBDIR) -lmfnwt -lprms -lmmf $(FLIBS)

###################################################

MODOBJS =	\
		gsflow_prms.o \
		gsflow_modflow.o \
		gsflow_prms2mf.o \
		gsflow_mf2prms.o \
		gsflow_budget.o \
		gsflow_sum.o

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): $(MFNWTLIB) $(PRMSLIB) $(MODOBJS) $(LIBDIR)/libmmf.a
	$(RM) $(TARGET)
	$(FC) $(LDFLAGS) -o $(TARGET) $(MODOBJS) $(LIBS)

clean:
	$(RM) *.o *.mod *~

####################################################
# Rules for the modules
###################################################
global.mod: 
	$(RM) global.mod
	$(CD) $(MODFLOWDIR);make global.mod
	$(LN) $(MODFLOWDIR)/global.mod global.mod

gwfbasmodule.mod: $(MODFLOWDIR)/gwfbasmodule.mod
	$(RM) gwfbasmodule.mod
	$(LN) $(MODFLOWDIR)/gwfbasmodule.mod gwfbasmodule.mod

gwfbcfmodule.mod: $(MODFLOWDIR)/gwfbcfmodule.mod
	$(RM) gwfbcfmodule.mod
	$(LN) $(MODFLOWDIR)/gwfbcfmodule.mod gwfbcfmodule.mod

gwfsfrmodule.mod: $(MODFLOWDIR)/gwfsfrmodule.mod
	$(RM) gwfsfrmodule.mod
	$(LN) $(MODFLOWDIR)/gwfsfrmodule.mod gwfsfrmodule.mod

gwfnwtmodule.mod: $(MODFLOWDIR)/gwfnwtmodule.mod
	$(RM) gwfnwtmodule.mod
	$(LN) $(MODFLOWDIR)/gwfnwtmodule.mod gwfnwtmodule.mod

gwfagmodule.mod: $(MODFLOWDIR)/gwfagmodule.mod
	$(RM) gwfagmodule.mod
	$(LN) $(MODFLOWDIR)/gwfagmodule.mod gwfagmodule.mod

gwflpfmodule.mod: $(MODFLOWDIR)/gwflpfmodule.mod
	$(RM) gwflpfmodule.mod
	$(LN) $(MODFLOWDIR)/gwflpfmodule.mod gwflpfmodule.mod

gwfuzfmodule.mod: $(MODFLOWDIR)/gwfuzfmodule.mod
	$(RM) gwfuzfmodule.mod
	$(LN) $(MODFLOWDIR)/gwfuzfmodule.mod gwfuzfmodule.mod

gwflakmodule.mod: $(MODFLOWDIR)/gwflakmodule.mod
	$(RM) gwflakmodule.mod
	$(LN) $(MODFLOWDIR)/gwflakmodule.mod gwflakmodule.mod

pcgmodule.mod: $(MODFLOWDIR)/pcgmodule.mod
	$(RM) pcgmodule.mod
	$(CD) $(MODFLOWDIR);make pcgmodule.mod
	$(LN) $(MODFLOWDIR)/pcgmodule.mod pcgmodule.mod

gwfevtmodule.mod: $(MODFLOWDIR)/gwfevtmodule.mod
	$(RM) gwfevtmodule.mod
	$(LN) $(MODFLOWDIR)/gwfevtmodule.mod gwfevtmodule.mod

gwfrchmodule.mod: $(MODFLOWDIR)/gwfrchmodule.mod
	$(RM) gwfrchmodule.mod
	$(LN) $(MODFLOWDIR)/gwfrchmodule.mod gwfrchmodule.mod

obsbasmodule.mod: $(MODFLOWDIR)/obsbasmodule.mod
	$(RM) obsbasmodule.mod
	$(LN) $(MODFLOWDIR)/obsbasmodule.mod obsbasmodule.mod

sipmodule.mod: $(MODFLOWDIR)/sipmodule.mod
	$(RM) sipmodule.mod
	$(LN) $(MODFLOWDIR)/sipmodule.mod sipmodule.mod

de4module.mod: $(MODFLOWDIR)/de4module.mod
	$(RM) de4module.mod
	$(LN) $(MODFLOWDIR)/de4module.mod de4module.mod

gwfhufmodule.mod: $(MODFLOWDIR)/gwfhufmodule.mod
	$(RM) gwfhufmodule.mod
	$(LN) $(MODFLOWDIR)/gwfhufmodule.mod gwfhufmodule.mod

prms_cascade.mod: $(PRMSDIR)/prms_cascade.mod
	$(RM) prms_cascade.mod
	$(LN) $(PRMSDIR)/prms_cascade.mod prms_cascade.mod

prms_snow.mod: $(PRMSDIR)/prms_snow.mod
	$(RM) prms_snow.mod
	$(LN) $(PRMSDIR)/prms_snow.mod prms_snow.mod

prms_intcp.mod: $(PRMSDIR)/prms_intcp.mod
	$(RM) prms_intcp.mod
	$(LN) $(PRMSDIR)/prms_intcp.mod prms_intcp.mod

prms_obs.mod: $(PRMSDIR)/prms_obs.mod
	$(RM) prms_obs.mod
	$(LN) $(PRMSDIR)/prms_obs.mod prms_obs.mod

prms_climatevars.mod: $(PRMSDIR)/prms_climatevars.mod
	$(RM) prms_climatevars.mod
	$(LN) $(PRMSDIR)/prms_climatevars.mod prms_climatevars.mod

prms_flowvars.mod: $(PRMSDIR)/prms_flowvars.mod
	$(RM) prms_flowvars.mod
	$(LN) $(PRMSDIR)/prms_flowvars.mod prms_flowvars.mod

prms_soilzone.mod: $(PRMSDIR)/prms_soilzone.mod
	$(RM) prms_soilzone.mod
	$(LN) $(PRMSDIR)/prms_soilzone.mod prms_soilzone.mod

prms_srunoff.mod: $(PRMSDIR)/prms_srunoff.mod
	$(RM) prms_srunoff.mod
	$(LN) $(PRMSDIR)/prms_srunoff.mod prms_srunoff.mod

prms_set_time.mod:
	$(RM) prms_set_time.mod
	$(CD) $(PRMSDIR);make prms_set_time.mod
	$(LN) $(PRMSDIR)/prms_set_time.mod prms_set_time.mod

prms_basin.mod: $(PRMSDIR)/prms_basin.mod
	$(RM) prms_basin.mod
	$(LN) $(PRMSDIR)/prms_basin.mod prms_basin.mod

prms_constants.mod:
	$(CD) $(PRMSDIR);make prms_constants.mod
	$(LN) $(PRMSDIR)/prms_constants.mod .
	
prms_water_use.mod:
	$(CD) $(PRMSDIR);make prms_water_use.mod
	$(LN) $(PRMSDIR)/prms_water_use.mod .

gwfupwmodule.mod: $(MODFLOWDIR)/gwfupwmodule.mod
	$(RM) gwfupwmodule.mod
	$(LN) $(MODFLOWDIR)/gwfupwmodule.mod gwfupwmodule.mod

gsfprms2mf.mod: gsflow_prms2mf.o
gsfmf2prms.mod: gsflow_mf2prms.o
gsfmodflow.mod: gsflow_modflow.o
prms_module.mod: gsflow_prms.o

gsflow_prms2mf.o: gsflow_prms2mf.f90 prms_module.mod prms_constants.mod gwfuzfmodule.mod gwfsfrmodule.mod gwflakmodule.mod gsfmodflow.mod prms_basin.mod prms_soilzone.mod global.mod prms_climatevars.mod prms_flowvars.mod prms_srunoff.mod gwfagmodule.mod prms_water_use.mod
	$(FC) -c $(FFLAGS) gsflow_prms2mf.f90

gsflow_modflow.o: gsflow_modflow.f openspec.inc prms_constants.mod prms_module.mod prms_set_time.mod global.mod gwfbasmodule.mod gwfsfrmodule.mod gwflakmodule.mod gwfuzfmodule.mod gwfhufmodule.mod gwfevtmodule.mod gwfbcfmodule.mod gwfnwtmodule.mod pcgmodule.mod sipmodule.mod de4module.mod prms_basin.mod gwfrchmodule.mod obsbasmodule.mod gwflpfmodule.mod gwfupwmodule.mod
	$(FC) -c $(FFLAGS) gsflow_modflow.f

gsflow_prms.o: gsflow_prms.f90 global.mod prms_constants.mod
	$(FC) -c $(FFLAGS) gsflow_prms.f90

gsflow_sum.o: gsflow_sum.f90 prms_constants.mod prms_module.mod gsfmodflow.mod gwflakmodule.mod gwfsfrmodule.mod global.mod prms_basin.mod prms_flowvars.mod prms_srunoff.mod gsfbudget.mod gwfuzfmodule.mod gwfbasmodule.mod prms_obs.mod prms_set_time.mod prms_climatevars.mod prms_snow.mod prms_intcp.mod prms_soilzone.mod gwfbcfmodule.mod gwflpfmodule.mod gwfupwmodule.mod gwfnwtmodule.mod gwfagmodule.mod
	$(FC) -c $(FFLAGS) gsflow_sum.f90

gsflow_budget.o: gsflow_budget.f90 prms_module.mod gwfsfrmodule.mod global.mod gwfuzfmodule.mod gsfmodflow.mod gsfprms2mf.mod gwfbasmodule.mod gwflakmodule.mod prms_constants.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) gsflow_budget.f90

gsflow_mf2prms.o: gsflow_mf2prms.f90 global.mod gwfuzfmodule.mod gwflakmodule.mod gwfsfrmodule.mod prms_soilzone.mod prms_module.mod gwfbasmodule.mod gwfagmodule.mod
	$(FC) -c $(FFLAGS) gsflow_mf2prms.f90

