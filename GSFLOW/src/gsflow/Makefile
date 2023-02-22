###################################################
# $Id: Makefile 7497 2015-07-17 16:38:50Z rsregan $
###################################################

include ../makelist
TARGET		= $(BINDIR)/gsflow
MFNWTLIB	= $(LIBDIR)/libmfnwt.a
LIBS 		= -L$(LIBDIR) -lmfnwt -lprms $(FLIBS)

###################################################

MODOBJS =	\
		gsflow_module.o \
		gsflow_prms.o \
		gsflow_modflow.o \
		gsflow_fortran.o \
		gsflow_prms2mf.o \
		gsflow_mf2prms.o \
		gsflow_prms2modsim.o \
		gsflow_modsim2prms.o \
		modsim_diversion_read.o \
		gsflow_budget.o \
		gsflow_sum.o

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): $(MFNWTLIB) $(PRMSLIB) $(MODOBJS)
	$(RM) $(TARGET)
	$(FC) $(LDFLAGS) -o $(TARGET) $(MODOBJS) $(LIBS)

clean:
	$(RM) *.o *.mod *.smod *genmod* *~

####################################################
# Rules for the modules
###################################################
global.mod:
	$(RM) global.mod
	$(LN) $(MODFLOWDIR)/global.mod .

gwfbasmodule.mod: $(MODFLOWDIR)/gwfbasmodule.mod
	$(RM) gwfbasmodule.mod
	$(LN) $(MODFLOWDIR)/gwfbasmodule.mod .

gwfbcfmodule.mod: $(MODFLOWDIR)/gwfbcfmodule.mod
	$(RM) gwfbcfmodule.mod
	$(LN) $(MODFLOWDIR)/gwfbcfmodule.mod .

gwfsfrmodule.mod: $(MODFLOWDIR)/gwfsfrmodule.mod
	$(RM) gwfsfrmodule.mod
	$(LN) $(MODFLOWDIR)/gwfsfrmodule.mod .

gwfnwtmodule.mod: $(MODFLOWDIR)/gwfnwtmodule.mod
	$(RM) gwfnwtmodule.mod
	$(LN) $(MODFLOWDIR)/gwfnwtmodule.mod .

gwfagmodule.mod: $(MODFLOWDIR)/gwfagmodule.mod
	$(RM) gwfagmodule.mod
	$(LN) $(MODFLOWDIR)/gwfagmodule.mod .

gwflpfmodule.mod: $(MODFLOWDIR)/gwflpfmodule.mod
	$(RM) gwflpfmodule.mod
	$(LN) $(MODFLOWDIR)/gwflpfmodule.mod .

gwfuzfmodule.mod: $(MODFLOWDIR)/gwfuzfmodule.mod
	$(RM) gwfuzfmodule.mod
	$(LN) $(MODFLOWDIR)/gwfuzfmodule.mod .

gwflakmodule.mod: $(MODFLOWDIR)/gwflakmodule.mod
	$(RM) gwflakmodule.mod
	$(LN) $(MODFLOWDIR)/gwflakmodule.mod .

pcgmodule.mod: $(MODFLOWDIR)/pcgmodule.mod
	$(RM) pcgmodule.mod
	$(LN) $(MODFLOWDIR)/pcgmodule.mod .

gwfevtmodule.mod: $(MODFLOWDIR)/gwfevtmodule.mod
	$(RM) gwfevtmodule.mod
	$(LN) $(MODFLOWDIR)/gwfevtmodule.mod .

gwfrchmodule.mod: $(MODFLOWDIR)/gwfrchmodule.mod
	$(RM) gwfrchmodule.mod
	$(LN) $(MODFLOWDIR)/gwfrchmodule.mod .

obsbasmodule.mod: $(MODFLOWDIR)/obsbasmodule.mod
	$(RM) obsbasmodule.mod
	$(LN) $(MODFLOWDIR)/obsbasmodule.mod .

sipmodule.mod: $(MODFLOWDIR)/sipmodule.mod
	$(RM) sipmodule.mod
	$(LN) $(MODFLOWDIR)/sipmodule.mod .

de4module.mod: $(MODFLOWDIR)/de4module.mod
	$(RM) de4module.mod
	$(LN) $(MODFLOWDIR)/de4module.mod .

gwfhufmodule.mod: $(MODFLOWDIR)/gwfhufmodule.mod
	$(RM) gwfhufmodule.mod
	$(LN) $(MODFLOWDIR)/gwfhufmodule.mod .

prms_snow.mod: $(PRMSDIR)/prms_snow.mod
	$(RM) prms_snow.mod
	$(LN) $(PRMSDIR)/prms_snow.mod .

prms_intcp.mod: $(PRMSDIR)/prms_intcp.mod
	$(RM) prms_intcp.mod
	$(LN) $(PRMSDIR)/prms_intcp.mod .

prms_obs.mod: $(PRMSDIR)/prms_obs.mod
	$(RM) prms_obs.mod
	$(LN) $(PRMSDIR)/prms_obs.mod .

prms_climatevars.mod: $(PRMSDIR)/prms_climatevars.mod
	$(RM) prms_climatevars.mod
	$(LN) $(PRMSDIR)/prms_climatevars.mod .

prms_flowvars.mod: $(PRMSDIR)/prms_flowvars.mod
	$(RM) prms_flowvars.mod
	$(LN) $(PRMSDIR)/prms_flowvars.mod .

prms_soilzone.mod: $(PRMSDIR)/prms_soilzone.mod
	$(RM) prms_soilzone.mod
	$(CD) $(PRMSDIR);make prms_soilzone.mod
	$(LN) $(PRMSDIR)/prms_soilzone.mod .

prms_soilzone_ag.mod: $(PRMSDIR)/prms_soilzone_ag.mod
	$(CD) $(PRMSDIR);make prms_soilzone_ag.mod
	$(LN) $(PRMSDIR)/prms_soilzone_ag.mod .

prms_srunoff.mod: $(PRMSDIR)/prms_srunoff.mod
	$(CD) $(PRMSDIR);make prms_srunoff.mod
	$(LN) $(PRMSDIR)/prms_srunoff.mod .

prms_set_time.mod: $(PRMSDIR)/prms_set_time.mod
	$(CD) $(PRMSDIR);make prms_set_time.mod
	$(LN) $(PRMSDIR)/prms_set_time.mod .

prms_basin.mod: $(PRMSDIR)/prms_basin.mod
	$(CD) $(PRMSDIR);make prms_set_time.mod
	$(LN) $(PRMSDIR)/prms_basin.mod .

prms_constants.mod: $(PRMSDIR)/prms_constants.mod
	$(CD) $(PRMSDIR);make prms_constants.mod
	$(LN) $(PRMSDIR)/prms_constants.mod .

gwfupwmodule.mod: $(MODFLOWDIR)/gwfupwmodule.mod
	$(RM) gwfupwmodule.mod
	$(LN) $(MODFLOWDIR)/gwfupwmodule.mod gwfupwmodule.mod

prms_control_file.mod:
	$(CD) $(PRMSDIR);make prms_control_file.mod
	$(LN) $(PRMSDIR)/prms_control_file.mod .

prms_data_file.mod:
	$(RM) prms_data_file.mod
	$(LN) $(PRMSDIR)/prms_data_file.mod .

prms_read_param_file.mod:
	$(RM) prms_read_param_file.mod
	$(LN) $(PRMSDIR)/prms_read_param_file.mod .
	
prms_it0_vars.mod: $(PRMSDIR)/prms_it0_vars.mod
	$(RM) prms_it0_vars.mod
	$(LN) $(PRMSDIR)/prms_it0_vars.mod .

prms_mmfapi.mod:
	$(RM) prms_mmfapi.mod
	$(LN) $(PRMSDIR)/prms_mmfapi.mod .

prms_utils.mod: $(PRMSDIR)/prms_utils.mod
	$(CD) $(PRMSDIR);make prms_utils.mod
	$(LN) $(PRMSDIR)/prms_utils.mod .

gsfprms2mf.mod: gsflow_prms2mf.o
gsfmf2prms.mod: gsflow_mf2prms.o
gsfmodflow.mod: gsflow_modflow.o
prms_module.mod: gsflow_module.o
gsfmodsim2prms.mod: gsflow_modsim2prms.o
prms_modsim_diversion_read.mod: modsim_diversion_read.o

gsflow_module.o: gsflow_module.f90 prms_constants.mod
	$(FC) -c $(FFLAGS) gsflow_module.f90

gsflow_fortran.o: gsflow_fortran.f90
	$(FC) -c $(FFLAGS) gsflow_fortran.f90

gsflow_prms2mf.o: gsflow_prms2mf.f90 prms_module.mod prms_constants.mod prms_read_param_file.mod gwfuzfmodule.mod gwfsfrmodule.mod gwflakmodule.mod gsfmodflow.mod prms_basin.mod prms_soilzone.mod global.mod prms_climatevars.mod prms_flowvars.mod prms_srunoff.mod gwfagmodule.mod gsflow_module.o
	$(FC) -c $(FFLAGS) gsflow_prms2mf.f90

gsflow_modflow.o: gsflow_modflow.f openspec.inc prms_constants.mod prms_module.mod prms_control_file.mod prms_set_time.mod prms_utils.mod prms_mmfapi.mod global.mod gwfbasmodule.mod gwfsfrmodule.mod gwflakmodule.mod gwfuzfmodule.mod gwfhufmodule.mod gwfevtmodule.mod gwfbcfmodule.mod gwfnwtmodule.mod pcgmodule.mod sipmodule.mod de4module.mod prms_basin.mod gwfrchmodule.mod obsbasmodule.mod gwflpfmodule.mod gwfupwmodule.mod gsflow_module.o
	$(FC) -c $(FFLAGS) gsflow_modflow.f

gsflow_prms.o: gsflow_prms.f90 global.mod prms_constants.mod prms_control_file.mod prms_read_param_file.mod prms_data_file.mod prms_read_param_file.mod prms_mmfapi.mod prms_utils.mod gsflow_module.o prms_set_time.mod gwfsfrmodule.mod gwflakmodule.mod gsflow_modflow.o
	$(FC) -c $(FFLAGS) gsflow_prms.f90

gsflow_sum.o: gsflow_sum.f90 prms_constants.mod prms_module.mod prms_control_file.mod prms_read_param_file.mod gsfmodflow.mod gwflakmodule.mod gwfsfrmodule.mod global.mod prms_basin.mod prms_flowvars.mod prms_it0_vars.mod prms_srunoff.mod gsfbudget.mod gwfuzfmodule.mod gwfbasmodule.mod prms_obs.mod prms_set_time.mod prms_climatevars.mod prms_snow.mod prms_intcp.mod prms_soilzone.mod gwfbcfmodule.mod gwflpfmodule.mod gwfupwmodule.mod gwfnwtmodule.mod gwfagmodule.mod gsflow_module.o
	$(FC) -c $(FFLAGS) gsflow_sum.f90

gsflow_budget.o: gsflow_budget.f90 prms_module.mod prms_read_param_file.mod gwfsfrmodule.mod global.mod gwfuzfmodule.mod gsfmodflow.mod gsfprms2mf.mod gwfbasmodule.mod gwflakmodule.mod prms_constants.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_soilzone.mod gsflow_module.o
	$(FC) -c $(FFLAGS) gsflow_budget.f90

gsflow_mf2prms.o: gsflow_mf2prms.f90 global.mod gwfuzfmodule.mod gwflakmodule.mod gwfsfrmodule.mod prms_soilzone.mod prms_module.mod gwfbasmodule.mod gwfagmodule.mod gsflow_module.o
	$(FC) -c $(FFLAGS) gsflow_mf2prms.f90
	
gsflow_prms2modsim.o: gsflow_prms2modsim.f90 prms_constants.mod prms_module.mod prms_utils.mod prms_basin.mod prms_mmfapi.mod prms_climatevars.mod prms_flowvars.mod prms_set_time.mod prms_srunoff.mod prms_soilzone.mod prms_soilzone_ag.mod prms_modsim_diversion_read.mod
	$(FC) -c $(FFLAGS) gsflow_prms2modsim.f90

gsflow_modsim2prms.o: gsflow_modsim2prms.f90 prms_constants.mod prms_module.mod prms_utils.mod prms_mmfapi.mod prms_modsim_diversion_read.mod
	$(FC) -c $(FFLAGS) gsflow_modsim2prms.f90

modsim_diversion_read.o : modsim_diversion_read.f90 prms_constants.mod prms_module.mod prms_control_file.mod
	$(FC) -c $(FFLAGS) modsim_diversion_read.f90
