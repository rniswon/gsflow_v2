# makefile created on 2020-10-27 11:17:18.255120
# by pymake (version 1.2.0) for the 'gsflow' executable 
# using the 'gfortran' fortran and 'gcc' c/c++ compiler(s).


include ./makedefaults

# Define the source file directories
SOURCEDIR1=./stemp
SOURCEDIR2=./stemp/gsflow
SOURCEDIR3=./stemp/merge
SOURCEDIR4=./stemp/mmf
SOURCEDIR5=./stemp/modflow
SOURCEDIR6=./stemp/prms

VPATH = \
${SOURCEDIR1} \
${SOURCEDIR2} \
${SOURCEDIR3} \
${SOURCEDIR4} \
${SOURCEDIR5} \
${SOURCEDIR6} 

.SUFFIXES: .f .f90 .c .o

OBJECTS = \
$(OBJDIR)/gwf2swr7util.o \
$(OBJDIR)/gwflakmodule_NWT.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/gwfuzfmodule_NWT.o \
$(OBJDIR)/modules.o \
$(OBJDIR)/mhc7.o \
$(OBJDIR)/prms_constants.o \
$(OBJDIR)/gwfsfrmodule_NWT.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/gwf2bas7_NWT.o \
$(OBJDIR)/gwf2str7.o \
$(OBJDIR)/gwf2lpf7.o \
$(OBJDIR)/gwf2evt7.o \
$(OBJDIR)/gwf2swt7.o \
$(OBJDIR)/gwf2fhb7.o \
$(OBJDIR)/gwf2ibs7.o \
$(OBJDIR)/gwf2chd7.o \
$(OBJDIR)/gwf2ets7.o \
$(OBJDIR)/gwf2ghb7_NWT.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/gwf2huf7.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/pcg7_NWT.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/gwf2riv7_NWT.o \
$(OBJDIR)/nogmg.o \
$(OBJDIR)/gsflow_prms.o \
$(OBJDIR)/gwf2drn7_NWT.o \
$(OBJDIR)/gwf2gag7.o \
$(OBJDIR)/gwf2rch7.o \
$(OBJDIR)/de47_NWT.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/gwf2bcf7.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/hufutl7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/sip7_NWT.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/gwf2res7.o \
$(OBJDIR)/gwf2drt7.o \
$(OBJDIR)/obs2str7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/gwf2upw1.o \
$(OBJDIR)/gwf2swr7.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/gwf2swi27.o \
$(OBJDIR)/basin.o \
$(OBJDIR)/convert_params.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/gwf2lak7_NWT.o \
$(OBJDIR)/climateflow.o \
$(OBJDIR)/gwf2sub7_NWT.o \
$(OBJDIR)/gwf2mnw27_NWT.o \
$(OBJDIR)/gwf2sfr7_NWT.o \
$(OBJDIR)/soltab.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/gwf2mnw17_NWT.o \
$(OBJDIR)/gwf2hydmod7.o \
$(OBJDIR)/cascade.o \
$(OBJDIR)/lmt8_NWT.o \
$(OBJDIR)/prms_time.o \
$(OBJDIR)/potet_hamon.o \
$(OBJDIR)/potet_hs.o \
$(OBJDIR)/transp_tindex.o \
$(OBJDIR)/gsflow_modflow.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/potet_jh.o \
$(OBJDIR)/precip_temp_map.o \
$(OBJDIR)/nsegment_summary.o \
$(OBJDIR)/basin_summary.o \
$(OBJDIR)/climate_hru.o \
$(OBJDIR)/nhru_summary.o \
$(OBJDIR)/frost_date.o \
$(OBJDIR)/water_use_read.o \
$(OBJDIR)/transp_frost.o \
$(OBJDIR)/map_results.o \
$(OBJDIR)/potet_pt.o \
$(OBJDIR)/obs.o \
$(OBJDIR)/utils_prms_linux.o \
$(OBJDIR)/write_climate_hru.o \
$(OBJDIR)/nsub_summary.o \
$(OBJDIR)/ide_dist.o \
$(OBJDIR)/Irestart.o \
$(OBJDIR)/potet_pm.o \
$(OBJDIR)/potet_pm_sta.o \
$(OBJDIR)/ddsolrad.o \
$(OBJDIR)/potet_pan.o \
$(OBJDIR)/temp_1sta_laps.o \
$(OBJDIR)/precip_dist2.o \
$(OBJDIR)/temp_dist2.o \
$(OBJDIR)/intcp.o \
$(OBJDIR)/precip_1sta_laps.o \
$(OBJDIR)/ccsolrad.o \
$(OBJDIR)/xyz_dist.o \
$(OBJDIR)/snowcomp.o \
$(OBJDIR)/srunoff.o \
$(OBJDIR)/glacr_melt.o \
$(OBJDIR)/soilzone.o \
$(OBJDIR)/gwf2ag1_NWT.o \
$(OBJDIR)/dynamic_param_read.o \
$(OBJDIR)/gwf2uzf1_NWT.o \
$(OBJDIR)/gwflow.o \
$(OBJDIR)/gsflow_prms2mf.o \
$(OBJDIR)/prms_summary.o \
$(OBJDIR)/water_balance.o \
$(OBJDIR)/strmflow.o \
$(OBJDIR)/gsflow_budget.o \
$(OBJDIR)/routing.o \
$(OBJDIR)/gsflow_mf2prms.o \
$(OBJDIR)/strmflow_in_out.o \
$(OBJDIR)/gsflow_sum.o \
$(OBJDIR)/stream_temp.o \
$(OBJDIR)/muskingum.o \
$(OBJDIR)/basin_sum.o \
$(OBJDIR)/muskingum_lake.o \
$(OBJDIR)/subbasin.o \
$(OBJDIR)/print_vars.o \
$(OBJDIR)/write_vstats.o \
$(OBJDIR)/batch_run.o \
$(OBJDIR)/param_addr.o \
$(OBJDIR)/sort_vars.o \
$(OBJDIR)/umalloc_etc.o \
$(OBJDIR)/julconvert.o \
$(OBJDIR)/readvar.o \
$(OBJDIR)/control_addr.o \
$(OBJDIR)/alloc_space.o \
$(OBJDIR)/julday.o \
$(OBJDIR)/getdimname.o \
$(OBJDIR)/save_params.o \
$(OBJDIR)/mmf.o \
$(OBJDIR)/reset_dim.o \
$(OBJDIR)/dim_addr.o \
$(OBJDIR)/check_vars.o \
$(OBJDIR)/control_var.o \
$(OBJDIR)/sort_dims.o \
$(OBJDIR)/decl_control.o \
$(OBJDIR)/control_array.o \
$(OBJDIR)/sort_params.o \
$(OBJDIR)/print_params.o \
$(OBJDIR)/load_param.o \
$(OBJDIR)/putvar.o \
$(OBJDIR)/create_vstats.o \
$(OBJDIR)/declparam.o \
$(OBJDIR)/get_times.o \
$(OBJDIR)/read_line.o \
$(OBJDIR)/batch_run_functions.o \
$(OBJDIR)/declvar.o \
$(OBJDIR)/read_control.o \
$(OBJDIR)/graph_single_run.o \
$(OBJDIR)/timing.o \
$(OBJDIR)/var_addr.o \
$(OBJDIR)/getparam.o \
$(OBJDIR)/str_to_vals.o \
$(OBJDIR)/call_modules.o \
$(OBJDIR)/parse_args.o \
$(OBJDIR)/read_params.o \
$(OBJDIR)/getvar.o \
$(OBJDIR)/read_datainfo.o \
$(OBJDIR)/read_vars.o \
$(OBJDIR)/getdim.o \
$(OBJDIR)/decldim.o \
$(OBJDIR)/call_setdims.o \
$(OBJDIR)/setup_cont.o \
$(OBJDIR)/get_elem_add.o \
$(OBJDIR)/build_lists.o \
$(OBJDIR)/free_vstats.o \
$(OBJDIR)/print_model_info.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.c
	@mkdir -p $(@D)
	$(CC) $(OPTLEVEL) $(CFLAGS) -c $< -o $@ $(INCSWITCH)

