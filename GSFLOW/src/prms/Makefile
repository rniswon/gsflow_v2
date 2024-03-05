# PRMS library makefile

include ../makelist
PRMSLIB = $(LIBDIR)/libprms.a
LIBS	= $(PRMSLIB) $(FLIBS)

####################################################
# Rules for targets
####################################################
all: $(PRMSLIB)

$(TARGET): $(PRMSLIB)
	$(RM) $(TARGET)

#
# Define all object files which make up the library
#


LIBOBJS = \
        prms_constants.o \
        c_prms_time.o \
        c_utils_prms.o \
        c_read_control_file.o \
        c_read_data_file.o \
        c_read_parameter_file.o \
        c_mmf_utils.o \
        basin.o \
        climateflow.o \
        cascade.o \
        soltab.o \
        convert_params.o \
        obs.o \
        climate_hru.o \
        potet_jh.o \
        potet_pt.o \
        potet_hs.o \
        potet_pm.o \
        potet_pm_sta.o \
        potet_pan.o \
        potet_hamon.o \
        ddsolrad.o \
        cloud_cover.o \
        ccsolrad.o \
        ide_dist.o \
        xyz_dist.o \
        precip_1sta_laps.o \
        temp_map.o \
        precip_map.o \
        precip_dist2.o \
        temp_1sta_laps.o \
        temp_dist2.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date.o \
        intcp.o \
        snowcomp.o \
        glacr_melt.o \
        srunoff.o \
        soilzone.o \
        soilzone_ag.o \
        gwflow.o \
        water_use_read.o \
        dynamic_param_read.o \
        dynamic_soil_param_read.o \
        water_balance.o \
        routing.o \
        strmflow.o \
        strmflow_in_out.o \
        muskingum.o \
        muskingum_lake.o \
        segment_to_hru.o \
        strmflow_character.o \
        subbasin.o \
        stream_temp.o \
        statvar_out.o \
        map_results.o \
        nhru_summary.o \
        nsub_summary.o \
        nsegment_summary.o \
        basin_summary.o \
        write_climate_hru.o \
        prms_summary.o \
        basin_sum.o \
        sm_prms_time.o \
        sm_utils_prms.o \
        sm_read_control_file.o \
        sm_read_data_file.o \
        sm_read_parameter_file.o \
        sm_mmf_utils.o

install: lib

lib: $(PRMSLIB)

$(PRMSLIB): $(LIBOBJS)
	$(RM) $(PRMSLIB)
	$(AR) $(PRMSLIB) $(LIBOBJS)
	$(RANLIB) $(PRMSLIB)

clean:
	$(RM) *.o *.mod *.smod *genmod* *~

#
# Define all object files which make up the library
#

prms_constants.o: prms_constants.f90
	$(FC) -c $(FFLAGS) prms_constants.f90

basin_sum.o: basin_sum.f90 prms_module.mod prms_read_param_file.mod prms_flowvars.mod prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_gwflow.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_muskingum.mod prms_constants.mod
	$(FC) -c $(FFLAGS) basin_sum.f90

subbasin.o: subbasin.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_intcp.mod prms_srunoff.mod prms_soilzone.mod prms_gwflow.mod prms_snow.mod prms_climatevars.mod prms_muskingum_lake.mod prms_constants.mod prms_climate_hru.mod
	$(FC) -c $(FFLAGS) subbasin.f90

ddsolrad.o: ddsolrad.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) ddsolrad.f90

ccsolrad.o: ccsolrad.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) ccsolrad.f90

snowcomp.o: snowcomp.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_set_time.mod prms_intcp.mod prms_constants.mod prms_soltab.mod prms_climate_hru.mod
	$(FC) -c $(FFLAGS) snowcomp.f90

gwflow.o: gwflow.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_srunoff.mod prms_soilzone.mod prms_cascade.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) gwflow.f90

sm_utils_prms.o: sm_utils_prms.f90 prms_utils.mod prms_module.mod prms_basin.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) sm_utils_prms.f90

prms_summary.o: prms_summary.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_climatevars.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_soilzone.mod prms_gwflow.mod prms_constants.mod
	$(FC) -c $(FFLAGS) prms_summary.f90

muskingum.o: muskingum.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_srunoff.mod prms_gwflow.mod prms_routing.mod prms_constants.mod
	$(FC) -c $(FFLAGS) muskingum.f90

intcp.o: intcp.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_obs.mod prms_climatevars.mod prms_flowvars.mod prms_set_time.mod prms_water_use.mod prms_constants.mod
	$(FC) -c $(FFLAGS) intcp.f90

map_results.o: map_results.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) map_results.f90

nhru_summary.o: nhru_summary.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) nhru_summary.f90

nsub_summary.o: nsub_summary.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) nsub_summary.f90

nsegment_summary.o: nsegment_summary.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) nsegment_summary.f90

basin_summary.o: basin_summary.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) basin_summary.f90

soltab.o: soltab.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_constants.mod
	$(FC) -c $(FFLAGS) soltab.f90

frost_date.o: frost_date.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) frost_date.f90

precip_1sta_laps.o: precip_1sta_laps.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) precip_1sta_laps.f90

temp_map.o: temp_map.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) temp_map.f90

precip_map.o: precip_map.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) precip_map.f90

transp_tindex.o: transp_tindex.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) transp_tindex.f90

transp_frost.o: transp_frost.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) transp_frost.f90

temp_1sta_laps.o: temp_1sta_laps.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) temp_1sta_laps.f90

temp_dist2.o: temp_dist2.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) temp_dist2.f90

precip_dist2.o: precip_dist2.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) precip_dist2.f90

strmflow.o: strmflow.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod prms_srunoff.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) strmflow.f90

strmflow_in_out.o: strmflow_in_out.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod prms_srunoff.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) strmflow_in_out.f90

stream_temp.o: stream_temp.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_routing.mod prms_set_time.mod prms_snow.mod prms_soltab.mod prms_constants.mod
	$(FC) -c $(FFLAGS) stream_temp.f90

strmflow_character.o: strmflow_character.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod prms_srunoff.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) strmflow_character.f90

potet_jh.o: potet_jh.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) potet_jh.f90

potet_pt.o: potet_pt.f90 prms_module.mod prms_basin.mod prms_read_param_file.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_climate_hru.mod prms_constants.mod
	$(FC) -c $(FFLAGS) potet_pt.f90

potet_hs.o: potet_hs.f90 prms_module.mod prms_basin.mod prms_read_param_file.mod prms_climatevars.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) potet_hs.f90

potet_pm.o: potet_pm.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_climate_hru.mod prms_constants.mod
	$(FC) -c $(FFLAGS) potet_pm.f90

potet_pm_sta.o: potet_pm_sta.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_climate_hru.mod prms_soltab.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) potet_pm_sta.f90

potet_pan.o: potet_pan.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_obs.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) potet_pan.f90

potet_hamon.o: potet_hamon.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) potet_hamon.f90

write_climate_hru.o: write_climate_hru.f90 prms_module.mod prms_control_file.mod prms_set_time.mod prms_climatevars.mod prms_constants.mod
	$(FC) -c $(FFLAGS) write_climate_hru.f90

climate_hru.o: climate_hru.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_constants.mod
	$(FC) -c $(FFLAGS) climate_hru.f90

cascade.o: cascade.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_constants.mod
	$(FC) -c $(FFLAGS) cascade.f90

basin.o: basin.f90 prms_module.mod prms_constants.mod prms_mmfapi.mod prms_read_param_file.mod prms_utils.mod
	$(FC) -c $(FFLAGS) basin.f90

obs.o: obs.f90 prms_module.mod prms_read_param_file.mod prms_set_time.mod prms_basin.mod prms_constants.mod
	$(FC) -c $(FFLAGS) obs.f90

srunoff.o: srunoff.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_cascade.mod prms_intcp.mod prms_snow.mod prms_set_time.mod prms_constants.mod prms_climate_hru.mod
	$(FC) -c $(FFLAGS) srunoff.f90

climateflow.o: climateflow.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_set_time.mod prms_constants.mod prms_utils.mod
	$(FC) -c $(FFLAGS) climateflow.f90

soilzone.o: soilzone.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_flowvars.mod prms_snow.mod prms_climatevars.mod prms_cascade.mod prms_set_time.mod prms_intcp.mod prms_srunoff.mod prms_constants.mod prms_climate_hru.mod gsfmodsim2prms.mod
	$(FC) -c $(FFLAGS) soilzone.f90

routing.o: routing.f90 prms_module.mod prms_basin.mod prms_read_param_file.mod prms_gwflow.mod prms_flowvars.mod prms_set_time.mod prms_srunoff.mod prms_climatevars.mod prms_water_use.mod prms_constants.mod
	$(FC) -c $(FFLAGS) routing.f90

sm_prms_time.o: sm_prms_time.f90 prms_set_time.mod prms_module.mod prms_basin.mod prms_constants.mod prms_mmfapi.mod prms_data_file.mod prms_utils.mod
	$(FC) -c $(FFLAGS) sm_prms_time.f90

water_balance.o: water_balance.f90 prms_module.mod prms_basin.mod prms_srunoff.mod prms_flowvars.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_cascade.mod prms_intcp.mod prms_snow.mod prms_soilzone.mod prms_constants.mod
	$(FC) -c $(FFLAGS) water_balance.f90

ide_dist.o: ide_dist.f prms_module.mod prms_read_param_file.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) ide_dist.f

xyz_dist.o: xyz_dist.f prms_module.mod prms_read_param_file.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) xyz_dist.f

muskingum_lake.o: muskingum_lake.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routing.mod prms_srunoff.mod prms_gwflow.mod prms_soilzone.mod prms_constants.mod
	$(FC) -c $(FFLAGS) muskingum_lake.f90

dynamic_param_read.o: dynamic_param_read.f90 prms_module.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_flowvars.mod prms_potet_jh.mod prms_potet_pm.mod prms_potet_hs.mod prms_potet_pt.mod prms_potet_hamon.mod prms_transp_tindex.mod prms_transp_frost.mod prms_intcp.mod prms_snow.mod prms_climate_hru.mod prms_constants.mod
	$(FC) -c $(FFLAGS) dynamic_param_read.f90

dynamic_soil_param_read.o: dynamic_soil_param_read.f90 prms_module.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_flowvars.mod prms_potet_jh.mod prms_srunoff.mod prms_soilzone.mod prms_soilzone_ag.mod prms_climate_hru.mod prms_constants.mod
	$(FC) -c $(FFLAGS) dynamic_soil_param_read.f90
	
modsim_diversion_read.o : prms_constants.mod prms_module.mod prms_control_file.mod
	$(FC) -c $(FFLAGS) modsim_diversion_read.f90

water_use_read.o: water_use_read.f90 prms_module.mod prms_read_param_file.mod prms_control_file.mod prms_basin.mod prms_set_time.mod prms_flowvars.mod prms_constants.mod
	$(FC) -c $(FFLAGS) water_use_read.f90

convert_params.o: convert_params.f90 prms_module.mod prms_read_param_file.mod
	$(FC) -c $(FFLAGS) convert_params.f90

glacr_melt.o: glacr_melt.f90 prms_control_file.mod prms_read_param_file.mod prms_snow.mod prms_intcp.mod prms_soltab.mod prms_constants.mod
	$(FC) -c $(FFLAGS) glacr_melt.f90

cloud_cover.o: cloud_cover.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod prms_constants.mod
	$(FC) -c $(FFLAGS) cloud_cover.f90

soilzone_ag.o: soilzone_ag.f90 prms_module.mod prms_read_param_file.mod prms_basin.mod prms_flowvars.mod prms_snow.mod prms_climatevars.mod prms_cascade.mod prms_set_time.mod prms_intcp.mod prms_srunoff.mod prms_constants.mod prms_climate_hru.mod gsfmodsim2prms.mod
	$(FC) -c $(FFLAGS) soilzone_ag.f90

statvar_out.o: statvar_out.f90 prms_module.mod prms_control_file.mod prms_constants.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) statvar_out.f90

segment_to_hru.o: segment_to_hru.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_constants.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) segment_to_hru.f90

c_mmf_utils.o: c_mmf_utils.f90 prms_constants.mod
	$(FC) -c $(FFLAGS) c_mmf_utils.f90

sm_mmf_utils.o: sm_mmf_utils.f90 prms_mmfapi.mod prms_constants.mod prms_set_time.mod prms_control_file.mod prms_utils.mod
	$(FC) -c $(FFLAGS) sm_mmf_utils.f90

c_prms_time.o: c_prms_time.f90 prms_constants.mod
	$(FC) -c $(FFLAGS) c_prms_time.f90

c_read_control_file.o: c_read_control_file.f90 prms_constants.mod
	$(FC) -c $(FFLAGS) c_read_control_file.f90

sm_read_control_file.o: sm_read_control_file.f90 prms_control_file.mod prms_module.mod gsfmodflow.mod prms_constants.mod prms_climate_hru.mod prms_map_results.mod prms_statvar_out.mod prms_nhru_summary.mod prms_nsub_summary.mod prms_basin_summary.mod prms_nsegment_summary.mod prms_water_use.mod prms_dynamic_param_read.mod prms_glacr.mod prms_precip_map.mod prms_temp_map.mod prms_utils.mod
	$(FC) -c $(FFLAGS) sm_read_control_file.f90

c_read_data_file.o: c_read_data_file.f90
	$(FC) -c $(FFLAGS) c_read_data_file.f90

sm_read_data_file.o: sm_read_data_file.f90 prms_data_file.mod prms_module.mod prms_constants.mod prms_mmfapi.mod prms_set_time.mod prms_obs.mod prms_climatevars.mod prms_utils.mod
	$(FC) -c $(FFLAGS) sm_read_data_file.f90

c_read_parameter_file.o: c_read_parameter_file.f90
	$(FC) -c $(FFLAGS) c_read_parameter_file.f90

sm_read_parameter_file.o: sm_read_parameter_file.f90 prms_read_param_file.mod prms_module.mod prms_constants.mod prms_mmfapi.mod prms_utils.mod prms_control_file.mod
	$(FC) -c $(FFLAGS) sm_read_parameter_file.f90

c_utils_prms.o: c_utils_prms.f90
	$(FC) -c $(FFLAGS) c_utils_prms.f90

prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_gwflow.mod: gwflow.o
prms_module.mod:
	$(CD) ../gsflow;make prms_module.mod
	$(CP) ../gsflow/prms_module.mod .

prms_control_file.mod: c_read_control_file.o
prms_data_file.mod: c_read_data_file.o
prms_obs.mod: obs.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
prms_muskingum.mod: muskingum.o
prms_intcp.mod: intcp.o
prms_snow.mod: snowcomp.o
prms_cascade.mod: cascade.o
prms_srunoff.mod: srunoff.o
prms_soilzone.mod: soilzone.o
prms_soilzone_ag.mod: soilzone_ag.o
prms_routing.mod: routing.o
prms_water_use.mod: water_use_read.o
prms_set_time.mod: c_prms_time.o
prms_glacr.mod: glacr_melt.o
prms_constants.mod: prms_constants.o
prms_climate_hru.mod: climate_hru.o
prms_potet_jh.mod: potet_jh.o
prms_potet_pm.mod: potet_pm.o
prms_potet_pt.mod: potet_pt.o
prms_potet_hs.mod: potet_hs.o
prms_potet_hamon.mod: potet_hamon.o
prms_mmfapi.mod: c_mmf_utils.o
prms_read_param_file.mod: c_read_parameter_file.o
prms_utils.mod: c_utils_prms.o
prms_it0_vars.mod: climateflow.o

gsfmodflow.mod: $(GSFLOWDIR)/gsfmodflow.mod
	$(RM) gsfmodflow.mod
	$(LN) $(GSFLOWDIR)/gsfmodflow.mod gsfmodflow.mod

gsfmodsim2prms.mod:
	$(CD) $(GSFLOWDIR);make gsfmodsim2prms.mod
	$(CP) $(GSFLOWDIR)/gsfmodsim2prms.mod .
