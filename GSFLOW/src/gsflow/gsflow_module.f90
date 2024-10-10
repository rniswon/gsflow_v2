!***********************************************************************
!     Modules PRMS_MODULE and GSFMODFLOW
!***********************************************************************
      MODULE PRMS_MODULE
      USE PRMS_CONSTANTS
      IMPLICIT NONE
      character(LEN=*), parameter :: &
     &          EQULS = '=========================================================================='
      character(len=*), parameter :: MODDESC = 'PRMS Computation Order'
      character(len=11), parameter :: MODNAME = 'gsflow_prms'
      character(len=*), parameter :: GSFLOW_versn = '2.4.0 09/01/2024'
      character(len=*), parameter :: PRMS_versn = '2024-09-01'
      character(len=*), parameter :: PRMS_VERSION = 'Version 6.0.0 09/01/2024'
      character(len=*), parameter :: githash = 'Github Commit Hash 87c870f3a2809a167698e4f88a354886415f6afa'
      character(len=*), parameter :: Version_read_control_file = '2024-08-01'
      character(len=*), parameter :: Version_read_parameter_file = '2024-08-27'
      character(len=*), parameter :: Version_read_data_file = '2023-06-02'
      CHARACTER(LEN=8), SAVE :: Process
! Dimensions
      INTEGER, SAVE :: Nratetbl, Nwateruse, Nexternal, Nconsumed, Npoigages, Ncascade, Ncascdgw, Ncbh
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Nhrucell, Nlake, Ngwcell, Nlake_hrus, Nmonths
      INTEGER, SAVE :: Ntemp, Nrain, Nsol, Nsegment, Ndepl, Nobs, Nevap, Ndeplval, Nmap2hru, Nmap, Nsnow
      INTEGER, SAVE :: NLAKES_MF, Nreach
! Global
      ! Model (0=GSFLOW; 1=PRMS; 2=MODFLOW; 3=MODSIM-PRMS; 4=MODSIM-PRMS-LOOSE; 10=MODSIM-GSFLOW; 12=MODSIM-MODFLOW; 13=MODSIM)
      INTEGER, SAVE :: Model, Process_flag, Call_cascade
      INTEGER, SAVE :: Start_year, Start_month, Start_day, End_year, End_month, End_day
      INTEGER, SAVE :: Transp_flag, Sroff_flag, Solrad_flag, Et_flag
      INTEGER, SAVE :: Climate_temp_flag, Climate_precip_flag, Climate_potet_flag, Climate_transp_flag
      INTEGER, SAVE :: Lake_route_flag, Strmflow_flag, Stream_order_flag
      INTEGER, SAVE :: Temp_flag, Precip_flag, Climate_hru_flag, Climate_swrad_flag
      INTEGER, SAVE :: Precip_combined_flag, Temp_combined_flag, Muskingum_flag
      INTEGER, SAVE :: Inputerror_flag, Timestep
      INTEGER, SAVE :: Humidity_cbh_flag, Windspeed_cbh_flag, Albedo_cbh_flag, Cloud_cover_cbh_flag
      INTEGER, SAVE :: PRMS4_flag, PRMS6_flag
      INTEGER, SAVE :: PRMS_flag, GSFLOW_flag, PRMS_only, Gsf_unt
      INTEGER, SAVE :: Kper_mfo, Kkstp_mfo, Have_lakes, Grid_flag, Ag_package, MODSIM_flag, AG_flag, gwflow_flag
      INTEGER, SAVE :: Canopy_iter, irrigated_area_cbh_flag, AET_cbh_flag, PET_cbh_flag
      INTEGER, SAVE :: PRMS_output_unit, Restart_inunit, Restart_outunit
      INTEGER, SAVE :: Dynamic_flag, Water_use_flag, Soilzone_add_water_use
      INTEGER, SAVE :: Elapsed_time_start(8), Elapsed_time_end(8), Elapsed_time_minutes
      INTEGER, SAVE :: Nowyear, Nowmonth, Nowday
      INTEGER, SAVE :: Gwr_transfer_water_use, Gwr_add_water_use
      INTEGER, SAVE :: Lake_transfer_water_use, Lake_add_water_use
      REAL, SAVE :: Execution_time_start, Execution_time_end, Elapsed_time
      INTEGER, SAVE :: Dynamic_soil_flag
      INTEGER, SAVE :: mf_timestep, startday, endday, mf_nowtime, Number_timesteps, timestep_start_flag
      INTEGER, SAVE :: num_words_command_line
      character(LEN=MAXCMDLINE_LENGTH) command_line, command_line_modsim
!   Declared Variables
      INTEGER, SAVE :: Kkiter
!   Declared Variables for DPRST agriculture computations
      REAL, SAVE, ALLOCATABLE :: Dprst_ag_transfer(:), Dprst_ag_gain(:), Hru_ag_irr(:)
!   Declared Parameters
      INTEGER, SAVE :: Mxsziter
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:), Hru_type(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 9=precip_map
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 8=temp_sta; 9=temp_map
! Control parameters
      INTEGER, SAVE :: Starttime(6), Endtime(6)
      INTEGER, SAVE :: Print_debug, MapOutON_OFF, CsvON_OFF, Dprst_flag, Subbasin_flag, Parameter_check_flag
      INTEGER, SAVE :: Init_vars_from_file, Save_vars_to_file, Orad_flag, Cascade_flag, Cascadegw_flag
      INTEGER, SAVE :: NhruOutON_OFF, Gwr_swale_flag, NsubOutON_OFF, BasinOutON_OFF, NsegmentOutON_OFF
      INTEGER, SAVE :: Stream_temp_flag, Strmtemp_humidity_flag, Stream_temp_shade_flag
      INTEGER, SAVE :: Prms_warmup, statsON_OFF
      INTEGER, SAVE :: Frozen_flag, Glacier_flag, one_subbasin_flag
      INTEGER, SAVE :: PRMS_land_iteration_flag, Iter_aet_flag, text_restart_flag
      INTEGER, SAVE :: irrigation_apply_flag, Dyn_ag_frac_flag, Dyn_ag_soil_flag, activeHRU_inactiveCELL_flag
      INTEGER, SAVE :: Dprst_add_water_use, Dprst_transfer_water_use
      INTEGER, SAVE :: Snarea_curve_flag, Soilzone_aet_flag, snow_cloudcover_flag
      INTEGER, SAVE :: seg2hru_flag, snow_flag, forcing_check_flag
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Model_output_file, Var_init_file, Var_save_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Gsflow_output_file, selectDatesFileName, Data_file, Stat_var_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Csv_output_file, Model_control_file, Param_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: mappingFileName, xyFileName
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Temp_module, Srunoff_module, Et_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Strmflow_module, Transp_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Model_mode, Precip_module, Solrad_module
      CHARACTER(LEN=11), SAVE :: Soilzone_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: irrigated_area_module, AET_module, PET_ag_module
      INTEGER, SAVE :: Dyn_imperv_flag, Dyn_intcp_flag, Dyn_covden_flag, Dyn_covtype_flag, Dyn_transp_flag, Dyn_potet_flag
      INTEGER, SAVE :: Dyn_soil_flag, Dyn_radtrncf_flag, Dyn_dprst_flag,  Dprst_transferON_OFF
      INTEGER, SAVE :: Dyn_snareathresh_flag, Dyn_transp_on_flag
      INTEGER, SAVE :: Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Dyn_fallfrost_flag, Dyn_springfrost_flag
      INTEGER, SAVE :: Gwr_transferON_OFF, External_transferON_OFF, Segment_transferON_OFF, Lake_transferON_OFF
      INTEGER, SAVE :: Gsf_rpt, Rpt_days
      END MODULE PRMS_MODULE

    MODULE GSFMODFLOW
      USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW MODFLOW main'
      character(len=14), parameter :: MODNAME = 'gsflow_modflow'
      character(len=*), parameter :: Version_gsflow_modflow='2024-06-01'
      character(len=*), parameter :: MODDESC_UZF = 'UZF-NWT Package'
      character(len=*), parameter :: MODDESC_SFR = 'SFR-NWT Package'
      character(len=*), parameter :: MODDESC_LAK = 'LAK-NWT Package'
      character(len=*), parameter :: MODDESC_AG =  'AG-NWT Package'
      character(len=*), parameter :: MODNAME_UZF = 'gwf2uzf1_NWT'
      character(len=*), parameter :: MODNAME_SFR = 'gwf2sfr7_NWT'
      character(len=*), parameter :: MODNAME_LAK = 'gwf2lak7_NWT'
      character(len=*), parameter :: MODNAME_AG =  'gwf2ag1_NWT_ponds'
      character(len=*), parameter :: Version_uzf = '2022-10-18'
      character(len=*), parameter :: Version_sfr = '2022-10-18'
      character(len=*), parameter :: Version_lak = '2021-09-28'
      character(len=*), parameter :: Version_ag =  '2022-10-18'
      INTEGER, PARAMETER :: ITDIM = 80
      INTEGER, SAVE :: Convfail_cnt, Steady_state, Ncells, Gsflag
      INTEGER, SAVE :: IGRID, KKPER, ICNVG, NSOL, IOUTS, KPERSTART
      INTEGER, SAVE :: AGCONVERGE
      INTEGER, SAVE :: KSTP, KKSTP, IERR, Max_iters, Itreal
      INTEGER, SAVE :: Mfiter_cnt(ITDIM), Iter_cnt(ITDIM), Iterations
      INTEGER, SAVE :: Szcheck, Sziters, INUNIT, KPER, NCVGERR
      INTEGER, SAVE :: Max_sziters, Maxgziter, ITREAL2, mf_julday
      INTEGER, SAVE, ALLOCATABLE :: Gwc_col(:), Gwc_row(:)
      REAL, SAVE :: Delt_save
      INTEGER, SAVE, ALLOCATABLE :: Stress_dates(:)
      INTEGER, SAVE :: Modflow_skip_stress, Kkper_new
      INTEGER, SAVE :: Modflow_skip_time_step
      DOUBLE PRECISION, SAVE :: Modflow_time_in_stress, Modflow_skip_time
      DOUBLE PRECISION, SAVE :: Mft_to_sec, Totalarea_mf
      DOUBLE PRECISION, SAVE :: Mfl2_to_acre, Mfl3_to_ft3, Sfr_conv
      DOUBLE PRECISION, SAVE :: Acre_inches_to_mfl3, Mfl3t_to_cfs, MFQ_to_inch_acres_dble
      REAL, SAVE :: Mft_to_days, Mfl_to_inch, Inch_to_mfl_t
      REAL, SAVE :: MFQ_to_inch_acres, Acre_inches_to_mfl3_sngl
      DOUBLE PRECISION, SAVE :: mfstrt_jul  !RGN to get MF to stop at End_time for MODFLOW only
      REAL, SAVE, ALLOCATABLE :: Mfq2inch_conv(:), Cellarea(:)
      REAL, SAVE, ALLOCATABLE :: Gvr2cell_conv(:), Mfvol2inch_conv(:)
      INTEGER, SAVE :: Stopcount
      REAL(8), DIMENSION(5) :: DIVS
!-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION,VERSION2,VERSION3
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='1.3.0 06/01/2022')
      PARAMETER (VERSION2='1.12.0 02/03/2017')
      PARAMETER (VERSION3='1.04.0 09/15/2016')
      PARAMETER (MFVNAM='-NWT-SWR1')
      INTEGER, SAVE :: IBDT(8)
!   Control Parameters
      INTEGER, SAVE :: Modflow_time_zero(6)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Modflow_name
    END MODULE GSFMODFLOW
