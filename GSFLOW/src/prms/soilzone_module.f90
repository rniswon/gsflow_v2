!***********************************************************************
! FORTRAN module shared by soilzone and soilzone_ag
!***********************************************************************
      MODULE PRMS_SOILZONE
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Soilzone Computations'
      character(len=8), parameter :: MODNAME = 'soilzone'
      character(len=*), parameter :: Version_soilzone = '2021-09-14'
      INTEGER, SAVE :: DBGUNT, Soil_iter, Iter_aet_PRMS_flag !, HRU_id
      INTEGER, SAVE :: Max_gvrs, Et_type, Pref_flag
      REAL, SAVE, ALLOCATABLE :: Gvr2pfr(:), Swale_limit(:)
      REAL, SAVE, ALLOCATABLE :: Soil_lower_stor_max(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_ante(:), Ssres_stor_ante(:)
      REAL, SAVE, ALLOCATABLE :: Grav_dunnian_flow(:), Pfr_dunnian_flow(:)
      DOUBLE PRECISION, SAVE :: Last_soil_moist, Last_ssstor
!   GSFLOW variables
      INTEGER, SAVE, ALLOCATABLE :: Hru_gvr_count(:), Hru_gvr_index(:, :), Hrucheck(:)
      REAL, SAVE, ALLOCATABLE :: Replenish_frac(:)
      REAL, SAVE, ALLOCATABLE :: It0_soil_rechr(:), It0_soil_moist(:)
      REAL, SAVE, ALLOCATABLE :: It0_pref_flow_stor(:), It0_ssres_stor(:)
      REAL, SAVE, ALLOCATABLE :: It0_gravity_stor_res(:), It0_sroff(:)
      REAL, SAVE, ALLOCATABLE :: It0_slow_stor(:), It0_potet(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: It0_strm_seg_in(:)
      DOUBLE PRECISION, SAVE :: Basin_sz_gwin
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gvr_hru_pct_adjusted(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sz2gw, Basin_cap_infil_tot
      DOUBLE PRECISION, SAVE :: Basin_interflow_max, Basin_sm2gvr_max ! this is the same as basin_sm2gvr
      DOUBLE PRECISION, SAVE :: Basin_soil_rechr, Basin_dunnian_gvr
      DOUBLE PRECISION, SAVE :: Basin_recharge, Basin_pref_flow_infil
      DOUBLE PRECISION, SAVE :: Basin_ssin, Basin_dunnian_pfr
      DOUBLE PRECISION, SAVE :: Basin_sm2gvr, Basin_dninterflow
      DOUBLE PRECISION, SAVE :: Basin_dncascadeflow, Basin_dndunnianflow
      DOUBLE PRECISION, SAVE :: Basin_capwaterin, Basin_dunnian
      DOUBLE PRECISION, SAVE :: Basin_gvr2pfr, Basin_slowflow
      DOUBLE PRECISION, SAVE :: Basin_pref_stor, Basin_slstor, Basin_prefflow
      DOUBLE PRECISION, SAVE :: Basin_lakeinsz, Basin_lakeprecip
      DOUBLE PRECISION, SAVE :: Basin_cap_up_max
      DOUBLE PRECISION, SAVE :: Basin_soil_moist_tot
      DOUBLE PRECISION, SAVE :: Basin_soil_lower_stor_frac, Basin_soil_rechr_stor_frac, Basin_sz_stor_frac
      DOUBLE PRECISION, SAVE :: Basin_cpr_stor_frac, Basin_gvr_stor_frac, Basin_pfr_stor_frac
      REAL, SAVE, ALLOCATABLE :: Perv_actet(:), Pref_flow_thrsh(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_tot(:), Recharge(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Upslope_interflow(:), Upslope_dunnianflow(:), Lakein_sz(:)
      REAL, SAVE, ALLOCATABLE :: Dunnian_flow(:), Cap_infil_tot(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_stor(:), Pref_flow(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_infil(:), Pref_flow_in(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sz_cascadeflow(:), Swale_actet(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_max(:), Snow_free(:)
      REAL, SAVE, ALLOCATABLE :: Cap_waterin(:), Soil_lower(:), Soil_zone_max(:)
      REAL, SAVE, ALLOCATABLE :: Potet_lower(:), Potet_rechr(:), Soil_lower_ratio(:)
      REAL, SAVE, ALLOCATABLE :: Unused_potet(:)
      INTEGER, SAVE, ALLOCATABLE :: Soil_saturated(:)
!      REAL, SAVE, ALLOCATABLE :: Cascade_interflow(:), Cascade_dunnianflow(:), Interflow_max(:)
!      REAL, SAVE, ALLOCATABLE :: Cpr_stor_frac(:), Pfr_stor_frac(:), Gvr_stor_frac(:), Soil_moist_frac(:)
!      REAL, SAVE, ALLOCATABLE :: Soil_rechr_ratio(:), Snowevap_aet_frac(:), Perv_avail_et(:), Cap_upflow_max(:)
!   GSFLOW Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_gvr2sm
      REAL, SAVE, ALLOCATABLE :: Sm2gw_grav(:), Gw2sm_grav(:)
      REAL, SAVE, ALLOCATABLE :: Gravity_stor_res(:), Gvr2sm(:), Grav_gwin(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Soil_type(:), Gvr_hru_id(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_den(:), Pref_flow_infil_frac(:)
      REAL, SAVE, ALLOCATABLE :: Fastcoef_lin(:), Fastcoef_sq(:)
      REAL, SAVE, ALLOCATABLE :: Slowcoef_lin(:), Slowcoef_sq(:)
      REAL, SAVE, ALLOCATABLE :: Ssr2gw_rate(:), Ssr2gw_exp(:)
      REAL, SAVE, ALLOCATABLE :: Soil2gw_max(:)
      REAL, SAVE, ALLOCATABLE :: Lake_evap_adj(:, :)
      END MODULE PRMS_SOILZONE
