!***********************************************************************
! FORTRAN module shared by soilzone and soilzone_ag
!***********************************************************************
      MODULE PRMS_SOILZONE_AG
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC_AG = 'Soilzone Computations'
      character(len=11), parameter :: MODNAME_AG = 'soilzone_ag'
      character(len=*), parameter :: Version_soilzone_ag = '2021-09-30'
      INTEGER, SAVE :: Soil_iter, Iter_aet_PRMS_flag, HRU_id
!   Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_saturated(:)
!   GSFLOW Declared Variables
      REAL, SAVE, ALLOCATABLE :: Ag_replenish_frac(:)
!   Declared Parameters
      INTEGER, SAVE :: max_soilzone_ag_iter
      REAL, SAVE :: soilzone_aet_converge
      ! AG variables and parameters
      DOUBLE PRECISION, SAVE :: Basin_ag_soil_to_gw, Basin_ag_up_max, Basin_ag_gvr2sm
      DOUBLE PRECISION, SAVE :: Basin_ag_actet, Last_ag_soil_moist, Basin_ag_soil_rechr, Basin_agwaterin
      !DOUBLE PRECISION, SAVE :: Basin_ag_ssstor, Basin_ag_recharge, Basin_ag_ssflow
      REAL, SAVE, ALLOCATABLE :: Ag_soil_to_gw(:), Ag_soil_to_ssr(:), Ag_hortonian(:), Unused_ag_et(:), Ag_soil2gvr(:)
      REAL, SAVE, ALLOCATABLE :: Ag_actet(:), Ag_dunnian(:), Ag_irrigation_add(:), Ag_gvr2sm(:), Ag_irrigation_add_vol(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ag_upslope_dunnian(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_lower(:), Ag_soil_lower_stor_max(:), Ag_potet_rechr(:), Ag_potet_lower(:)
      REAL, SAVE, ALLOCATABLE :: It0_ag_soil_rechr(:), It0_ag_soil_moist(:), Ag_soilwater_deficit(:)
      !REAL, SAVE, ALLOCATABLE :: Ag_slow_flow(:), Ag_ssres_in(:) !, Ag_water_maxin(:)
      !REAL, SAVE, ALLOCATABLE :: Ag_ssr_to_gw(:), Ag_slow_stor(:), Ag_recharge(:)
      !REAL, SAVE, ALLOCATABLE :: Ag_ssres_stor(:), Ag_ssres_flow(:)
      integer, save :: total_iters, iter_nonconverge
      real, save :: unsatisfied_big
      ! parameters
! have covden a monthly, later
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_type(:), Ag_soilwater_deficit_min(:), Ag_covden_sum(:), Ag_covden_win(:) !, Ag_crop_type(:)
!      REAL, SAVE, ALLOCATABLE :: Ag_sat_threshold(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_rechr_max_frac(:) ! Ag_crop_coef later, will specify PET
      !REAL, SAVE, ALLOCATABLE :: Ag_snowinfil_max(:), Ag_ssstor_init_frac(:)

      END MODULE PRMS_SOILZONE_AG
