!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a
! non-linear (smidx) and linear (carea) variable-source-area method
! Combinded smidx and carea modules 3/12/2013
!
!     version: 2.1 combined with srunoff_cfgi.f from M.C.Mastin
!          includes glacr_melt for HRU's w/ glaciers,IE: hru_type(HRU#)=4
!          modified 6/98 by JJ Vaccaro
!      MODIFIED 5/00 by JJ Vaccaro to account for irrigation application
!      MODIFIED 9/04 by M.C.Mastin to include a Continuous Frozen Ground
!         Index (cfgi) that shuts off infiltration once the index reaches
!         a threshold value (parameter cfgi_thrshld). A daily decay of
!         the index is defined by cfgi_decay parameter.
!     version: 2.2 added cascading flow for infiltration and runoff
!
! rsr, 04/15/2020 removed depression storage code
! rsr, 10/1/2008 added Vaccaro code
! rsr, 10/21/2008 added frozen ground code
! rsr, 10/30/2008 added depression storage code
! rsr, 04/11/2011 changed so dprst_area to be a parameter (does not change)
! rsr, 07/1/2013 combined smidx and carea into one module
!***********************************************************************
      MODULE PRMS_SRUNOFF
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=13), SAVE :: MODNAME
      INTEGER, SAVE :: Ihru
      REAL, SAVE, ALLOCATABLE :: Carea_dif(:), Imperv_stor_ante(:)
      REAL, SAVE :: Srp, Sri, Perv_frac, Imperv_frac, Hruarea_imperv, Hruarea
      DOUBLE PRECISION, SAVE :: Hruarea_dble, Basin_apply_sroff, Basin_cfgi_sroff
      INTEGER, SAVE :: Use_sroff_transfer
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sroff_down, Basin_sroff_upslope
      DOUBLE PRECISION, SAVE :: Basin_sroffi, Basin_sroffp
      DOUBLE PRECISION, SAVE :: Basin_imperv_stor, Basin_imperv_evap, Basin_infil
      DOUBLE PRECISION, SAVE :: Basin_hortonian, Basin_hortonian_lakes, Basin_contrib_fraction
      REAL, SAVE, ALLOCATABLE :: Contrib_fraction(:)
      REAL, SAVE, ALLOCATABLE :: Imperv_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sroffp(:), Hru_sroffi(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Upslope_hortonian(:)
      REAL, SAVE, ALLOCATABLE :: Hortonian_flow(:)
      REAL, SAVE, ALLOCATABLE :: Hru_impervevap(:), Hru_impervstor(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Strm_seg_in(:), Hortonian_lakes(:), Hru_hortn_cascflow(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Smidx_coef(:), Smidx_exp(:)
      REAL, SAVE, ALLOCATABLE :: Carea_min(:), Carea_max(:)
!   Declared Parameters for Frozen Ground
      REAL, SAVE :: Cfgi_thrshld, Cfgi_decay
!   Declared Variables for Frozen Ground
      REAL, SAVE, ALLOCATABLE :: Cfgi(:), Cfgi_prev(:)
      INTEGER, SAVE, ALLOCATABLE :: Frozen(:)
      END MODULE PRMS_SRUNOFF

!***********************************************************************
!     Main srunoff routine
!***********************************************************************
      INTEGER FUNCTION srunoff()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srunoffdecl, srunoffinit, srunoffrun
      EXTERNAL :: srunoff_restart
!***********************************************************************
      srunoff = 0

      IF ( Process(:3)=='run' ) THEN
        srunoff = srunoffrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        srunoff = srunoffdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file>0 ) CALL srunoff_restart(1)
        srunoff = srunoffinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL srunoff_restart(0)
      ENDIF

      END FUNCTION srunoff

!***********************************************************************
!     srunoffdecl - set up parameters for surface runoff computations
!   Declared Parameters
!     smidx_coef, smidx_exp, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max, soil_rechr_max, carea_min
!     cfgi_thrshld, cfgi_decay
!***********************************************************************
      INTEGER FUNCTION srunoffdecl()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Model, Nhru, Nsegment, Print_debug, DOCUMENTATION, &
     &    Cascade_flag, Sroff_flag, Nlake, Call_cascade, Frozen_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam
      EXTERNAL read_error, print_module, declvar_dble, declvar_real, declvar_int
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_srunoff
!***********************************************************************
      srunoffdecl = 0

      Version_srunoff = 'srunoff.f90 2020-04-22 16:42:00Z'
      IF ( Sroff_flag==1 ) THEN
        MODNAME = 'srunoff_smidx'
      ELSE
        MODNAME = 'srunoff_carea'
      ENDIF
      Version_srunoff = MODNAME//'.f90 '//Version_srunoff(13:80)
      CALL print_module(Version_srunoff, 'Surface Runoff              ', 90)

      CALL declvar_dble(MODNAME, 'basin_imperv_evap', 'one', 1, 'double', &
     &     'Basin area-weighted average evaporation from impervious area', &
     &     'inches', Basin_imperv_evap)

      CALL declvar_dble(MODNAME, 'basin_imperv_stor', 'one', 1, 'double', &
     &     'Basin area-weighted average storage on impervious area', &
     &     'inches', Basin_imperv_stor)

      CALL declvar_dble(MODNAME, 'basin_infil', 'one', 1, 'double', &
     &     'Basin area-weighted average infiltration to the capillary reservoirs', &
     &     'inches', Basin_infil)

      CALL declvar_dble(MODNAME, 'basin_hortonian', 'one', 1, 'double', &
     &     'Basin area-weighted average Hortonian runoff', &
     &     'inches', Basin_hortonian)

      CALL declvar_dble(MODNAME, 'basin_contrib_fraction', 'one', 1, 'double', &
     &     'Basin area-weighted average contributing area of the pervious area of each HRU', &
     &     'decimal fraction', Basin_contrib_fraction)

      ALLOCATE ( Contrib_fraction(Nhru) )
      CALL declvar_real(MODNAME, 'contrib_fraction', 'nhru', Nhru, 'real', &
     &     'Contributing area of each HRU pervious area', &
     &     'decimal fraction', Contrib_fraction)

      ALLOCATE ( Hru_impervevap(Nhru) )
      CALL declvar_real(MODNAME, 'hru_impervevap', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average evaporation from impervious area for each HRU', &
     &     'inches', Hru_impervevap)

      ALLOCATE ( Hru_impervstor(Nhru) )
      CALL declvar_real(MODNAME, 'hru_impervstor', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average storage on impervious area for each HRU', &
     &     'inches', Hru_impervstor)

      ALLOCATE ( Imperv_evap(Nhru) )
      CALL declvar_real(MODNAME, 'imperv_evap', 'nhru', Nhru, 'real', &
     &     'Evaporation from impervious area for each HRU', &
     &     'inches', Imperv_evap)

      CALL declvar_dble(MODNAME, 'basin_sroffi', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff from impervious areas', &
     &     'inches', Basin_sroffi)

      CALL declvar_dble(MODNAME, 'basin_sroffp', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff from pervious areas', &
     &     'inches', Basin_sroffp)

      ALLOCATE ( Hru_sroffp(Nhru) )
      CALL declvar_real(MODNAME, 'hru_sroffp', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average surface runoff from pervious areas for each HRU', &
     &     'inches', Hru_sroffp)

      ALLOCATE ( Hru_sroffi(Nhru) )
      CALL declvar_real(MODNAME, 'hru_sroffi', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average surface runoff from impervious areas for each HRU', &
     &     'inches', Hru_sroffi)

      ALLOCATE ( Hortonian_flow(Nhru) )
      CALL declvar_real(MODNAME, 'hortonian_flow', 'nhru', Nhru, 'real', &
     &     'Hortonian surface runoff reaching stream network for each HRU', &
     &     'inches', Hortonian_flow)

! cascading variables and parameters
      IF ( Cascade_flag>0 .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Upslope_hortonian(Nhru) )
        CALL declvar_dble(MODNAME, 'upslope_hortonian', 'nhru', Nhru, 'double', &
     &       'Hortonian surface runoff received from upslope HRUs', &
     &       'inches', Upslope_hortonian)

        CALL declvar_dble(MODNAME, 'basin_sroff_down', 'one', 1, 'double', &
     &       'Basin area-weighted average of cascading surface runoff', &
     &       'inches', Basin_sroff_down)

        CALL declvar_dble(MODNAME, 'basin_sroff_upslope', 'one', 1, 'double', &
     &       'Basin area-weighted average of cascading surface runoff received from upslope HRUs', &
     &       'inches', Basin_sroff_upslope)

        ALLOCATE ( Hru_hortn_cascflow(Nhru) )
        CALL declvar_dble(MODNAME, 'hru_hortn_cascflow', 'nhru', Nhru, 'double', &
     &       'Cascading Hortonian surface runoff leaving each HRU', &
     &       'inches', Hru_hortn_cascflow)

        IF ( Nlake>0 ) THEN
          CALL declvar_dble(MODNAME, 'basin_hortonian_lakes', 'one', 1, 'double', &
     &         'Basin area-weighted average Hortonian surface runoff to lakes', &
     &         'inches', Basin_hortonian_lakes)

          ALLOCATE ( Hortonian_lakes(Nhru) )
          CALL declvar_dble(MODNAME, 'hortonian_lakes', 'nhru', Nhru, 'double', &
     &         'Surface runoff to lakes for each HRU', &
     &         'inches', Hortonian_lakes)
        ENDIF
      ENDIF

      IF ( Call_cascade==1 .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Strm_seg_in(Nsegment) )
        CALL declvar_dble(MODNAME, 'strm_seg_in', 'nsegment', Nsegment, 'double', &
     &       'Flow in stream segments as a result of cascading flow in each stream segment', &
     &       'cfs', Strm_seg_in)
      ENDIF

! frozen ground variables and parameters
      ALLOCATE ( Frozen(Nhru) )
      IF ( Frozen_flag==1 .OR. Model==DOCUMENTATION ) THEN
        CALL declvar_int(MODNAME, 'frozen', 'nhru', Nhru, 'integer', &
     &       'Flag for frozen ground (0=no; 1=yes)', &
     &       'none', Frozen)

        ALLOCATE ( Cfgi(Nhru) )
        CALL declvar_real(MODNAME, 'cfgi', 'nhru', Nhru, 'real', &
     &       'Continuous Frozen Ground Index', &
     &       'index', Cfgi)

        ALLOCATE ( Cfgi_prev(Nhru) )
        CALL declvar_real(MODNAME, 'cfgi_prev', 'nhru', Nhru, 'real', &
     &       'Continuous Frozen Ground Index from previous day', &
     &       'index', Cfgi_prev)

        IF ( declparam(MODNAME, 'cfgi_decay', 'one', 'real', &
     &       '0.97', '0.01', '1.0', &
     &       'CFGI daily decay of index, value of 1.0 is no decay', &
     &       'CFGI daily decay of index, value of 1.0 is no decay', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'cfgi_decay')

        IF ( declparam(MODNAME, 'cfgi_thrshld', 'one', 'real', &
     &       '52.55', '1.0', '500.0', &
     &       'CFGI threshold value indicating frozen soil', &
     &       'CFGI threshold value indicating frozen soil', &
     &       'index')/=0 ) CALL read_error(1, 'cfgi_thrshld')
      ENDIF

! Declare parameters
      IF ( Sroff_flag==1 .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Smidx_coef(Nhru) )
        IF ( declparam(MODNAME, 'smidx_coef', 'nhru', 'real', &
     &       '0.005', '0.0', '1.0', &
     &       'Coefficient in contributing area computations', &
     &       'Coefficient in non-linear contributing area algorithm for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'smidx_coef')
        ALLOCATE ( Smidx_exp(Nhru) )
        IF ( declparam(MODNAME, 'smidx_exp', 'nhru', 'real', &
     &       '0.3', '0.0', '5.0', &
     &       'Exponent in contributing area computations', &
     &       'Exponent in non-linear contributing area algorithm for each HRU', &
     &       '1.0/inch')/=0 ) CALL read_error(1, 'smidx_exp')
      ENDIF

      IF ( Sroff_flag==2 .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Carea_min(Nhru), Carea_dif(Nhru) )
        IF ( declparam(MODNAME, 'carea_min', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Minimum contributing area', &
     &       'Minimum possible area contributing to surface runoff expressed as a portion of the area for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'carea_min')
      ENDIF

      ALLOCATE ( Carea_max(Nhru) )
      IF ( declparam(MODNAME, 'carea_max', 'nhru', 'real', &
     &     '0.6', '0.0', '1.0', &
     &     'Maximum contributing area', &
     &     'Maximum possible area contributing to surface runoff expressed as a portion of the HRU area', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'carea_max')

      IF ( Print_debug==1 ) ALLOCATE ( Imperv_stor_ante(Nhru) )

      END FUNCTION srunoffdecl

!***********************************************************************
!     srunoffinit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srunoffinit()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Cascade_flag, Sroff_flag, &
     &    Init_vars_from_file, Call_cascade, Water_use_flag, Print_debug, Frozen_flag !, Parameter_check_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
!      USE PRMS_FLOWVARS, ONLY: Soil_moist_max
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, j !, k, num_hrus
!      REAL :: frac
!***********************************************************************
      srunoffinit = 0

      Use_sroff_transfer = 0
      IF ( Water_use_flag==1 ) Use_sroff_transfer = 1

      Imperv_evap = 0.0
      Hortonian_flow = 0.0
      Hru_sroffi = 0.0
      Hru_sroffp = 0.0
      Contrib_fraction = 0.0
      Hru_impervevap = 0.0
      IF ( Call_cascade==1 ) Strm_seg_in = 0.0D0
      IF ( Cascade_flag>0 ) THEN
        Upslope_hortonian = 0.0D0
        Hru_hortn_cascflow = 0.0D0
        IF ( Nlake>0 ) Hortonian_lakes = 0.0D0
      ENDIF

      IF ( Init_vars_from_file==0 ) THEN
        Basin_sroffi = 0.0D0
        Basin_sroffp = 0.0D0
        Basin_infil = 0.0D0
        Basin_imperv_evap = 0.0D0
        Basin_imperv_stor = 0.0D0
        Basin_hortonian = 0.0D0
        Basin_sroff_upslope = 0.0D0
        Basin_sroff_down = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Basin_contrib_fraction = 0.0D0
        Hru_impervstor = 0.0
        Srp = 0.0
        Sri = 0.0

        Frozen = 0
        IF ( Frozen_flag==1 ) THEN
          Cfgi = 0.0
          Cfgi_prev = 0.0
        ENDIF
      ENDIF

      IF ( getparam(MODNAME, 'carea_max', Nhru, 'real', Carea_max)/=0 ) CALL read_error(2, 'carea_max')

      IF ( Sroff_flag==1 ) THEN
! Smidx parameters
        IF ( getparam(MODNAME, 'smidx_coef', Nhru, 'real', Smidx_coef)/=0 ) CALL read_error(2, 'smidx_coef')
        IF ( getparam(MODNAME, 'smidx_exp', Nhru, 'real', Smidx_exp)/=0 ) CALL read_error(2, 'smidx_exp')
      ELSE !IF ( Sroff_flag==2 ) THEN
! Carea parameters
        IF ( getparam(MODNAME, 'carea_min', Nhru, 'real', Carea_min)/=0 ) CALL read_error(2, 'carea_min')
        Carea_dif = 0.0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Carea_dif(i) = Carea_max(i) - Carea_min(i)
        ENDDO
      ENDIF

!      num_hrus = 0
!      DO j = 1, Active_hrus
!        i = Hru_route_order(j)
!        IF ( Sroff_flag==2 ) THEN
!          Carea_dif(i) = Carea_max(i) - Carea_min(i)
!        ELSEIF ( Parameter_check_flag>0 ) THEN
!          frac = Smidx_coef(i)*10**(Soil_moist_max(i)*Smidx_exp(i))
!          k = 0
!          IF ( frac>2.0 ) k = 1
!          IF ( frac>Carea_max(i)*2.0 ) k = k + 2
!          IF ( k>0 ) THEN
!            num_hrus = num_hrus + 1
            !IF ( Print_debug>-1 ) THEN
            !  PRINT *, ' '
            !  PRINT *, 'WARNING'
            !  PRINT *, 'Contributing area based on smidx parameters and soil_moist_max:', frac
            !  IF ( k==1 .OR. k==3 ) PRINT *, 'Maximum contributing area > 200%'
            !  IF ( k>1 ) PRINT *, 'Maximum contributing area > carea_max:', Carea_max(i)
            !  PRINT *, 'HRU:', i, '; soil_moist_max:', Soil_moist_max(i)
            !  PRINT *, 'smidx_coef:', Smidx_coef(i), '; smidx_exp:', Smidx_exp(i)
            !  PRINT *, 'This can make smidx parameters insensitive and carea_max very sensitive'
            !ENDIF
!          ENDIF
!        ENDIF
!      ENDDO
!      IF ( num_hrus>0 .AND. Print_debug>-1 ) THEN
!        WRITE (*, '(/,A,/,9X,A,/,9X,A,I7,/,9X,A,/,9X,A,/)') &
!     &         'WARNING, maximum contributing area based on smidx coefficents and', &
!     &         'soil_moist_max are > 200% of the HRU area and/or > 2*carea_max', &
!     &         'number of HRUs for which this condition exists:', num_hrus, &
!     &         'This means the smidx parameters are insensitive and', &
!     &         'carea_max very sensitive for those HRUs'
!      ENDIF

! Frozen soil parameters
      IF ( Frozen_flag==1 ) THEN
        IF ( getparam(MODNAME, 'cfgi_thrshld', 1, 'real', Cfgi_thrshld)/=0 ) CALL read_error(2, 'cfgi_thrshld')
        IF ( getparam(MODNAME, 'cfgi_decay', 1, 'real', Cfgi_decay)/=0 ) CALL read_error(2, 'cfgi_decay')
      ENDIF

      IF ( Print_debug==1 ) Imperv_stor_ante = Hru_impervstor

      END FUNCTION srunoffinit

!***********************************************************************
!     srunoffrun - Computes surface runoff using contributing area
!                  computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srunoffrun()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Cascade_flag, Call_cascade, Print_debug, Frozen_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, &
     &    Hru_perv, Hru_imperv, Hru_percent_imperv, Hru_frac_perv, &
     &    Hru_area, Hru_type, Basin_area_inv, Hru_area_dble
      USE PRMS_CLIMATEVARS, ONLY: Potet, Tavgc
      USE PRMS_FLOWVARS, ONLY: Sroff, Infil, Imperv_stor, Pkwater_equiv, Imperv_stor_max, Snowinfil_max, Basin_sroff
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_INTCP, ONLY: Net_rain, Net_snow, Net_ppt, Hru_intcpevap, Net_apply, Intcp_changeover
      USE PRMS_SNOW, ONLY: Snow_evap, Snowcov_area, Snowmelt, Pk_depth
      IMPLICIT NONE
      INTRINSIC SNGL, DBLE
      EXTERNAL imperv_et, compute_infil, run_cascade_sroff, dprst_comp, perv_comp
! Local Variables
      INTEGER :: i, k, frzen
      REAL :: srunoff, avail_et, hperv, sra, availh2o
      DOUBLE PRECISION :: hru_sroff_down, runoff, apply_sroff, cfgi_sroff
      REAL :: cfgi_k, depth_cm !frozen ground
!***********************************************************************
      srunoffrun = 0

      IF ( Print_debug==1 ) Imperv_stor_ante = Hru_impervstor

      Basin_sroffi = 0.0D0
      Basin_sroffp = 0.0D0
      Basin_sroff = 0.0D0
      Basin_infil = 0.0D0
      Basin_imperv_evap = 0.0D0
      Basin_imperv_stor = 0.0D0
      Basin_hortonian = 0.0D0
      Basin_contrib_fraction = 0.0D0
      Basin_cfgi_sroff = 0.0D0
      Basin_apply_sroff = 0.0D0

      IF ( Call_cascade==1 ) Strm_seg_in = 0.0D0
      IF ( Cascade_flag>0 ) THEN
        Basin_sroff_down = 0.0D0
        Basin_sroff_upslope = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Upslope_hortonian = 0.0D0
      ENDIF

      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        Hruarea = Hru_area(i)
        Hruarea_dble = Hru_area_dble(i)
        Ihru = i
        runoff = 0.0D0

        IF ( Hru_type(i)==2 ) THEN
! HRU is a lake
!     eventually add code for lake area less than hru_area
!     that includes soil_moist for fraction of hru_area that is dry bank
          ! Sanity check
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i)>0.0 ) &
     &         PRINT *, 'srunoff lake ERROR', Infil(i), Sroff(i), Imperv_stor(i), Imperv_evap(i), i
          IF ( Cascade_flag>0 ) THEN
            Hortonian_lakes(i) = Upslope_hortonian(i)
            Basin_hortonian_lakes = Basin_hortonian_lakes + Hortonian_lakes(i)*Hruarea_dble
          ENDIF
          CYCLE
        ENDIF

        Infil(i) = 0.0
        hperv = Hru_perv(i)
        Perv_frac = Hru_frac_perv(i)
        Srp = 0.0
        Sri = 0.0
        Hru_sroffp(i) = 0.0
        Contrib_fraction(i) = 0.0
        Hruarea_imperv = Hru_imperv(i)
        IF ( Hruarea_imperv>0.0 ) THEN
          Imperv_frac = Hru_percent_imperv(i)
          Hru_sroffi(i) = 0.0
          Imperv_evap(i) = 0.0
          Hru_impervevap(i) = 0.0
        ENDIF

        avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)
        availh2o = Intcp_changeover(i) + Net_rain(i)

        frzen = 0
        IF ( Frozen_flag==1 ) THEN
          IF ( Tavgc(i)>0.0 ) THEN
            cfgi_k = 0.5
          ELSE
            cfgi_k = 0.08
          ENDIF
          depth_cm = SNGL(Pk_depth(i))*2.54 !depth of snow cover averaged over HRU
          Cfgi(i) = Cfgi_decay*Cfgi_prev(i) - Tavgc(i)*( 2.71828**(-0.4*cfgi_k*depth_cm) )
          IF ( Cfgi(i)<0.0 ) Cfgi(i) = 0.0
          Cfgi_prev(i) = Cfgi(i)
          IF ( Cfgi(i)>=Cfgi_thrshld ) THEN
            frzen = 1
            ! depression storage states are not changed if frozen
            IF ( Cascade_flag>0 ) THEN
              cfgi_sroff = (Snowmelt(i) + availh2o + Upslope_hortonian(i))*Hruarea
            ELSE
              cfgi_sroff = (Snowmelt(i) + availh2o)*Hruarea
            ENDIF
            IF ( Use_sroff_transfer==1 ) cfgi_sroff = cfgi_sroff + Net_apply(i)*Hruarea
            runoff = runoff + cfgi_sroff
            Basin_cfgi_sroff = Basin_cfgi_sroff + cfgi_sroff
          ENDIF
          Frozen(i) = frzen
        ENDIF

!******Compute runoff for pervious, impervious, and depression storage area, only if not frozen ground
        IF ( frzen==0 ) THEN
! DO IRRIGATION APPLICATION, ONLY DONE HERE, ASSUMES NO SNOW and
! only for pervious areas (just like infiltration)
        IF ( Use_sroff_transfer==1 ) THEN
          IF ( Net_apply(i)>0.0 ) THEN
            sra = 0.0
            Infil(i) = Infil(i) + Net_apply(i)
            IF ( Hru_type(i)==1 ) THEN
              CALL perv_comp(Net_apply(i), Net_apply(i), Infil(i), sra)
! ** ADD in water from irrigation application and water-use transfer for pervious portion - sra (if any)
              apply_sroff = DBLE( sra*hperv )
              Basin_apply_sroff = Basin_apply_sroff + apply_sroff
              runoff = runoff + apply_sroff
            ENDIF
          ENDIF
        ENDIF

        CALL compute_infil(Net_rain(i), Net_ppt(i), Imperv_stor(i), Imperv_stor_max(i), Snowmelt(i), &
     &                     Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i), Infil(i), Hru_type(i), Intcp_changeover(i))

        ENDIF

!         **********************************************************

        srunoff = 0.0
        IF ( Hru_type(i)==1 ) THEN
!******Compute runoff for pervious and impervious area, and depression storage area
          runoff = runoff + DBLE( Srp*hperv + Sri*Hruarea_imperv )
          srunoff = SNGL( runoff/Hruarea_dble )

!******Compute HRU weighted average (to units of inches/dt)
          IF ( Cascade_flag>0 ) THEN
            hru_sroff_down = 0.0D0
            IF ( srunoff>0.0 ) THEN
              IF ( Ncascade_hru(i)>0 ) CALL run_cascade_sroff(Ncascade_hru(i), srunoff, hru_sroff_down)
              Hru_hortn_cascflow(i) = hru_sroff_down
              !IF ( Hru_hortn_cascflow(i)<0.0D0 ) Hru_hortn_cascflow(i) = 0.0D0
              !IF ( Upslope_hortonian(i)<0.0D0 ) Upslope_hortonian(i) = 0.0D0
              Basin_sroff_upslope = Basin_sroff_upslope + Upslope_hortonian(i)*Hruarea_dble
              Basin_sroff_down = Basin_sroff_down + hru_sroff_down*Hruarea_dble
            ELSE
              Hru_hortn_cascflow(i) = 0.0D0
            ENDIF
          ENDIF
          Hru_sroffp(i) = Srp*Perv_frac
          Basin_sroffp = Basin_sroffp + Srp*hperv
        ENDIF

        Basin_infil = Basin_infil + DBLE( Infil(i)*hperv )
        Basin_contrib_fraction = Basin_contrib_fraction + DBLE( Contrib_fraction(i)*hperv )

!******Compute evaporation from impervious area
        IF ( frzen==0 ) THEN
        IF ( Hruarea_imperv>0.0 ) THEN
          IF ( Imperv_stor(i)>0.0 ) THEN
            CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i), Snowcov_area(i), avail_et)
            Hru_impervevap(i) = Imperv_evap(i)*Imperv_frac
            !IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
            avail_et = avail_et - Hru_impervevap(i)
            IF ( avail_et<0.0 ) THEN
               ! sanity check
!              IF ( avail_et<-NEARZERO ) PRINT*, 'avail_et<0 in srunoff imperv', i, Nowmonth, Nowday, avail_et
              Hru_impervevap(i) = Hru_impervevap(i) + avail_et
              IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
              Imperv_evap(i) = Hru_impervevap(i)/Imperv_frac
              Imperv_stor(i) = Imperv_stor(i) - avail_et/Imperv_frac
              avail_et = 0.0
            ENDIF
            Basin_imperv_evap = Basin_imperv_evap + DBLE( Hru_impervevap(i)*Hruarea )
            Hru_impervstor(i) = Imperv_stor(i)*Imperv_frac
            Basin_imperv_stor = Basin_imperv_stor + DBLE(Imperv_stor(i)*Hruarea_imperv )
          ENDIF
          Hru_sroffi(i) = Sri*Imperv_frac
          Basin_sroffi = Basin_sroffi + DBLE( Sri*Hruarea_imperv )
        ENDIF
        ENDIF

        Sroff(i) = srunoff
        Hortonian_flow(i) = srunoff
        Basin_hortonian = Basin_hortonian + DBLE( srunoff*Hruarea )
        Basin_sroff = Basin_sroff + DBLE( srunoff*Hruarea )
      ENDDO

!******Compute basin weighted averages (to units of inches/dt)
      !rsr, should be land_area???
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_imperv_evap = Basin_imperv_evap*Basin_area_inv
      Basin_imperv_stor = Basin_imperv_stor*Basin_area_inv
      Basin_infil = Basin_infil*Basin_area_inv
      ! doesn't include CFGI runoff
      Basin_sroffp = Basin_sroffp*Basin_area_inv
      Basin_sroffi = Basin_sroffi*Basin_area_inv
      Basin_hortonian = Basin_hortonian*Basin_area_inv
      Basin_contrib_fraction = Basin_contrib_fraction*Basin_area_inv
      IF ( Cascade_flag>0 ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
      ENDIF

      END FUNCTION srunoffrun

!***********************************************************************
!      Subroutine to compute evaporation from impervious area at
!      potential ET rate up to available ET
!***********************************************************************
      SUBROUTINE imperv_et(Imperv_stor, Potet, Imperv_evap, Sca, Avail_et)
      USE PRMS_SRUNOFF, ONLY: Imperv_frac
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Potet, Sca, Avail_et
      REAL, INTENT(INOUT) :: Imperv_stor, Imperv_evap
!***********************************************************************
      IF ( Sca<1.0 ) THEN
        IF ( Potet<Imperv_stor ) THEN
          Imperv_evap = Potet*(1.0-Sca)
        ELSE
          Imperv_evap = Imperv_stor*(1.0-Sca)
        ENDIF
        IF ( Imperv_evap*Imperv_frac>Avail_et ) Imperv_evap = Avail_et/Imperv_frac
        Imperv_stor = Imperv_stor - Imperv_evap
      ENDIF
      !rsr, sanity check
!      IF ( Imperv_stor<0.0 ) THEN
!        PRINT *, 'imperv_stor<0', Imperv_stor
!        Imperv_stor = 0.0
!      ENDIF

      END SUBROUTINE imperv_et

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil(Net_rain, Net_ppt, Imperv_stor, Imperv_stor_max, Snowmelt, &
     &                         Snowinfil_max, Net_snow, Pkwater_equiv, Infil, Hru_type, Intcp_changeover)
      USE PRMS_SRUNOFF, ONLY: Sri, Hruarea_imperv, Upslope_hortonian, Ihru, Srp, Perv_frac
      USE PRMS_SNOW, ONLY: Pptmix_nopack
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      USE PRMS_MODULE, ONLY: Cascade_flag
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Net_rain, Net_ppt, Imperv_stor_max
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow, Intcp_changeover
      DOUBLE PRECISION, INTENT(IN) :: Pkwater_equiv
      REAL, INTENT(INOUT) :: Imperv_stor, Infil
! Functions
      INTRINSIC SNGL
      EXTERNAL perv_comp, check_capacity
! Local Variables
      REAL :: avail_water
!***********************************************************************
! compute runoff from cascading Hortonian flow, all upslope goes to pervious
      IF ( Cascade_flag>0 ) THEN
        avail_water = SNGL( Upslope_hortonian(Ihru)/Perv_frac )
        IF ( avail_water>0.0 ) THEN
          Infil = Infil + avail_water
          IF ( Hru_type==1 ) CALL perv_comp(avail_water, avail_water, Infil, Srp)
        ENDIF
      ELSE
        avail_water = 0.0
      ENDIF

! compute runoff from canopy changeover water
      IF ( Intcp_changeover>0.0 ) THEN
        avail_water = avail_water + Intcp_changeover
        Infil = Infil + Intcp_changeover
        IF ( Hru_type==1 ) CALL perv_comp(Intcp_changeover, Intcp_changeover, Infil, Srp)
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack(Ihru)==1 ) THEN
        avail_water = avail_water + Net_rain
        Infil = Infil + Net_rain
        IF ( Hru_type==1 ) CALL perv_comp(Net_rain, Net_rain, Infil, Srp)
      ENDIF

!******If precipitation on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN
        avail_water = avail_water + Snowmelt
        Infil = Infil + Snowmelt
        IF ( Hru_type==1 ) THEN
          IF ( Pkwater_equiv>0.0D0 .OR. Net_ppt-Net_snow<NEARZERO ) THEN
!******Pervious area computations
            CALL check_capacity(Snowinfil_max, Infil)
!******Snowmelt occurred and depleted the snowpack
          ELSE
            CALL perv_comp(Snowmelt, Net_ppt, Infil, Srp)
          ENDIF
        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( Pkwater_equiv<DNEARZERO ) THEN

!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.

        IF ( Net_snow<NEARZERO .AND. Net_rain>0.0 ) THEN
! no snow, some rain
          avail_water = avail_water + Net_rain
          Infil = Infil + Net_rain
          IF ( Hru_type==1 ) CALL perv_comp(Net_rain, Net_rain, Infil, Srp)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.

      ELSEIF ( Infil>0.0 ) THEN
        IF ( Hru_type==1 ) CALL check_capacity(Snowinfil_max, Infil)
      ENDIF

!******Impervious area computations
      IF ( Hruarea_imperv>0.0 ) THEN
        Imperv_stor = Imperv_stor + avail_water
        IF ( Hru_type==1 ) THEN
          IF ( Imperv_stor>Imperv_stor_max ) THEN
            Sri = Imperv_stor - Imperv_stor_max
            Imperv_stor = Imperv_stor_max
          ENDIF
        ENDIF
      ENDIF

      END SUBROUTINE compute_infil

!***********************************************************************
      SUBROUTINE perv_comp(Pptp, Ptc, Infil, Srp)
      USE PRMS_SRUNOFF, ONLY: Ihru, Smidx_coef, Smidx_exp, &
     &    Carea_max, Carea_min, Carea_dif, Contrib_fraction
      USE PRMS_MODULE, ONLY: Sroff_flag
!      USE PRMS_BASIN, ONLY: CLOSEZERO
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Soil_rechr, Soil_rechr_max
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Pptp, Ptc
      REAL, INTENT(INOUT) :: Infil, Srp
! Local Variables
      REAL :: smidx, srpp, ca_fraction
!***********************************************************************
!******Pervious area computations
      IF ( Sroff_flag==1 ) THEN
        ! antecedent soil_moist
        smidx = Soil_moist(Ihru) + (0.5*Ptc)
        ca_fraction = Smidx_coef(Ihru)*10.0**(Smidx_exp(Ihru)*smidx)
      ELSE
        ! antecedent soil_rechr
        ca_fraction = Carea_min(Ihru) + Carea_dif(Ihru)*(Soil_rechr(Ihru)/Soil_rechr_max(Ihru))
      ENDIF
      IF ( ca_fraction>Carea_max(Ihru) ) ca_fraction = Carea_max(Ihru)
      srpp = ca_fraction*Pptp
      Contrib_fraction(Ihru) = ca_fraction
!      IF ( srpp<0.0 ) THEN
!        PRINT *, 'negative srp', srpp
!        srpp = 0.0
!      ENDIF
      Infil = Infil - srpp
      Srp = Srp + srpp
      !IF ( Srp<CLOSEZERO ) Srp = 0.0

      END SUBROUTINE perv_comp

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_sroff(Ncascade_hru, Runoff, Hru_sroff_down)
!      USE PRMS_BASIN, ONLY: NEARZERO
!      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Ihru, Upslope_hortonian, Strm_seg_in
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt, Cascade_area
      IMPLICIT NONE
! Functions
      INTRINSIC IABS, ABS, DBLE
! Arguments
      INTEGER, INTENT(IN) :: Ncascade_hru
      REAL, INTENT(INOUT) :: Runoff
      DOUBLE PRECISION, INTENT(INOUT) :: Hru_sroff_down
! Local Variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j>0 ) THEN
          Upslope_hortonian(j) = Upslope_hortonian(j) + DBLE( Runoff*Hru_down_fracwt(k, Ihru) )
          Hru_sroff_down = Hru_sroff_down + DBLE( Runoff*Hru_down_frac(k,Ihru) )

! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          Strm_seg_in(j) = Strm_seg_in(j) + DBLE( Runoff*Cascade_area(k, Ihru) )*Cfs_conv
        ENDIF
      ENDDO

! reset Sroff as it accumulates flow to streams
      Runoff = Runoff - SNGL( Hru_sroff_down )
!      IF ( Runoff<0.0 ) THEN
!        IF ( Runoff<-NEARZERO ) THEN
!          IF ( Print_debug>-1 ) PRINT *, 'runoff < NEARZERO', Runoff
!          IF ( Hru_sroff_down>ABS(Runoff) ) THEN
!            Hru_sroff_down = Hru_sroff_down - Runoff
!          ELSE
!            DO k = 1, Ncascade_hru
!              j = Hru_down(k, Ihru)
!              IF ( Strm_seg_in(j)>ABS(Runoff) ) THEN
!                Strm_seg_in(j) = Strm_seg_in(j) - Runoff
!                EXIT
!              ENDIF
!            ENDDO
!          ENDIF
!        ENDIF
!        Runoff = 0.0
!      ENDIF

      END SUBROUTINE run_cascade_sroff

!***********************************************************************
! fill soil to soil_moist_max, if more than capacity restrict
! infiltration by snowinfil_max, with excess added to runoff
!***********************************************************************
      SUBROUTINE check_capacity(Snowinfil_max, Infil)
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_moist
      USE PRMS_SRUNOFF, ONLY: Ihru, Srp
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Snowinfil_max
      REAL, INTENT(INOUT) :: Infil
! Local Variables
      REAL :: capacity, excess
!***********************************************************************
      capacity = Soil_moist_max(Ihru) - Soil_moist(Ihru)
      excess = Infil - capacity
      IF ( excess>Snowinfil_max ) THEN
        Srp = Srp + excess - Snowinfil_max
        Infil = Snowinfil_max + capacity
      ENDIF

      END SUBROUTINE check_capacity

!***********************************************************************
!     srunoff_restart - write or read srunoff restart file
!***********************************************************************
      SUBROUTINE srunoff_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Frozen_flag
      USE PRMS_SRUNOFF
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_sroff_down, Basin_sroff_upslope, Basin_sroffi, Basin_sroffp, &
     &                            Basin_imperv_stor, Basin_imperv_evap, Basin_infil, Basin_hortonian, &
     &                            Sri, Srp, Basin_hortonian_lakes, Basin_contrib_fraction
        WRITE ( Restart_outunit ) Hru_impervstor
        IF ( Frozen_flag==1 ) THEN
          WRITE ( Restart_outunit ) Frozen
          WRITE ( Restart_outunit ) Cfgi
          WRITE ( Restart_outunit ) Cfgi_prev
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_sroff_down, Basin_sroff_upslope, Basin_sroffi, Basin_sroffp, &
     &                          Basin_imperv_stor, Basin_imperv_evap, Basin_infil, Basin_hortonian, &
     &                          Sri, Srp, Basin_hortonian_lakes, Basin_contrib_fraction
        READ ( Restart_inunit ) Hru_impervstor
        IF ( Frozen_flag==1 ) THEN ! could be problem for restart
          READ ( Restart_inunit ) Frozen
          READ ( Restart_inunit ) Cfgi
          READ ( Restart_inunit ) Cfgi_prev
        ENDIF
      ENDIF
      END SUBROUTINE srunoff_restart
