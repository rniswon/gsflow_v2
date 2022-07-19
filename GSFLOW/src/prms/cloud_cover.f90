!***********************************************************************
! Computes cloud cover fraction
! Declared Parameters: ccov_slope, ccov_intcp
!***********************************************************************
      MODULE PRMS_CLOUD_COVER
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Cloud cover'
        character(len=*), parameter :: MODNAME = 'cloud_cover'
        character(len=*), parameter :: Version_cloud_cover = '2022-06-07'
        ! Declared Variable
        DOUBLE PRECISION, SAVE :: Basin_cloud_cover
        REAL, SAVE, ALLOCATABLE :: Cloud_cover_hru(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Ccov_slope(:, :), Ccov_intcp(:, :)
      END MODULE PRMS_CLOUD_COVER
!***********************************************************************
      SUBROUTINE cloud_cover()
      use PRMS_MMFAPI, only: declvar_real, declvar_dble
      use PRMS_READ_PARAM_FILE, only: declparam, getparam_real
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, MONTHS_PER_YEAR, OFF
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Nowmonth, Cloud_cover_cbh_flag
      USE PRMS_CLOUD_COVER
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tmax_hru, Tmin_hru
      USE PRMS_CLIMATE_HRU, ONLY: Cloud_cover_cbh
      use prms_utils, only: print_module, read_error
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE
! Local Variables
      INTEGER :: j, jj
      REAL :: ccov
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        Basin_cloud_cover = 0.0D0
        IF ( Cloud_cover_cbh_flag == OFF ) THEN
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            ccov = Ccov_slope(j, Nowmonth)*(Tmax_hru(j)-Tmin_hru(j)) + Ccov_intcp(j, Nowmonth)
            IF ( ccov<0.0 ) THEN
              ccov = 0.0
            ELSEIF ( ccov>1.0 ) THEN
              ccov = 1.0
            ENDIF
            Cloud_cover_hru(j) = ccov
            Basin_cloud_cover = Basin_cloud_cover + DBLE( Cloud_cover_hru(j)*Hru_area(j) )
          ENDDO
        ELSE
          Cloud_cover_hru = Cloud_cover_cbh
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            Basin_cloud_cover = Basin_cloud_cover + DBLE( Cloud_cover_hru(j)*Hru_area(j) )
          ENDDO
        ENDIF
        Basin_cloud_cover = Basin_cloud_cover*Basin_area_inv

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_cloud_cover)

        ALLOCATE ( Cloud_cover_hru(Nhru) )
        CALL declvar_real(MODNAME, 'cloud_cover_hru', 'nhru', Nhru, &
     &       'Cloud cover proportion of each HRU', &
     &       'decimal fraction', Cloud_cover_hru)

        CALL declvar_dble(MODNAME, 'basin_cloud_cover', 'one', 1, &
     &       'Basin area-weighted average cloud cover proportion', &
     &       'decimal fraction', Basin_cloud_cover)

        ALLOCATE ( Ccov_slope(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(MODNAME, 'ccov_slope', 'nhru,nmonths', 'real', &
     &       '-0.13', '-0.5', '-0.01', &
     &       'Slope in temperature cloud cover relationship', &
     &       'Monthly (January to December) coefficient in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_slope')

        ALLOCATE ( Ccov_intcp(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(MODNAME, 'ccov_intcp', 'nhru,nmonths', 'real', &
     &       '1.83', '0.0', '5.0', &
     &       'Intercept in temperature cloud cover relationship', &
     &       'Monthly (January to December) intercept in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_intcp')

      ELSEIF ( Process_flag==INIT ) THEN
        Cloud_cover_hru = 0.0

        IF ( getparam_real(MODNAME, 'ccov_slope', Nhru*MONTHS_PER_YEAR, Ccov_slope)/=0 ) CALL read_error(2, 'ccov_slope')
        IF ( getparam_real(MODNAME, 'ccov_intcp', Nhru*MONTHS_PER_YEAR, Ccov_intcp)/=0 ) CALL read_error(2, 'ccov_intcp')

      ENDIF

      END SUBROUTINE cloud_cover
