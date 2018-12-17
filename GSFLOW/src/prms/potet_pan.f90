!***********************************************************************
! Computes the potential evapotranspiration for each HRU using
! pan-evaporation data
!   Declared Parameters: hru_pansta, epan_coef
!***********************************************************************
      MODULE PRMS_POTET_PAN
        IMPLICIT NONE
        ! Local Variables
        REAL, SAVE, ALLOCATABLE :: Last_pan_evap(:)
        CHARACTER(LEN=9), SAVE :: MODNAME
      END MODULE PRMS_POTET_PAN

      INTEGER FUNCTION potet_pan()
      USE PRMS_POTET_PAN
      USE PRMS_MODULE, ONLY: Process, Print_debug, Save_vars_to_file, Init_vars_from_file, Nevap
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Hru_pansta, Epan_coef
      USE PRMS_SET_TIME, ONLY: Nowmonth
      USE PRMS_OBS, ONLY: Pan_evap
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, potet_pan_restart, print_date
! Local Variables
      INTEGER :: i, k, j
      CHARACTER(LEN=80), SAVE :: Version_potet_pan
!***********************************************************************
      potet_pan = 0

      IF ( Process(:3)=='run' ) THEN
        DO i = 1, Nevap
          IF ( Pan_evap(i)<0.0 ) THEN
            IF ( Print_debug>-1 ) THEN
              PRINT *, 'Pan_evap<0, set to last value, station:', i, '; value:', Pan_evap(i)
              CALL print_date(1)
            ENDIF
            Pan_evap(i) = Last_pan_evap(i)
          ENDIF
        ENDDO

        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          k = Hru_pansta(i)
          Potet(i) = Pan_evap(k)*Epan_coef(i, Nowmonth)
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + DBLE( Potet(i)*Hru_area(i) )
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
        Last_pan_evap = Pan_evap

!******Declare parameters
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_pan = 'potet_pan.f90 2015-12-04 23:29:08Z'
        CALL print_module(Version_potet_pan, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_pan'

        IF ( Nevap==0 ) STOP 'ERROR, potet_pan module selected, but nevap=0'
        ALLOCATE ( Last_pan_evap(Nevap) )

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file>0 ) THEN
          CALL potet_pan_restart(1)
        ELSE
          Last_pan_evap = 0.0
        ENDIF

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL potet_pan_restart(0)

      ENDIF

      END FUNCTION potet_pan

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_pan_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_POTET_PAN, ONLY: MODNAME, Last_pan_evap
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=9) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Last_pan_evap
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Last_pan_evap
      ENDIF
      END SUBROUTINE potet_pan_restart
