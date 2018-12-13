!***********************************************************************
! Computes the potential evapotranspiration using the Hamon
! formulation (Hamon, 1961)
!***********************************************************************
      MODULE PRMS_POTET_HAMON
        IMPLICIT NONE
        ! Local Variables
        DOUBLE PRECISION, PARAMETER :: ONE_12TH = 1.0D0/12.0D0
        CHARACTER(LEN=11), SAVE :: MODNAME
        ! Declared Parameter
        REAL, SAVE, ALLOCATABLE :: Hamon_coef(:, :)
      END MODULE PRMS_POTET_HAMON

      INTEGER FUNCTION potet_hamon()
      USE PRMS_POTET_HAMON
      USE PRMS_MODULE, ONLY: Process, Nhru
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tavgc, Basin_potet, Potet
      USE PRMS_SOLTAB, ONLY: Soltab_sunhrs
      USE PRMS_SET_TIME, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, DBLE, SNGL
      INTEGER, EXTERNAL :: getparam, declparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i, j
      REAL :: dyl, vpsat, vdsat
      CHARACTER(LEN=80), SAVE :: Version_potet
!***********************************************************************
      potet_hamon = 0

      IF ( Process(:3)=='run' ) THEN
!******Compute potential et for each HRU using Hamon formulation
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
! Convert daylength from hours to 12 hour multiple (equal day and night period)
          dyl = SNGL( Soltab_sunhrs(Jday, i)*ONE_12TH )
          vpsat = 6.108*EXP(17.26939*Tavgc(i)/(Tavgc(i)+237.3)) ! in Hamon 1963, eqn. used 273.3??
          vdsat = 216.7*vpsat/(Tavgc(i)+273.3)
          Potet(i) = Hamon_coef(i, Nowmonth)*dyl*dyl*vdsat !??? why day length squared??? Hamon 1963 did not square dyl, it was squared in 1961 original
          ! pet = 0.1651*dyl*vdsat*HC  (default HC would be 1.2 for units mm/day
          ! hamon_coef includes conversion to inches and 0.1651. hamon_coef = x*0.1651/25.4 = x*0.0065 so potet = inches/day
          ! Potet(i) = hamoncoef*0.1651/25.4*dyl*vdsat  1963 version
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + DBLE( Potet(i)*Hru_area(i) )
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet = 'potet_hamon.f90 2015-12-04 23:29:08Z'
        CALL print_module(Version_potet, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_hamon'

        ALLOCATE ( Hamon_coef(Nhru,12) )
        IF ( declparam(MODNAME, 'hamon_coef', 'nhru,nmonths', 'real', &
     &       '0.0055', '0.004', '0.008', &
     &       'Monthly air temperature coefficient - Hamon', &
     &       'Monthly (January to December) air temperature coefficient used in Hamon potential ET computations for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'hamon_coef')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'hamon_coef', Nhru*12, 'real', Hamon_coef)/=0 ) CALL read_error(2, 'hamon_coef')
      ENDIF

      END FUNCTION potet_hamon
