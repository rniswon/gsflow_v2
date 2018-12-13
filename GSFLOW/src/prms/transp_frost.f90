!***********************************************************************
! Determine whether transpiration is occurring. Transpiration is based
! on time between the last spring and the first fall killing frost.
!***********************************************************************
      MODULE PRMS_TRANSP_FROST
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=12), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Fall_frost(:), Spring_frost(:)
      END MODULE PRMS_TRANSP_FROST

      INTEGER FUNCTION transp_frost()
      USE PRMS_TRANSP_FROST
      USE PRMS_MODULE, ONLY: Process, Nhru
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Transp_on, Basin_transp_on
      USE PRMS_SET_TIME, ONLY: Jsol
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i, j
      CHARACTER(LEN=80), SAVE :: Version_transp_frost
!***********************************************************************
      transp_frost = 0

      IF ( Process(:3)=='run' ) THEN
!******Set switch for active transpiration period
! If the current solar day is between the last frost of the
! spring and the first frost of the fall, then transpiration
! is on for the HRU. If any HRU is transpiring, then
! Basin_transp_on is set to 1.
        Basin_transp_on = 0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Jsol>=Spring_frost(i) .AND. Jsol<=Fall_frost(i) ) THEN
            Transp_on(i) = 1
            Basin_transp_on = 1
          ELSE
            Transp_on(i) = 0
          ENDIF
        ENDDO

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_transp_frost = 'transp_frost.f90 2014-12-02 19:06:41Z'
        CALL print_module(Version_transp_frost, 'Transpiration Distribution  ', 90)
        MODNAME = 'transp_frost'

        ALLOCATE ( Spring_frost(Nhru) )
        IF ( declparam(MODNAME, 'spring_frost', 'nhru', 'integer', &
     &       '111', '1', '366', &
     &       'The solar date (number of days after winter solstice) of the last killing frost of the spring', &
     &       'The solar date (number of days after winter solstice) of the last killing frost of the spring', &
     &       'Solar date')/=0 ) CALL read_error(1, 'spring_frost')

        ALLOCATE ( Fall_frost(Nhru) )
        IF ( declparam(MODNAME, 'fall_frost', 'nhru', 'integer', &
     &       '264', '1', '366', &
     &       'The solar date (number of days after winter solstice) of the first killing frost of the fall', &
     &       'The solar date (number of days after winter solstice) of the first killing frost of the fall', &
     &       'Solar date')/=0 ) CALL read_error(1, 'fall_frost')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'spring_frost', Nhru, 'integer', Spring_frost)/=0 ) CALL read_error(2, 'spring_frost')
        IF ( getparam(MODNAME, 'fall_frost', Nhru, 'integer', Fall_frost)/=0 ) CALL read_error(2, 'fall_frost')

        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Jsol>=Spring_frost(i) .AND. Jsol<=Fall_frost(i) ) THEN
            Transp_on(i) = 1
            Basin_transp_on = 1
          ENDIF
        ENDDO
      ENDIF

      END FUNCTION transp_frost
 