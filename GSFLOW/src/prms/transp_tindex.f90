!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
      MODULE PRMS_TRANSP_TINDEX
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE, ALLOCATABLE :: Transp_check(:), Transp_beg_restart(:), Transp_end_restart(:)
        REAL, SAVE, ALLOCATABLE :: Tmax_sum(:), Transp_tmax_f(:), Transp_tmax_restart(:)
        CHARACTER(LEN=13), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Transp_beg(:), Transp_end(:)
        REAL, SAVE, ALLOCATABLE :: Transp_tmax(:)
      END MODULE PRMS_TRANSP_TINDEX

      INTEGER FUNCTION transp_tindex()
      USE PRMS_TRANSP_TINDEX
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file, Init_vars_from_file, Start_month, Start_day
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Temp_units, Transp_on, Basin_transp_on 
      USE PRMS_SET_TIME, ONLY: Nowmonth, Nowday
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      REAL, EXTERNAL :: c_to_f
      EXTERNAL :: read_error, print_module, transp_tindex_restart
! Local Variables
      INTEGER :: i, j, motmp, new_values
      CHARACTER(LEN=80), SAVE :: Version_transp
!***********************************************************************
      transp_tindex = 0

      IF ( Process(:3)=='run' ) THEN
!******Set switch for active transpiration period
        Basin_transp_on = 0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

!******check for month to turn check switch on or
!******transpiration switch off
          IF ( Nowday==1 ) THEN
            !******check for end of period
            IF ( Nowmonth==Transp_end(i) ) THEN
              Transp_on(i) = 0
              Transp_check(i) = 0
              Tmax_sum(i) = 0.0
            ENDIF
!******check for month to turn transpiration switch on or off
            IF ( Nowmonth==Transp_beg(i) ) THEN
              Transp_check(i) = 1
              Tmax_sum(i) = 0.0
            ENDIF
          ENDIF

!******If in checking period, then for each day
!******sum maximum temperature until greater than temperature index parameter,
!******at which time, turn transpiration switch on, check switch off
          ! freezing temperature assumed to be 32 degrees Fahrenheit
          IF ( Transp_check(i)==1 ) THEN
            IF ( Tmaxf(i)>32.0 ) Tmax_sum(i) = Tmax_sum(i) + Tmaxf(i)
            IF ( Tmax_sum(i)>Transp_tmax_f(i) ) THEN
              Transp_on(i) = 1
              Transp_check(i) = 0
              Tmax_sum(i) = 0.0
            ENDIF
          ENDIF

          IF ( Basin_transp_on==0 ) THEN
            IF ( Transp_on(i)==1 ) Basin_transp_on = 1
          ENDIF
        ENDDO

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_transp = 'transp_tindex.f90 2015-01-06 00:09:15Z'
        CALL print_module(Version_transp, 'Transpiration Distribution  ', 90)
        MODNAME = 'transp_tindex'

        ALLOCATE ( Tmax_sum(Nhru), Transp_check(Nhru), Transp_tmax_f(Nhru) )

        ALLOCATE ( Transp_beg(Nhru) )
        IF ( declparam(MODNAME, 'transp_beg', 'nhru', 'integer', &
     &       '1', '1', '12', &
     &       'Month to begin testing for transpiration', &
     &       'Month to begin summing the maximum air temperature for each HRU; when sum is greater than or'// &
     &       ' equal to transp_tmax, transpiration begins', &
     &       'month')/=0 ) CALL read_error(1, 'transp_beg')

        ALLOCATE ( Transp_end(Nhru) )
        IF ( declparam(MODNAME, 'transp_end', 'nhru', 'integer', &
     &       '13', '1', '13', &
     &       'Month to stop transpiration period', &
     &       'Month to stop transpiration computations; transpiration is computed thru end of previous month', &
     &       'month')/=0 ) CALL read_error(1, 'transp_end')

        ALLOCATE ( Transp_tmax(Nhru) )
        IF ( declparam(MODNAME, 'transp_tmax', 'nhru', 'real', &
     &       '1.0', '0.0', '1000.0', &
     &       'Tmax index to determine start of transpiration', &
     &       'Temperature index to determine the specific date of the start of the transpiration period;'// &
     &       ' the maximum air temperature for each HRU is summed starting with the first day of month transp_beg;'// &
     &       ' when the sum exceeds this index, transpiration begins', &
     &       'temp_units')/=0 ) CALL read_error(1, 'transp_tmax')

      ELSEIF ( Process(:4)=='init' ) THEN

        IF ( getparam(MODNAME, 'transp_beg', Nhru, 'integer', Transp_beg)/=0 ) CALL read_error(2, 'transp_beg')
        IF ( getparam(MODNAME, 'transp_end', Nhru, 'integer', Transp_end)/=0 ) CALL read_error(2, 'transp_end')
        IF ( getparam(MODNAME, 'transp_tmax', Nhru, 'real', Transp_tmax)/=0 ) CALL read_error(2, 'transp_tmax')

        new_values = 0
        IF ( Init_vars_from_file>0 ) THEN
          ALLOCATE ( Transp_beg_restart(Nhru), Transp_end_restart(Nhru), Transp_tmax_restart(Nhru) )
          CALL transp_tindex_restart(1)
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            IF ( new_values==1 ) EXIT
            IF ( Transp_beg(i)/=Transp_beg_restart(i) ) new_values = 1
            IF ( Transp_end(i)/=Transp_end_restart(i) ) new_values = 1
            IF ( Transp_tmax(i)/=Transp_tmax_restart(i) ) new_values = 1
          ENDDO
          DEALLOCATE ( Transp_beg_restart, Transp_end_restart, Transp_tmax_restart )
        ENDIF

        IF ( Temp_units==0 ) THEN
          Transp_tmax_f = Transp_tmax
        ELSE
          DO i = 1, Nhru
            Transp_tmax_f(i) = c_to_f(Transp_tmax(i))
          ENDDO
        ENDIF
        !DEALLOCATE ( Transp_tmax )

        IF ( Init_vars_from_file==0 .OR. new_values==1 ) THEN
          motmp = Start_month + 12
          Tmax_sum = 0.0
          Transp_check = 0
          Basin_transp_on = 0
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            IF ( Start_month==Transp_beg(i) ) THEN
              IF ( Start_day>10 ) THEN ! rsr, why 10? if transp_tmax < 300, should be < 10
                Transp_on(i) = 1
              ELSE
                Transp_check(i) = 1
              ENDIF
            ELSEIF ( Transp_end(i)>Transp_beg(i) ) THEN
              IF ( Start_month>Transp_beg(i) .AND. Start_month<Transp_end(i) ) Transp_on(i) = 1
            ELSE
              IF ( Start_month>Transp_beg(i) .OR. motmp<Transp_end(i)+12 ) Transp_on(i) = 1
            ENDIF
            IF ( Basin_transp_on==0 ) THEN
              IF ( Transp_on(i)==1 ) Basin_transp_on = 1
            ENDIF
          ENDDO
        ENDIF

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL transp_tindex_restart(0)

      ENDIF

      END FUNCTION transp_tindex

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE transp_tindex_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_TRANSP_TINDEX
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Transp_check
        WRITE ( Restart_outunit ) Tmax_sum
        WRITE ( Restart_outunit ) Transp_beg
        WRITE ( Restart_outunit ) Transp_end
        WRITE ( Restart_outunit ) Transp_tmax
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Transp_check
        READ ( Restart_inunit ) Tmax_sum
        READ ( Restart_inunit ) Transp_beg_restart
        READ ( Restart_inunit ) Transp_end_restart
        READ ( Restart_inunit ) Transp_tmax_restart
      ENDIF
      END SUBROUTINE transp_tindex_restart
