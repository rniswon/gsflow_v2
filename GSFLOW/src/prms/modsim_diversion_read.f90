!***********************************************************************
! Read and makes available MODSIM diversion mapping from segments to associated HRUs
!***********************************************************************
      MODULE PRMS_MODSIM_DIVERSION_READ
      USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
      ! Local Variables
      character(len=*), parameter :: MODDESC = 'Time Series Data'
      character(len=*), parameter :: MODNAME = 'modsim_diversion_read'
      character(len=*), parameter :: Version_modsim_diversion_read = '2022-04-13'

      ! Local Variables
      INTEGER, SAVE :: diversion_unit, outunit, Nag_diversions, event_year, event_month, event_day
      INTEGER, SAVE :: diversion_next_year, diversion_next_month, diversion_next_day
      INTEGER, ALLOCATABLE, SAVE :: seg_source_id(:), hru_destination_id(:), destination_frac(:)
      LOGICAL, SAVE :: not_eof
! Control Parameters
      CHARACTER(LEN=MAXFILE_LENGTH) :: modsim_diversion_file
      END MODULE PRMS_MODSIM_DIVERSION_READ

      SUBROUTINE modsim_diversion_read()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ERROR_water_use
      use PRMS_CONTROL_FILE, only: control_string
      USE PRMS_MODULE, ONLY: Process_flag, Start_year, Start_month, Start_day, End_year, End_month, End_day
      USE PRMS_MODSIM_DIVERSION_READ
      use prms_utils, only: error_stop, find_header_end, print_module, PRMS_open_module_file, read_error
      IMPLICIT NONE
! Functions
      EXTERNAL :: read_div_event
! Local variables
      INTEGER :: i, ierr, year, month, day, ios
      character(LEN=4) :: dum
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        if ( not_eof ) THEN
          i = 0
          do while (i == 0)
            read (diversion_unit, '(A)', iostat=ios) dum
            if (ios /= 0) THEN
              not_eof = .FALSE.
              i = 1
            else if (dum == '####') then
              DEALLOCATE ( seg_source_id, hru_destination_id, destination_frac )
              CALL read_div_event()
              i = 1
            end if
          end do
        end if

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_modsim_diversion_read)

      ELSEIF ( Process_flag==INIT ) THEN
        Nag_diversions = 0
        year = Start_year
        month = Start_month
        day = Start_day
        ierr = 0

        CALL PRMS_open_module_file(Outunit, 'MODSIM_diversions.out')
        WRITE ( Outunit, 10 ) 'Simulation Start Date:', year, month, day, '   End Date:', End_year, End_month, End_day
10      FORMAT ( 'MODSIM diversion Summary File', /, 2(A, I5, 2('/',I2.2)), / )

        IF ( control_string(modsim_diversion_file, 'segment_transfer_file')/=0 ) CALL read_error(5, 'modsim_diversion_file')
        CALL find_header_end(diversion_unit, modsim_diversion_file, ierr)
        IF ( ierr == 0 ) CALL error_stop('in modsim_diversion_read module', ERROR_water_use)

        not_eof = .TRUE.
        CALL read_div_event()
      ENDIF

      END SUBROUTINE modsim_diversion_read

!*****************************
! Read MODSIM diversion event
!*****************************
      SUBROUTINE read_div_event()
      USE PRMS_CONSTANTS, ONLY: ERROR_water_use, ACTIVE, OFF
      USE PRMS_MODSIM_DIVERSION_READ
      USE PRMS_MODULE, ONLY: Nowyear, Nowmonth, Nowday, Nsegment
      use prms_utils, only: error_stop
      IMPLICIT NONE
! Functions
      INTRINSIC :: ABS
! Local variables
      INTEGER :: i, istop, ios, keep_reading, next_year, next_month, next_day, ndiv
      REAL :: check_frac(Nsegment)
!*******************************************************************************
      istop = 0
 
      check_frac = 0.0
      keep_reading = ACTIVE
      DO WHILE ( keep_reading==ACTIVE )
        READ ( diversion_unit, *, IOSTAT=ios ) next_year, next_month, next_day, ndiv
        IF ( ios == 0 ) THEN
          IF ( next_year==Nowyear .AND. next_month==Nowmonth .AND. next_day==Nowday ) THEN
            diversion_next_year = next_year
            diversion_next_month = next_month
            diversion_next_day = next_day
            Nag_diversions = ndiv
            ALLOCATE ( seg_source_id(Nag_diversions), hru_destination_id(Nag_diversions), destination_frac(Nag_diversions) )
            DO i = 1, Nag_diversions
              READ ( diversion_unit, *, IOSTAT=ios ) seg_source_id(i), hru_destination_id(i), destination_frac(i)
              IF ( ios /= 0 ) THEN
                istop = 1
                EXIT
              ENDIF
              check_frac(seg_source_id(i)) = check_frac(seg_source_id(i)) + destination_frac(i)
            ENDDO
          ELSE
            backspace diversion_unit
            keep_reading = OFF
          ENDIF
        ELSE
          istop = 1
        ENDIF
      ENDDO

      DO i = 1, Nsegment
         IF ( check_frac(i) > 0.0 ) THEN
            IF ( ABS(check_frac(i) - 1.0) > 1.0E-8 ) THEN
                PRINT *, 'ERROR, Diversion fraction not equal 1 for segment:', i
                istop = 1
            ENDIF
         ENDIF
      ENDDO

      IF ( istop==1 ) CALL error_stop('in modsim_diversion_read module', ERROR_water_use)

      END SUBROUTINE read_div_event


