!***********************************************************************
!     Distribute MODSIM segment diversions to PRMS HRUs
!***********************************************************************
      MODULE GSFMODSIM2PRMS
      IMPLICIT NONE
!   Module Variables
      character(len=*), parameter :: MODDESC = 'MODSIM to PRMS'
      character(len=*), parameter :: MODNAME = 'gsflow_modsim2prms'
      character(len=*), parameter :: Version_gsflow_modsim2prms = '2022-04-14'
      DOUBLE PRECISION, SAVE :: MSl3_to_acre_inches
!   Declared Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_diversions(:)
      REAL, SAVE, ALLOCATABLE :: HRU_diversion(:)
      END MODULE GSFMODSIM2PRMS

!     ******************************************************************
!     Mapping module to convert MODSIM diversions for use by PRMS
!     ******************************************************************
      SUBROUTINE gsflow_modsim2prms(DIVERSIONS)
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, FT2_PER_ACRE
      USE PRMS_MODULE, ONLY: Process_flag, Nsegment, Nhru
      USE GSFMODSIM2PRMS
      USE PRMS_MODSIM_DIVERSION_READ, ONLY: Nag_diversions, seg_source_id, hru_destination_id, destination_frac
      use prms_utils, only: print_module
      USE PRMS_MMFAPI, only: declvar_dble, declvar_real
      IMPLICIT NONE
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: DIVERSIONS(Nsegment)
! Local Variables
      INTEGER :: i
      DOUBLE PRECISION :: MSl3_to_ft3
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
!-----------------------------------------------------------------------
! Diversion to HRUs in inches
!-----------------------------------------------------------------------
         HRU_diversion = 0.0
         DO i = 1, Nag_diversions
            HRU_diversion(hru_destination_id(i)) = HRU_diversion(hru_destination_id(i)) + &
                                                   DIVERSIONS(seg_source_id(i)) * destination_frac(i)
         ENDDO
         Segment_diversions = DIVERSIONS * MSl3_to_acre_inches
      ELSEIF ( Process_flag==DECL ) THEN
         ALLOCATE ( Segment_diversions(Nsegment) )
         CALL declvar_dble(MODNAME, 'Segment_diversions', 'nsegment', Nsegment, &
     &        'Diversion fow from each segment as computed by MODSIM', &
     &        'acre-inches', Segment_diversions)
         ALLOCATE ( HRU_diversion(Nhru) )
         CALL declvar_real(MODNAME, 'HRU_diversion', 'nhru', Nhru, &
     &        'Diversion fow from each segment to each HRU as computed by MODSIM', &
     &        'inches', HRU_diversion)
         CALL print_module(MODDESC, MODNAME, Version_gsflow_modsim2prms)
      ELSEIF ( Process_flag==INIT ) THEN
         ! MODSIM in meters
         MSl3_to_ft3 = 3.280839895D0**3.0D0
         MSl3_to_acre_inches = (MSl3_to_ft3*12.0D0) / FT2_PER_ACRE
      ENDIF

      END SUBROUTINE gsflow_modsim2prms
