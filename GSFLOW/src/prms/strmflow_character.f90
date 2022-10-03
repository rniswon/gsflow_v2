!***********************************************************************
! streamflow characteristics module
!***********************************************************************
      MODULE PRMS_STRMFLOW_CHARACTER
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Streamflow Characteristics'
      character(len=18), parameter :: MODNAME = 'strmflow_character'
      character(len=*), parameter :: Version_strmflow_character = '2022-09-28'

!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Seg_width(:), Seg_depth(:), Seg_area(:)
      REAL, SAVE, ALLOCATABLE :: Seg_velocity(:)
!   Segment Parameters
      REAL, SAVE, ALLOCATABLE :: width_alpha(:), width_m(:)
      REAL, SAVE, ALLOCATABLE :: depth_alpha(:), depth_m(:)
!   Conversions
      REAL, PARAMETER :: CFS_TO_CMS = 0.028316847
      END MODULE PRMS_STRMFLOW_CHARACTER

!***********************************************************************
!     Main stream temperature routine
!***********************************************************************
      INTEGER FUNCTION strmflow_character()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: strmflow_character_run, strmflow_character_decl
      INTEGER, EXTERNAL :: strmflow_character_init
!***********************************************************************
      strmflow_character = 0

      IF ( Process_flag==RUN ) THEN
         strmflow_character = strmflow_character_run()
      ELSEIF ( Process_flag==DECL ) THEN
         strmflow_character = strmflow_character_decl()
      ELSEIF ( Process_flag==INIT ) THEN
         strmflow_character = strmflow_character_init()
      ENDIF

      END FUNCTION strmflow_character

!***********************************************************************
!     strmflow_character_decl - set up parameters and storage
!   Declared Parameters
!***********************************************************************
      INTEGER FUNCTION strmflow_character_decl()
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_STRMFLOW_CHARACTER
      use PRMS_MMFAPI, only: declvar_real
      use PRMS_READ_PARAM_FILE, only: declparam
      use prms_utils, only: read_error, print_module
      IMPLICIT NONE
! Functions
      INTRINSIC :: INDEX
 !***********************************************************************
      strmflow_character_decl = 0

      CALL print_module(MODDESC, MODNAME, Version_strmflow_character)

! Declared Variables
      ALLOCATE ( Seg_width(Nsegment) )
      CALL declvar_real( MODNAME, 'seg_width', 'nsegment', Nsegment, &
     &     'Width of flow in each segment', &
     &     'meters', Seg_width )

      ALLOCATE ( Seg_depth(Nsegment) )
      CALL declvar_real( MODNAME, 'seg_depth', 'nsegment', Nsegment, &
     &     'Depth of flow in each segment', &
     &     'meters', Seg_depth )

      ALLOCATE ( Seg_area(Nsegment) )
      CALL declvar_real( MODNAME, 'seg_area', 'nsegment', Nsegment, &
     &     'Cross sectional area of flow in each segment', &
     &     'square meters', Seg_area )

      ALLOCATE ( Seg_velocity(Nsegment) )
      CALL declvar_real( MODNAME, 'seg_velocity', 'nsegment', Nsegment, &
     &     'Mean velocity of flow in each segment', &
     &     'meters per second', Seg_velocity )

      ALLOCATE ( width_alpha(Nsegment) )
      IF ( declparam( MODNAME, 'width_alpha', 'nsegment', 'real', &
     &     '7.2', '2.6', '20.2', &
     &     'Alpha coefficient in power function for width calculation', &
     &     'Alpha coefficient in power function for width calculation (for units M and CMS)', &
     &     'meters')/=0 ) CALL read_error(1, 'width_alpha')

      ALLOCATE ( width_m(Nsegment) )
      IF ( declparam( MODNAME, 'width_m', 'nsegment', 'real', &
     &     '0.5', '0.48', '0.52', &
     &     'M value in power function for width calculation', &
     &     'M value in power function for width calculation (for units M and CMS)', &
     &     'none')/=0 ) CALL read_error(1, 'width_m')

      ALLOCATE ( depth_alpha(Nsegment) )
      IF ( declparam( MODNAME, 'depth_alpha', 'nsegment', 'real', &
    &      '0.27', '0.12', '0.63', &
     &     'Alpha coefficient in power function for depth calculation', &
     &     'Alpha coefficient in power function for depth calculation (for units M and CMS)', &
     &     'meters')/=0 ) CALL read_error(1, 'depth_alpha')

      ALLOCATE ( depth_m(Nsegment) )
      IF ( declparam( MODNAME, 'depth_m', 'nsegment', 'real', &
     &     '0.39', '0.38', '0.40', &
     &     'M value in power function for depth calculation', &
     &     'M value in power function for depth calculation (for units M and CMS)', &
     &     'meters')/=0 ) CALL read_error(1, 'depth_m')

      END FUNCTION strmflow_character_decl

!***********************************************************************
!    strmflow_character_init - Initialize module - get parameter values
!***********************************************************************
      INTEGER FUNCTION strmflow_character_init()
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_STRMFLOW_CHARACTER
      use PRMS_READ_PARAM_FILE, only: getparam_real
      use prms_utils, only: read_error
      IMPLICIT NONE
!***********************************************************************
      strmflow_character_init = 0

      IF ( getparam_real( MODNAME, 'width_alpha', Nsegment, width_alpha)/=0 ) CALL read_error(2, 'width_alpha')
      IF ( getparam_real( MODNAME, 'width_m', Nsegment, width_m)/=0 ) CALL read_error(2, 'width_m')
      IF ( getparam_real( MODNAME, 'depth_alpha', Nsegment, depth_alpha)/=0 ) CALL read_error(2, 'depth_alpha')
      IF ( getparam_real( MODNAME, 'depth_m', Nsegment, depth_m)/=0 ) CALL read_error(2, 'depth_m')

! Initialize declared variables
      Seg_width = 0.0
      Seg_depth = 0.0
      Seg_area = 0.0
      Seg_velocity = 0.0

      END FUNCTION strmflow_character_init

!***********************************************************************
!     strmflow_character_run - Computes streamflow characteristics
!***********************************************************************
      INTEGER FUNCTION strmflow_character_run()
      USE PRMS_CONSTANTS, ONLY: NEARZERO
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_STRMFLOW_CHARACTER
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
!***********************************************************************
      strmflow_character_run = 0

      DO i = 1, Nsegment
         if (seg_outflow(i) > NEARZERO) then
            Seg_width(i) = width_alpha(i) * (SNGL(Seg_outflow(i)) * CFS_TO_CMS) ** width_m(i)
            Seg_depth(i) = depth_alpha(i) * (SNGL(Seg_outflow(i)) * CFS_TO_CMS) ** depth_m(i)
            Seg_area(i) = Seg_width(i) * Seg_depth(i)
            if (seg_area(i) > NEARZERO) then
               Seg_velocity(i) = SNGL(Seg_outflow(i)) * CFS_TO_CMS / Seg_area(i)
            else
               Seg_velocity(i) = 0.0
            endif
         else
            Seg_width(i) = 0.0
            Seg_depth(i) = 0.0
            Seg_area(i) = 0.0
            Seg_velocity(i) = 0.0
         endif
      ENDDO

      END FUNCTION strmflow_character_run
