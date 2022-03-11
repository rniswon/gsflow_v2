      PROGRAM GSFLOW_FORTRAN
!***********************************************************************
! PRMS main that controls time loop
!***********************************************************************
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, SETDIMENS
      USE PRMS_MODULE, ONLY: Number_timesteps
      IMPLICIT NONE
! Functions
      EXTERNAL :: gsflow_prms
! Local Variables
      INTEGER :: i, Nsegshold, Nlakeshold
      LOGICAL :: AFR, MS_GSF_converge
      INTEGER, SAVE, ALLOCATABLE :: Idivert(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Diversions(:), agDemand(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: DELTAVOL(:), EXCHANGE(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: LAKEVOL(:), LAKEVAP(:)
!***********************************************************************
      AFR = .TRUE.
      MS_GSF_converge = .FALSE.
      Nsegshold = 1
      Nlakeshold = 1
      ALLOCATE ( Idivert(Nsegshold) )
      ALLOCATE ( Diversions(Nsegshold), agDemand(Nsegshold), EXCHANGE(Nsegshold) )
      ALLOCATE ( DELTAVOL(Nlakeshold), LAKEVOL(Nlakeshold), LAKEVAP(Nlakeshold) )
      Diversions = 0.0D0
      agDemand = 0.0D0
      EXCHANGE = 0.0D0
      LAKEVOL = 0.0D0
      LAKEVAP = 0.0D0
      DELTAVOL = 0.0D0

      CALL gsflow_prms(SETDIMENS, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
                       Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)

      CALL gsflow_prms(DECL, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
                       Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)

      CALL gsflow_prms(INIT, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
                       Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)
      DO i = 1, Number_timesteps
        CALL gsflow_prms(RUN, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
                         Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)
      ENDDO
      CALL gsflow_prms(CLEAN, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
                       Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)

      END PROGRAM GSFLOW_FORTRAN
