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
      INTEGER :: i
      LOGICAL :: AFR, MS_GSF_converge
      INTEGER :: Nsegshold, Nlakeshold
      INTEGER, ALLOCATABLE :: Idivert(:)
      DOUBLE PRECISION, ALLOCATABLE :: Diversions(:), LAKEVOL(:), LAKEVAP(:)
      DOUBLE PRECISION, ALLOCATABLE :: agDemand(:), DELTAVOL(:), EXCHANGE(:)        
!***********************************************************************
      AFR = .TRUE.
      MS_GSF_converge = .FALSE.
      Nlakeshold = 1
      Nsegshold = 1
      ALLOCATE ( Idivert(Nlakeshold), DELTAVOL(Nlakeshold),LAKEVOL(Nlakeshold), LAKEVAP(Nlakeshold)  )
      ALLOCATE ( Diversions(Nsegshold), agDemand(Nsegshold), EXCHANGE(Nsegshold) )
      CALL gsflow_prms(SETDIMENS, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
     &                 Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)

      CALL gsflow_prms(DECL, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
     &                 Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)

      CALL gsflow_prms(INIT, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
     &                 Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)
      DO i = 1, Number_timesteps
        CALL gsflow_prms(RUN, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
     &                   Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)
      ENDDO
      CALL gsflow_prms(CLEAN, AFR, MS_GSF_converge, Nsegshold, Nlakeshold, &
     &                 Diversions, Idivert, EXCHANGE, DELTAVOL, LAKEVOL, LAKEVAP, agDemand)

      END PROGRAM GSFLOW_FORTRAN
