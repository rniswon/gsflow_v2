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
      LOGICAL :: AFR
!***********************************************************************
      AFR = .TRUE.
      CALL gsflow_prms(SETDIMENS, AFR)

      CALL gsflow_prms(DECL, AFR)

      CALL gsflow_prms(INIT, AFR)
      DO i = 1, Number_timesteps
        CALL gsflow_prms(RUN, AFR)
      ENDDO
      CALL gsflow_prms(CLEAN, AFR)

      END PROGRAM GSFLOW_FORTRAN
