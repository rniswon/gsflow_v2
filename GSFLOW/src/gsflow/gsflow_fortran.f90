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
!***********************************************************************
      CALL gsflow_prms(SETDIMENS)

      CALL gsflow_prms(DECL)

      CALL gsflow_prms(INIT)
      DO i = 1, Number_timesteps
        CALL gsflow_prms(RUN)
      ENDDO
      CALL gsflow_prms(CLEAN)

      END PROGRAM GSFLOW_FORTRAN
