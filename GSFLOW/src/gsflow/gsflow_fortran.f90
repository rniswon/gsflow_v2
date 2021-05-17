      PROGRAM GSFLOW_FORTRAN
!***********************************************************************
! PRMS main that controls time loop
!***********************************************************************
      USE PRMS_MODULE, ONLY: Model, Number_timesteps
      IMPLICIT NONE
! Functions
      EXTERNAL gsflow_prms
! Local Variables
      INTEGER :: i
!***********************************************************************
      CALL gsflow_prms('setdims')

      CALL gsflow_prms('decl')

      CALL gsflow_prms('init')

      DO i = 1, Number_timesteps
        CALL gsflow_prms('run',)
      ENDDO
      CALL gsflow_prms('clean')

      END PROGRAM GSFLOW_FORTRAN
