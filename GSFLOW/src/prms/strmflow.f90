!***********************************************************************
! Computes daily streamflow as the sum of surface runoff,
! shallow-subsurface flow (interflow), and ground-water flow
!***********************************************************************
      INTEGER FUNCTION strmflow()
      USE PRMS_MODULE, ONLY: Process
      USE PRMS_BASIN, ONLY: Active_area, CFS2CMS_CONV
      USE PRMS_GWFLOW, ONLY: Basin_gwflow
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_cfs, Basin_cms, Basin_stflow_in, &
     &    Basin_sroff_cfs, Basin_ssflow_cfs, Basin_gwflow_cfs, Basin_stflow_out
      USE PRMS_SRUNOFF, ONLY: Basin_sroff
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      IMPLICIT NONE
! Functions
      EXTERNAL :: print_module
! Local Variables
      DOUBLE PRECISION :: area_fac
!      CHARACTER(LEN=8), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_strmflow
!***********************************************************************
      strmflow = 0

      IF ( Process(:3)=='run' ) THEN
!   Compute daily flow.
        area_fac = Cfs_conv*Active_area
        Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow
        Basin_stflow_out = Basin_stflow_in
        Basin_cfs = Basin_stflow_in*area_fac
        Basin_cms = Basin_cfs*CFS2CMS_CONV
        Basin_sroff_cfs = Basin_sroff*area_fac
        Basin_ssflow_cfs = Basin_ssflow*area_fac
        Basin_gwflow_cfs = Basin_gwflow*area_fac

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_strmflow = 'strmflow.f90 2014-12-02 19:06:41Z'
        CALL print_module(Version_strmflow, 'Streamflow Routing          ', 90)
!        MODNAME = 'strmflow'
      ENDIF

      END FUNCTION strmflow
