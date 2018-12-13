!***********************************************************************
! Routes water between segments in the system as inflow equals outflow
!***********************************************************************
      INTEGER FUNCTION strmflow_in_out()
      USE PRMS_MODULE, ONLY: Process, Nsegment
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_BASIN, ONLY: Active_area, CFS2CMS_CONV
      USE PRMS_GWFLOW, ONLY: Basin_gwflow
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_cfs, Basin_cms, Basin_stflow_in, &
     &    Basin_sroff_cfs, Basin_ssflow_cfs, Basin_gwflow_cfs, Basin_stflow_out, &
     &    Seg_inflow, Seg_outflow, Seg_upstream_inflow, Seg_lateral_inflow, Flow_out
      USE PRMS_ROUTING, ONLY: Obsin_segment, Segment_order, Tosegment
      USE PRMS_SRUNOFF, ONLY: Basin_sroff
      USE PRMS_OBS, ONLY: Streamflow_cfs
      IMPLICIT NONE
! Functions
      EXTERNAL :: print_module
! Local Variables
      INTEGER :: i, iorder, toseg
      DOUBLE PRECISION :: area_fac
      CHARACTER(LEN=80), SAVE :: Version_strmflow
!***********************************************************************
      strmflow_in_out = 0

      IF ( Process(:3)=='run' ) THEN
        Seg_inflow = 0.0D0
        Seg_outflow = 0.0D0
        Seg_upstream_inflow = 0.0D0
        Flow_out = 0.0D0
        DO i = 1, Nsegment
          iorder = Segment_order(i)
          toseg = Tosegment(iorder)
          IF ( Obsin_segment(iorder)>0 ) Seg_upstream_inflow(iorder) = Streamflow_cfs(Obsin_segment(iorder))
          Seg_inflow(iorder) = Seg_upstream_inflow(iorder) + Seg_lateral_inflow(iorder)
          Seg_outflow(iorder) = Seg_inflow(iorder)
          IF ( toseg==0 ) THEN
            Flow_out = Flow_out + Seg_outflow(iorder)
          ELSE
            Seg_upstream_inflow(toseg) = Seg_upstream_inflow(toseg) + Seg_outflow(iorder)
          ENDIF
        ENDDO
        area_fac = Cfs_conv*Active_area
        Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow ! not equal to basin_stflow_out if replacement flows
        Basin_stflow_out = Flow_out/area_fac
        Basin_cfs = Flow_out
        Basin_cms = Basin_cfs*CFS2CMS_CONV
        Basin_sroff_cfs = Basin_sroff*area_fac
        Basin_ssflow_cfs = Basin_ssflow*area_fac
        Basin_gwflow_cfs = Basin_gwflow*area_fac
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_strmflow = 'strmflow_in_out.f90 2017-07-11 11:06:00Z'
        CALL print_module(Version_strmflow, 'Streamflow Routing          ', 90)
      ENDIF

      END FUNCTION strmflow_in_out
