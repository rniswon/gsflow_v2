!***********************************************************************
! Transfer MODFLOW data to PRMS
!***********************************************************************
!     ******************************************************************
!     Mapping module to convert MODFLOW to PRMS states for use by GSFLOW
!   Declared Parameters
!     gvr_hru_id, gvr_cell_id
!     ******************************************************************
      INTEGER FUNCTION gsflow_mf2prms()
      USE GSFMODFLOW, ONLY: Mfq2inch_conv, Gwc_col, Gwc_row
      USE PRMS_SOILZONE, ONLY: Hrucheck, Gvr_hru_id, Gw2sm_grav
      USE GWFUZFMODULE, ONLY: SEEPOUT
      USE PRMS_MODULE, ONLY: Process, Nhrucell, Gvr_cell_id
      IMPLICIT NONE
! Functions
      EXTERNAL print_module
! Local Variables
      INTEGER :: i
!      CHARACTER(LEN=14) :: MODNAME
! Save Variables
      CHARACTER(LEN=80), SAVE :: Version_gsflow_mf2prms
!***********************************************************************
      gsflow_mf2prms = 0

      IF ( Process(:3)=='run' ) THEN
        DO i = 1, Nhrucell
          IF ( Hrucheck(Gvr_hru_id(i))==1 ) &
     &         Gw2sm_grav(i) = SEEPOUT(Gwc_col(Gvr_cell_id(i)), Gwc_row(Gvr_cell_id(i)))*Mfq2inch_conv(i)
        ENDDO
!
! Add irrigation to HRU from AG Package
!
! From irrigation wells
!
        soilzone_gain = 0.0
        mf_q2prms_inch = DELT*Mfl2_to_acre*Mfl_to_inch
        IF ( Iunit(66) > 0 ) then
          DO J = 1, NUMIRRWELSP
            IRWL = IRRWELVAR(J)
            NMCL = NUMCELLS(IRWL)
            DO K = 1, NMCL
              ihru = IRRROW_GW(K,IRWL)
              soilzone_gain(ihru) = soilzone_gain(ihru) + WELLIRRPRMS(k,j)*mf_q2prms_inch/HRU_PERV(IHRU)
            END DO
          END DO
!
! From segment diversions     
!
          DO J = 1, NUMIRRDIVERSIONSP
            SGNM = IRRSEG(J)
            NMCL = DVRCH(SGNM)
            DO K=1,NMCL        
              ihru = IRRROW_GW(K,SGNM)
              soilzone_gain(ihru) = soilzone_gain(ihru) + DIVERSIONIRRPRMS(k,j)*mf_q2prms_inch/HRU_PERV(ihru)
            END DO
          END DO
        END IF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_gsflow_mf2prms = 'gsflow_mf2prms.f90 2017-11-15 09:58:00Z'
        CALL print_module(Version_gsflow_mf2prms, 'GSFLOW MODFLOW to PRMS      ', 90)
!        MODNAME = 'gsflow_mf2prms'
      ENDIF

      END FUNCTION gsflow_mf2prms
