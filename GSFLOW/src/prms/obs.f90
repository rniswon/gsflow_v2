!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
      MODULE PRMS_OBS
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=3), SAVE :: MODNAME
      INTEGER, SAVE :: Nsnow, Nlakeelev, Nwind, Nhumid, Rain_flag
!   Declared Variables
      INTEGER, SAVE :: Rain_day
      REAL, SAVE, ALLOCATABLE :: Pan_evap(:), Runoff(:), Precip(:)
      REAL, SAVE, ALLOCATABLE :: Humidity(:), Wind_speed(:)
      REAL, SAVE, ALLOCATABLE :: Tmax(:), Tmin(:), Solrad(:), Snowdepth(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Streamflow_cfs(:), Streamflow_cms(:)
      ! Lake Module Variables
      REAL, SAVE, ALLOCATABLE :: Gate_ht(:), Lake_elev(:)
!   Declared Parameters
      INTEGER, SAVE :: Runoff_units, Rain_code(12)
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun, obssetdims
      EXTERNAL :: obs_restart
!***********************************************************************
      obs = 0

      IF ( Process(:3)=='run' ) THEN
        obs = obsrun()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        obs = obssetdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        obs = obsdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file>0 ) CALL obs_restart(1)
        obs = obsinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL obs_restart(0)
      ENDIF

      END FUNCTION obs

!***********************************************************************
!     obssetdims - declares obs module specific dimensions
!***********************************************************************
      INTEGER FUNCTION obssetdims()
      USE PRMS_MODULE, ONLY: MAXDIM
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL read_error
!***********************************************************************
      obssetdims = 0

      IF ( decldim('nsnow', 0, MAXDIM, 'Number of snow-depth-measurement stations')/=0 ) CALL read_error(7, 'nsnow')
      IF ( decldim('nlakeelev', 0, MAXDIM, &
     &     'Maximum number of lake elevations for any rating table data set')/=0 ) CALL read_error(7, 'nlakeelev')
      IF ( decldim('nwind', 0, MAXDIM, 'Number of wind-speed measurement stations')/=0 ) CALL read_error(7, 'nwind')
      IF ( decldim('nhumid', 0, MAXDIM, 'Number of relative humidity measurement stations')/=0 ) CALL read_error(7, 'nhumid')

      END FUNCTION obssetdims

!***********************************************************************
!     obsdecl - makes public variable declarations for the obs module
!   Declared Parameters
!     rain_code
!***********************************************************************
      INTEGER FUNCTION obsdecl()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Precip_flag, Model, Nratetbl, Ntemp, Nrain, Nsol, Nobs, Nevap
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, getdim, declparam
      EXTERNAL read_error, print_module
! Local Variable
      CHARACTER(LEN=80), SAVE :: Version_obs
!***********************************************************************
      obsdecl = 0

      Version_obs = 'obs.f90 2017-09-27 12:07:00Z'
      CALL print_module(Version_obs, 'Time Series Data            ', 90)
      MODNAME = 'obs'

!   Declared Variables
      IF ( Nobs>0 ) THEN
        ALLOCATE ( Runoff(Nobs) )
        IF ( declvar(MODNAME, 'runoff', 'nobs', Nobs, 'real', &
     &       'Streamflow at each measurement station', &
     &       'runoff_units', Runoff)/=0 ) CALL read_error(8, 'runoff')
        ALLOCATE ( Streamflow_cfs(Nobs) )
        IF ( declvar(MODNAME, 'streamflow_cfs', 'nobs', Nobs, 'double', &
     &       'Streamflow at each measurement station', &
     &       'cfs', Streamflow_cfs)/=0 ) CALL read_error(8, 'streamflow_cfs')
        ALLOCATE ( Streamflow_cms(Nobs) )
        IF ( declvar(MODNAME, 'streamflow_cms', 'nobs', Nobs, 'double', &
     &       'Streamflow at each measurement station', &
     &       'cms', Streamflow_cms)/=0 ) CALL read_error(8, 'streamflow_cms')
        IF ( declparam(MODNAME, 'runoff_units', 'one', 'integer', &
     &       '0', '0', '1', &
     &       'Measured streamflow units', 'Measured streamflow units (0=cfs; 1=cms)', &
     &       'none')/=0 ) CALL read_error(1, 'runoff_units')
      ENDIF

      IF ( Nrain>0 ) THEN
        ALLOCATE ( Precip(Nrain) )
        IF ( declvar(MODNAME, 'precip', 'nrain', Nrain, 'real', &
     &       'Precipitation at each measurement station', &
     &       'precip_units', Precip)/=0 ) CALL read_error(8, 'precip')
      ENDIF

      IF ( Ntemp>0 ) THEN
        ALLOCATE ( Tmin(Ntemp) )
        IF ( declvar(MODNAME, 'tmin', 'ntemp', Ntemp, 'real', &
     &       'Minimum air temperature at each measurement station', &
     &       'temp_units', Tmin)/=0 ) CALL read_error(8, 'tmin')
        ALLOCATE ( Tmax(Ntemp) )
        IF ( declvar(MODNAME, 'tmax', 'ntemp', Ntemp, 'real', &
     &       'Maximum air temperature at each measurement station', &
     &       'temp_units', Tmax)/=0 ) CALL read_error(8, 'tmax')
      ENDIF

      IF ( Nsol>0 ) THEN
        ALLOCATE ( Solrad(Nsol) )
        IF ( declvar(MODNAME, 'solrad', 'nsol', Nsol, 'real', &
     &       'Solar radiation at each measurement station', &
     &       'Langleys', Solrad)/=0 ) CALL read_error(8, 'solrad')
      ENDIF

      Nsnow = getdim('nsnow')
      IF ( Nsnow==-1 ) CALL read_error(6, 'nsnow')
      Nhumid = getdim('nhumid')
      IF ( Nhumid==-1 ) CALL read_error(6, 'nhumid')
      Nwind = getdim('nwind')
      IF ( Nwind==-1 ) CALL read_error(6, 'nwind')
      Nlakeelev = getdim('nlakeelev')
      IF ( Nlakeelev==-1 ) CALL read_error(6, 'nlakeelev')

      IF ( Model==99 ) THEN
        IF ( Nsnow==0 ) Nsnow = 1
        IF ( Nhumid==0 ) Nhumid = 1
        IF ( Nwind==0 ) Nwind = 1
        IF ( Nlakeelev==0 ) Nlakeelev = 1
      ENDIF

      IF ( Nsnow>0 ) THEN
        ALLOCATE ( Snowdepth(Nsnow) )
        IF ( declvar(MODNAME, 'snowdepth', 'nsnow', Nsnow, 'real', &
     &       'Snow depth at each measurement station', &
     &       'inches', Snowdepth)/=0 ) CALL read_error(8, 'snowdepth')
      ENDIF

      IF ( Nevap>0 ) THEN
        ALLOCATE ( Pan_evap(Nevap) )
        IF ( declvar(MODNAME, 'pan_evap', 'nevap', Nevap, 'real', &
     &       'Pan evaporation at each measurement station', &
     &       'inches', Pan_evap)/=0 ) CALL read_error(8, 'pan_evap')
      ENDIF

      IF ( Nhumid>0 ) THEN
        ALLOCATE ( Humidity(Nhumid) )
        IF ( declvar(MODNAME, 'humidity', 'nhumid', Nhumid, 'real', &
     &       'Relative humidity at each measurement station', &
     &       'percentage', Humidity)/=0 ) CALL read_error(8, 'humidity')
      ENDIF

      IF ( Nwind>0 ) THEN
        ALLOCATE ( Wind_speed(Nwind) )
        IF ( declvar(MODNAME, 'wind_speed', 'nwind', Nwind, 'real', &
     &       'Wind speed at each measurement station', &
     &       'mph', Wind_speed)/=0 ) CALL read_error(8, 'wind_speed')
      ENDIF

!   Declared Parameters
      Rain_flag = 0
      IF ( Precip_flag==6 ) Rain_flag = 1
      IF ( Rain_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'rain_day', 'one', 1, 'integer', &
     &       'Flag to set the form of any precipitation to rain (0=determine form; 1=rain)', &
     &       'none', Rain_day)/=0 ) CALL read_error(8, 'rain_day')
        IF ( declparam(MODNAME, 'rain_code', 'nmonths', 'integer', &
     &       '2', '1', '5', &
     &       'Flag indicating rule for precipitation station use', &
     &       'Monthly (January to December) flag indicating rule for'// &
     &       ' precipitation measurement station use (1=only'// &
     &       ' precipitation if the regression stations have'// &
     &       ' precipitation; 2=only precipitation'// &
     &       ' if any station in the basin has precipitation;'// &
     &       ' 3=precipitation if xyz says so; 4=only'// &
     &       ' precipitation if rain_day variable is set to 1; 5=only'// &
     &       ' precipitation if psta_freq_nuse stations have precipitation)', &
     &       'none')/=0 ) CALL read_error(1, 'rain_code')
      ENDIF

! Lake Variables
      IF ( Nratetbl>0 ) THEN
        ALLOCATE ( Gate_ht(Nratetbl) )
        IF ( declvar(MODNAME, 'gate_ht', 'nratetbl', Nratetbl, 'real', &
     &       'Height of the gate opening at each dam with a gate', &
     &       'inches', Gate_ht)/=0 ) CALL read_error(8, 'gate_ht')
      ENDIF

      IF ( Nlakeelev>0 ) THEN
        ALLOCATE ( Lake_elev(Nlakeelev) )
        IF ( declvar(MODNAME, 'lake_elev', 'nlakeelev', Nlakeelev, 'real', &
     &       'Elevation of each simulated lake surface', &
     &       'feet', Lake_elev)/=0 ) CALL read_error(8, 'lake_elev')
      ENDIF

      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Nratetbl, Ntemp, Nrain, Nsol, Nobs, Nevap, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
!***********************************************************************
      obsinit = 0

      Runoff_units = 0
      IF ( Nobs>0 ) THEN
        IF ( getparam(MODNAME, 'runoff_units', 1, 'integer', Runoff_units)/=0 ) CALL read_error(2, 'runoff_units')
      ENDIF

      IF ( Rain_flag==1 ) THEN
        IF ( getparam(MODNAME, 'rain_code', 12, 'integer', Rain_code)/=0 ) CALL read_error(2, 'rain_code')
      ENDIF

      IF ( Init_vars_from_file==0 ) THEN
        IF ( Nobs>0 ) THEN
          Runoff = 0.0
          Streamflow_cfs = 0.0D0
          Streamflow_cms = 0.0D0
        ENDIF
        IF ( Nrain>0 ) Precip = 0.0
        Rain_day = 0
        IF ( Ntemp>0 ) THEN
          Tmax = 0.0
          Tmin = 0.0
        ENDIF
        IF ( Nsol>0 ) Solrad = 0.0
        IF ( Nevap>0 ) Pan_evap = 0.0
        IF ( Nsnow>0 ) Snowdepth = 0.0
        IF ( Nlakeelev>0 ) Lake_elev = 0.0
        IF ( Nratetbl>0 ) Gate_ht = 0.0
        IF ( Nhumid>0 ) Humidity = 0.0
        IF ( Nwind>0 ) Wind_speed = 0.0
      ENDIF

      END FUNCTION obsinit

! **********************************************************************
!     obsrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsrun()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Nratetbl, Ntemp, Nrain, Nsol, Nobs, Nevap
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV
      USE PRMS_SET_TIME, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC DBLE
      INTEGER, EXTERNAL :: readvar
      EXTERNAL :: read_error
! Local Variables
      INTEGER :: i
! **********************************************************************
      obsrun = 0

      IF ( Nobs>0 ) THEN
        IF ( readvar(MODNAME, 'runoff')/=0 ) CALL read_error(9, 'runoff')
        IF ( Runoff_units==1 ) THEN
          DO i = 1, Nobs
            Streamflow_cms(i) = DBLE( Runoff(i) )
            Streamflow_cfs(i) = Streamflow_cms(i)/CFS2CMS_CONV
          ENDDO
        ELSE
          DO i = 1, Nobs
            Streamflow_cfs(i) = DBLE( Runoff(i) )
            Streamflow_cms(i) = Streamflow_cfs(i)*CFS2CMS_CONV
          ENDDO
        ENDIF
      ENDIF

      IF ( Nrain>0 ) THEN
        IF ( readvar(MODNAME, 'precip')/=0 ) CALL read_error(9, 'precip')
      ENDIF

      IF ( Ntemp>0 ) THEN
        IF ( readvar(MODNAME, 'tmax')/=0 ) CALL read_error(9, 'tmax')
        IF ( readvar(MODNAME, 'tmin')/=0 ) CALL read_error(9, 'tmin')
      ENDIF

      IF ( Nsol>0 ) THEN
        IF ( readvar(MODNAME, 'solrad')/=0 ) CALL read_error(9, 'solrad')
      ENDIF

      IF ( Nevap>0 ) THEN
        IF ( readvar(MODNAME, 'pan_evap')/=0 ) CALL read_error(9, 'pan_evap')
      ENDIF

      IF ( Nsnow>0 ) THEN
        IF ( readvar(MODNAME, 'snowdepth')/=0 ) CALL read_error(9, 'snowdepth')
      ENDIF

      IF ( Rain_flag==1 ) THEN
        IF ( Rain_code(Nowmonth)==4 ) THEN
          IF ( readvar(MODNAME, 'rain_day')/=0 ) CALL read_error(9, 'rain_day')
        ENDIF
      ENDIF

      IF ( Nlakeelev>0 ) THEN
        IF ( readvar(MODNAME, 'lake_elev')/=0 ) CALL read_error(9, 'lake_elev')
      ENDIF

      IF ( Nratetbl>0 ) THEN
        IF ( readvar(MODNAME, 'gate_ht')/=0 ) CALL read_error(9, 'gate_ht')
      ENDIF

      IF ( Nhumid>0 ) THEN
        IF ( readvar(MODNAME, 'humidity')/=0 ) CALL read_error(9, 'humidity')
      ENDIF

      IF ( Nwind>0 ) THEN
        IF ( readvar(MODNAME, 'wind_speed')/=0 ) CALL read_error(9, 'wind_speed')
      ENDIF

      END FUNCTION obsrun

!***********************************************************************
!     obs_restart - write or read obs restart file
!***********************************************************************
      SUBROUTINE obs_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Nrain, Ntemp, Nsol, Nratetbl, Nobs, Nevap
      USE PRMS_OBS
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart, check_restart_dimen
      ! Local Variables
      INTEGER :: ierr, nrain_test, ntemp_test, nobs_test, nsol_test, nevap_test, nlakeelev_test
      INTEGER :: nsnow_test, nhumid_test, nwind_test, nratetbl_test
      CHARACTER(LEN=3) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Nrain, Ntemp, Nobs, Nsol, Nevap, &
     &          Nsnow, Nhumid, Nwind, Nratetbl, Nlakeelev
        IF ( Nrain>0 ) WRITE ( Restart_outunit ) Precip
        IF ( Ntemp>0 ) THEN
          WRITE ( Restart_outunit ) Tmax
          WRITE ( Restart_outunit ) Tmin
        ENDIF
        IF ( Nobs>0 ) THEN
          WRITE ( Restart_outunit ) Runoff
          WRITE ( Restart_outunit ) Streamflow_cfs
          WRITE ( Restart_outunit ) Streamflow_cms
        ENDIF
        IF ( Nsol>0 ) WRITE ( Restart_outunit ) Solrad
        IF ( Nevap>0 ) WRITE ( Restart_outunit ) Pan_evap
        IF ( Nsnow>0 ) WRITE ( Restart_outunit ) Snowdepth
        IF ( Nhumid>0 ) WRITE ( Restart_outunit ) Humidity
        IF ( Nwind>0 ) WRITE ( Restart_outunit ) Wind_speed
        IF ( Rain_flag==1 ) WRITE ( Restart_outunit ) Rain_day
        IF ( Nratetbl>0 ) WRITE ( Restart_outunit ) Gate_ht
        IF ( Nlakeelev>0 ) WRITE ( Restart_outunit ) Lake_elev
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) nrain_test, ntemp_test, nobs_test, nsol_test, nevap_test, &
     &         nsnow_test, nhumid_test, nwind_test, nratetbl_test, nlakeelev_test
        ierr = 0
        CALL check_restart_dimen('nrain', nrain_test, Nrain, ierr)
        CALL check_restart_dimen('ntemp', ntemp_test, Ntemp, ierr)
        CALL check_restart_dimen('nobs', nobs_test, Nobs, ierr)
        CALL check_restart_dimen('nsol', nsol_test, Nsol, ierr)
        CALL check_restart_dimen('nevap', nevap_test, Nevap, ierr)
        CALL check_restart_dimen('nsnow', nsnow_test, Nsnow, ierr)
        CALL check_restart_dimen('nhumid', nhumid_test, Nhumid, ierr)
        CALL check_restart_dimen('nwind', nwind_test, Nwind, ierr)
        CALL check_restart_dimen('nratetbl', nratetbl_test, Nratetbl, ierr)
        CALL check_restart_dimen('nlakeelev', nlakeelev_test, Nlakeelev, ierr)
        IF ( ierr==1 ) STOP
        IF ( Nrain>0 ) READ ( Restart_inunit ) Precip
        IF ( Ntemp>0 ) THEN
          READ ( Restart_inunit ) Tmax
          READ ( Restart_inunit ) Tmin
        ENDIF
        IF ( Nobs>0 ) THEN
          READ ( Restart_inunit ) Runoff
          READ ( Restart_inunit ) Streamflow_cfs
          READ ( Restart_inunit ) Streamflow_cms
        ENDIF
        IF ( Nsol>0 ) READ ( Restart_inunit ) Solrad
        IF ( Nevap>0 ) READ ( Restart_inunit ) Pan_evap
        IF ( Nsnow>0 ) READ ( Restart_inunit ) Snowdepth
        IF ( Nhumid>0 ) READ ( Restart_inunit ) Humidity
        IF ( Nwind>0 ) READ ( Restart_inunit ) Wind_speed
        IF ( Rain_flag==1 ) READ ( Restart_inunit ) Rain_day
        IF ( Nratetbl>0 ) READ ( Restart_inunit ) Gate_ht
        IF ( Nlakeelev>0 ) READ ( Restart_inunit ) Lake_elev
      ENDIF
      END SUBROUTINE obs_restart
