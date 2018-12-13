! ... Module files used for hydrotherm for definition of data groups
! ... $Revision: 6461 $//$Date: 2009/03/31 21:30:03 $
MODULE f_units
  ! ... fortran unit assignments
  IMPLICIT NONE
  SAVE
  INTEGER, PARAMETER :: fuins=7, fuinc=8,  &
       fuclog=30, fupdef=10, fup=11, fuen=12, fut=13, fusat=14, fuden=15, fuvis=16,  &
       fupot=17, fuvel=18, fubcf=19, fubcf2=33, fusrc=20, futhp=21, fupor=22, fuperm=23,  &
       fubal=24, fuorst=25, fures=26, fupltsca=27, fupltsca2=35, fupltvec=28, fuplttimser=29,  &
       fuicpp=31, fudimno=32, fuwtelev=34
  INTEGER :: fustdin=5, fustdout=6
END MODULE f_units

MODULE math_constants
! ...  mathematical constants
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  DOUBLE PRECISION, PARAMETER :: pi = 3.1415926535898_kdp, twopi = 2*pi
END MODULE math_constants

MODULE mesh
  ! ... Mesh geometry data
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE 
  LOGICAL :: irad
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ax, ay, az
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: dx, dy, dz
  DOUBLE PRECISION :: avgdx, avgdy, avgdz, xcmin, xcmax, ycmin, ycmax, zcmin, zcmax
  INTEGER, DIMENSION(:), ALLOCATABLE :: mrno
  INTEGER, DIMENSION(:), ALLOCATABLE :: nb, npic
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: ci, np, npp
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: lprntxif, lprntyif, lprntzif
  INTEGER :: mrnomax
  INTEGER :: nx, ny, nz, nxx, nyy, nzz, nxy, nxz, nxyz, npicmax, npiccons
  INTEGER :: nxxz, nxzz, nxxyz, nxyyz, nxyzz
CONTAINS
  SUBROUTINE alloc_mesh(a_status)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
    nxy = nx*ny
    nxz = nx*nz
    nxxz = (nx+1)*nz
    nxzz = nx*(nz+1)
    nxyz = nx*ny*nz
    nxxyz = (nx+1)*ny*nz
    nxyyz = nx*(ny+1)*nz
    !***This reduction can not be done until usy, uwy logic is improved
    !***      IF(ny == 1) nxyyz = 1
    nxyzz = nx*ny*(nz+1)
    ALLOCATE(ax(nxxyz), ay(nxyyz), az(nxyzz), dx(nx), dy(ny), dz(nz), &
         ci(6,nxyz), mrno(nxyz), nb(ny), npic(nxyz), np(nxz,ny), npp(2*nxz,ny), &
         lprntxif(nxxz,ny), lprntyif(nxz,ny+1), lprntzif(nxzz,ny),  &
         STAT=a_status)
    IF(a_status == 0) THEN
       ax = 0._kdp
       ay = 0._kdp
       az = 0._kdp
       dx = 0._kdp
       dy = 0._kdp
       dz = 0._kdp
       mrno = 0
       nb = 0
       npic = 0
       np = 0
       npp = 0
       lprntxif = 0
       lprntyif = 0
       lprntzif = 0
    ENDIF
  END SUBROUTINE alloc_mesh
END MODULE mesh

MODULE bc
  ! ... Boundary condition data
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  LOGICAL :: unconfined
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: seeping
  INTEGER :: nhcond, nprecip, nseep, nspecpbc
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: ibc, prflag
  DOUBLE PRECISION :: ptop
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ptopa
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ehassoc, tcassoc
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: cdtn, qprecip, tflux, denflux, ehflux, qhflux
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: land_surf_area, qseep
CONTAINS
  SUBROUTINE alloc_bc(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    ALLOCATE(ptopa(nxy),cdtn(nxy),qprecip(nxy),tflux(nxy),denflux(nxy),ehflux(nxy),qhflux(nxy), &
         land_surf_area(nxy), qseep(nxyz), ibc(nxz,ny), prflag(nxz,ny), seeping(nxz,ny),  &
         ehassoc(nxyz), tcassoc(nxyz),  &
         STAT=a_status)
    !*** should make the b.c. arrays the length of the number of b.c. cells of each type
    IF(a_status == 0) THEN
       ptopa = 0._kdp
       cdtn = 0._kdp
       qprecip = 0._kdp
       tflux = 0._kdp
       denflux = 0._kdp
       ehflux = 0._kdp
       qhflux = 0._kdp
       ehassoc = 0._kdp
       tcassoc = 0._kdp
    ENDIF
  END SUBROUTINE alloc_bc
END MODULE bc

MODULE fdeq
  ! ... Finite difference equation coefficients, Jacobian, residuals, weights
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  LOGICAL :: ioptupst
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: c, d, f, g 
  ! ... Jacobian derivatives for Newton-Raphson
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: dhrsh, dhrsp, dhrwh, dhrwp, dqhh, &
       dqhp, dth, dtp, drsh, drsp, drwh, drwp, dxvdsh, dxvdsp, dxvdwh, dxvdwp, &
       dyvdsh, dyvdsp, dyvdwh, dyvdwp, dzvdsh, dzvdsp, dzvdwh, dzvdwp, &
       dzvdgsh, dzvdgsp, dzvdgwh, dzvdgwp 
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: reside, residm
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: hrs, hrw
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: tx, txk, ty, tyk, tz, tzk
  !...Spatial weighting factors
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: usx, usy, usz, uwx, uwy, uwz
  !...Density and viscosity coefficients in difference equations
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: xvds, xvdw, xvs, xvw, xds, xdw, yvds, yvdw, yvs, & 
       yvw, yds, ydw, zvdgs, zvdgw, zvds, zvdw, zvs, zvw, zds, zdw
  DOUBLE PRECISION :: pcee=0._kdp, pcem=0._kdp, resmaxm, resmaxe, resmas, reseng, resmaxmall, resmaxeall
  DOUBLE PRECISION :: reseold, reseold2, resmold, resmold2
  INTEGER :: imres, jmres, kmres, ieres, jeres, keres, ltimresmaxm, ltimresmaxe
  DOUBLE PRECISION :: potdif
CONTAINS
  SUBROUTINE alloc_fdeq(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
    ALLOCATE(c(nxyz),d(nxyz),f(nxyz),g(nxyz), &
         dhrsh(nxyz), dhrsp(nxyz), dhrwh(nxyz), dhrwp(nxyz), dqhh(nxyz), &
         dqhp(nxyz), dth(nxyz), dtp(nxyz), drsh(nxyz), drsp(nxyz), drwh(nxyz), drwp(nxyz), &
         dxvdsh(nxxyz), dxvdsp(nxxyz), dxvdwh(nxxyz), dxvdwp(nxxyz), &
         dyvdsh(nxyyz), dyvdsp(nxyyz), dyvdwh(nxyyz), dyvdwp(nxyyz), &
         dzvdsh(nxyzz), dzvdsp(nxyzz), dzvdwh(nxyzz), dzvdwp(nxyzz), &
         dzvdgsh(nxyzz), dzvdgsp(nxyzz), dzvdgwh(nxyzz), dzvdgwp(nxyzz), &
         reside(nxyz), residm(nxyz), &
         hrs(nxyz), hrw(nxyz), &
         tx(nxxyz), txk(nxxyz), ty(nxyyz), tyk(nxyyz), tz(nxyzz), tzk(nxyzz), &
         usx(nxxyz), usy(nxyyz), usz(nxyzz), uwx(nxxyz), uwy(nxyyz), uwz(nxyzz), &
         xvds(nxxyz), xvdw(nxxyz), xvs(nxxyz), xvw(nxxyz), xds(nxxyz), xdw(nxxyz), &
         yvds(nxyyz), yvdw(nxyyz), yvs(nxyyz), & 
         yvw(nxyyz), yds(nxyyz), ydw(nxyyz), &
         zvdgs(nxyzz), zvdgw(nxyzz), zvds(nxyzz), zvdw(nxyzz), zvs(nxyzz), zvw(nxyzz), &
         zds(nxyzz), zdw(nxyzz), &
         STAT=a_status)
    IF(a_status == 0) THEN
       c = 0._kdp
       d = 0._kdp
       f = 0._kdp
       g = 0._kdp
       dhrsh = 0._kdp
       dhrsp = 0._kdp
       dhrwh = 0._kdp
       dhrwp = 0._kdp
       dqhh = 0._kdp
       dqhp = 0._kdp
       dth = 0._kdp
       dtp = 0._kdp
       drsh = 0._kdp
       drsp = 0._kdp
       drwh = 0._kdp
       drwp = 0._kdp
       dxvdsh = 0._kdp
       dxvdsp = 0._kdp
       dxvdwh = 0._kdp
       dxvdwp = 0._kdp
       dyvdsh = 0._kdp
       dyvdsp = 0._kdp
       dyvdwh = 0._kdp
       dyvdwp = 0._kdp
       dzvdsh = 0._kdp
       dzvdsp = 0._kdp
       dzvdwh = 0._kdp
       dzvdwp = 0._kdp
       dzvdgsh = 0._kdp
       dzvdgsp = 0._kdp
       dzvdgwh = 0._kdp
       dzvdgwp = 0._kdp
       reside = 0._kdp
       residm = 0._kdp
       hrs = 0._kdp
       hrw = 0._kdp
       tx = 0._kdp
       txk = 0._kdp
       ty = 0._kdp
       tyk = 0._kdp
       tz = 0._kdp
       tzk = 0._kdp
       usx = 0._kdp
       usy = 0._kdp
       usz = 0._kdp
       uwx = 0._kdp
       uwy = 0._kdp
       uwz = 0._kdp
       xvds = 0._kdp
       xvdw = 0._kdp
       xvs = 0._kdp
       xvw = 0._kdp
       xds = 0._kdp
       xdw = 0._kdp
       yvds = 0._kdp
       yvdw = 0._kdp
       yvs = 0._kdp
       yvw = 0._kdp
       yds = 0._kdp
       ydw = 0._kdp
       zvdgs = 0._kdp
       zvdgw = 0._kdp
       zvds = 0._kdp
       zvdw = 0._kdp
       zvs = 0._kdp
       zvw = 0._kdp
       zds = 0._kdp
       zdw = 0._kdp
    ENDIF
  END SUBROUTINE alloc_fdeq
END MODULE fdeq

MODULE i_c
  ! ... Initial condition data
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  LOGICAL :: initphi
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: pinit, plith
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: phiinit
CONTAINS
  SUBROUTINE alloc_ic(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    ALLOCATE(pinit(nxyz), plith(nxyz), phiinit(nxyz), &
         STAT=a_status)
    IF(a_status == 0) THEN
       pinit = 0._kdp
       plith = 0._kdp
       phiinit = 0._kdp
    ENDIF
  END SUBROUTINE alloc_ic
END MODULE i_c

MODULE parameters
  ! ... Physical parameters of the fluid and region
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  INTEGER, PARAMETER :: mprxparm=8, mptlparm=mprxparm+12, mprock=9
  DOUBLE PRECISION :: grav, patm, pwb, pwr, bb, cc, lambda, gamma
  DOUBLE PRECISION :: ssr, swr
  DOUBLE PRECISION :: ykfactor, zkfactor
  !..Relative permeabilities
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: rs, rw
  DOUBLE PRECISION,  DIMENSION(:), ALLOCATABLE :: phi, xkc, ykc, zkc, xk, yk, zk
  !...Porous matrix properties
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: beta, df, phfwt, phfwtdt
  DOUBLE PRECISION, DIMENSION(mprock) :: ykrxkxfac, zkrxkxfac
  LOGICAL, DIMENSION(mprock) :: ilrxtype
  LOGICAL :: lrxftreq, lrxftimreq
  DOUBLE PRECISION, DIMENSION(mprxparm,mprock) :: rxparm
  DOUBLE PRECISION, DIMENSION(mprxparm,mprock,8) :: rxftparm
  DOUBLE PRECISION, DIMENSION(2:4,mprock,8) :: rxftimparm
  INTEGER :: nrxtype
  INTEGER, DIMENSION(:), ALLOCATABLE :: icrxtype
  INTEGER, DIMENSION(mprock) :: irxused
  INTEGER, DIMENSION(mprxparm,mprock,2) :: irxftopt
  INTEGER, DIMENSION(2:4,mprock,2) :: irxftimopt
CONTAINS
  SUBROUTINE alloc_parameters(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
    ALLOCATE(rs(nxyz), rw(nxyz), phi(nxyz), xkc(nxyz), ykc(nxyz), zkc(nxyz), &
         xk(nxyz), yk(nxyz), zk(nxyz), &
         beta(nxyz), df(nxyz), phfwt(nxyz), phfwtdt(nxyz), &
         icrxtype(nxyz),  &
         STAT=a_status)
    IF(a_status == 0) THEN
       rs = 0._kdp
       rw = 0._kdp
       phi = 0._kdp
       xkc = 0._kdp
       ykc = 0._kdp
       zkc = 0._kdp
       xk = 0._kdp
       yk = 0._kdp
       zk = 0._kdp
       beta = 0._kdp
       df = 0._kdp
       phfwt = 0._kdp
       phfwtdt = 0._kdp
       ykrxkxfac = 0._kdp
       zkrxkxfac = 0._kdp
       ilrxtype = .FALSE.
       rxparm = 0._kdp
       rxftparm = 0._kdp
       rxftimparm = 0._kdp
       icrxtype = 0
       irxused = 0
       irxftopt = 0
       irxftimopt = 0
    ENDIF
  END SUBROUTINE alloc_parameters
END MODULE parameters

MODULE control
  ! ... Parameters to control the simulation
  USE machine_constants, ONLY: kdp
  USE parameters, ONLY: mptlparm
  IMPLICIT NONE
  SAVE
  LOGICAL :: tmcut
  LOGICAL :: gui
  LOGICAL, DIMENSION(200) :: ierr
  INTEGER :: ilnrimax, iphschgtot, iphschglt, iphschgnr, iter_gm_max
  INTEGER, DIMENSION(21) :: itmcntl
  INTEGER, DIMENSION(3) :: iconvrg
  DOUBLE PRECISION :: maxdelt, mindelt
  !...Input and print control
  INTEGER, DIMENSION(28) :: ioptprta
  INTEGER :: icall, iplot
  INTEGER :: ifmt, kodrp, kodt
  INTEGER, DIMENSION(mptlparm) :: kod
  INTEGER, DIMENSION(20,3) :: ijktp
  INTEGER, DIMENSION(6) :: ioptpr
  INTEGER :: lnrimax, ltimemax, ltime, lnri, lnritotl, ltmcutmx, tot_iter_gm
  DOUBLE PRECISION :: tmincmx, pchgmxts, hchgmxts, wschgmx, flwrlx, deltmin, deltmax
  DOUBLE PRECISION :: prfreq, prtimenxt, tchg, timestr
  DOUBLE PRECISION :: hchgmxnr, pchgmxnr
  INTEGER :: iphschgmx
  INTEGER :: informt
  INTEGER :: iprsat, iprden, iprvis, iprpot, iprvel, iprbcflow, &
       iprsource, iprpmprop, iprdimno, plotfile_type
  INTEGER :: n_locations
  INTEGER :: ltmcntl, nppn
  INTEGER :: slmeth=0
  DOUBLE PRECISION :: print_press, print_enth, print_temp, print_satn, &
       print_dens, print_vis, print_pot, &
       print_vel, print_bcflow, print_source, &
       print_pmprop, print_poros, print_perm, & 
       print_balance, print_dump, print_dimno, print_resid, &
       print_plotscalar, print_plotvector, print_temporal 
  DOUBLE PRECISION :: timprp, timprh, timprt, timprsat, timprd, timprv, timprpot,  &
       timprvel, timprbcf, timprsrc, timprprop, timprpor, timprperm, timprbal, timprdump,  &
       timprresid, timprdimno, timprpscal, timprpvec, timprtemp
!!$  CONTAINS
!!$    SUBROUTINE alloc_control(a_status)
!!$      IMPLICIT NONE
!!$      INTEGER, INTENT(OUT) :: a_status
!!$!      ALLOCATE(kod(n_param_array), &
!!$      ALLOCATE(kod(mptlparm), 
!!$           STAT=a_status)
!!$      IF(a_status == 0) THEN
!!$         KOD = 0
!!$      ENDIF
!!$      END SUBROUTINE alloc_control
END MODULE control

MODULE solver
  ! ... Array storage for the linear equation solver
  ! ...       used in formeq, solve, and ssor
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  INTEGER :: mbw
  DOUBLE PRECISION :: tol, wo
  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: alit
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: rlit, cscal
  INTEGER :: lssormax
CONTAINS
  SUBROUTINE alloc_solver(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
!!    mbw = 4*nz+3
!!    nxz = nx*nz
!!$   *****could be smaller when actual shape and bc are known
    ALLOCATE(alit(2*nxz,mbw), rlit(2*nxz), cscal(2*nxz),  &
         STAT=a_status)
!!    IF(a_status == 0) THEN
!!       alit = 0._kdp
!!       rlit = 0._kdp
!!    ENDIF
  END SUBROUTINE alloc_solver
END MODULE solver

MODULE solver_gmres
  ! ... Array storage for the linear equation solver
  ! ... for the pre conditioned generalized minimum residual solver
  USE machine_constants, ONLY: kdp
!!$  USE ilupc_mod
  IMPLICIT NONE
  SAVE
  INTEGER :: npaja, neq
  INTEGER :: maxit_gmres, m_save_dir
  INTEGER :: ilu_method, lev_fill, nnz_ilu
  DOUBLE PRECISION :: stop_tol_gmres, r_norm_gmres
  DOUBLE PRECISION :: drop_tol
  INTEGER, DIMENSION(:), ALLOCATABLE :: ja, ia
  INTEGER, DIMENSION(:), ALLOCATABLE :: jb, ib
  INTEGER, DIMENSION(:), ALLOCATABLE :: jailu, iilu, levs
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: av, rhs, xxs
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: bv, diagc, diagr
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ailu
CONTAINS
  SUBROUTINE alloc_solver_gmres(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
!!$    ALLOCATE(av(npaja), ja(npaja), ia(2*mrnomax), &
    ALLOCATE(av(npaja), bv(npaja), diagc(2*mrnomax), diagr(2*mrnomax),  &
         rhs(2*mrnomax), xxs(2*mrnomax), &
         STAT=a_status)
  END SUBROUTINE alloc_solver_gmres
  SUBROUTINE alloc_ilu_gmres(a_status)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
    ALLOCATE(ailu(nnz_ilu+1), jailu(nnz_ilu+1), iilu(neq), levs(nnz_ilu+1),  &
         STAT=a_status)
  END SUBROUTINE alloc_ilu_gmres
END MODULE solver_gmres

MODULE source
  ! ... Point source and well source data
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  DOUBLE PRECISION,  DIMENSION(:), ALLOCATABLE :: q, qh, qhi, qhcondflx, qhinodsrc, qhnodsrc,  &
       qhwelsrc, qnodsrc, qwelsrc,  &
       qt, qv
  !..Well screen layer index
  INTEGER, DIMENSION(:), ALLOCATABLE :: iq
CONTAINS
  SUBROUTINE alloc_source(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
    ALLOCATE(q(nxyz), qh(nxyz), qhi(nxy), qhcondflx(nxyz), qhinodsrc(nxyz), qhnodsrc(nxyz),  &
         qhwelsrc(nxyz), qnodsrc(nxyz), qwelsrc(nxyz),  &
         qt(nxy), qv(nxy),  &
         iq(nxyz), &
         STAT=a_status)
    IF(a_status == 0) THEN
       q = 0._kdp
       qh = 0._kdp
       qhi = 0._kdp
       qhcondflx = 0._kdp
       qhinodsrc = 0._kdp
       qhnodsrc = 0._kdp
       qhwelsrc = 0._kdp
       qnodsrc = 0._kdp
       qwelsrc = 0._kdp
       qt = 0._kdp
       qv = 0._kdp
       iq = 0
    ENDIF
  END SUBROUTINE alloc_source
END MODULE source

MODULE units
  ! ... Dimensional units, conversion factors, label strings, and file names
  USE machine_constants, ONLY: kdp
  USE parameters, ONLY: mptlparm  
  IMPLICIT NONE
  SAVE
  INTEGER :: iyr
  CHARACTER(LEN=10) :: version_name
  CHARACTER(LEN=80) :: version_string='$Name:  $'
  CHARACTER(LEN=80) :: title1, title2
  CHARACTER(LEN=80) :: tdfltdir
  CHARACTER(LEN=80) :: fname
  CHARACTER(LEN=40) :: usuff
  CHARACTER(LEN=80) :: phifile
  DOUBLE PRECISION, DIMENSION(mptlparm) :: unitfac = 0._kdp
  CHARACTER(LEN=13), DIMENSION(mptlparm) :: unitlabel = "             "
  CHARACTER(LEN=130) :: dash = '------------------------------------------------------------&
       &----------------------------------------------------------------------', &
       dots = '........................................................................&
       &..........................................................'
CONTAINS
  SUBROUTINE version
    IMPLICIT NONE
    INTEGER :: i1, i2
    ! ... Extract the version name for the header
    i1 = INDEX(version_string,'$Name')
    i2 = INDEX(version_string(i1+1:),'$')
    version_name = version_string(i1+6:i2-1)
  END SUBROUTINE version

  FUNCTION sec(years)
    IMPLICIT NONE
    DOUBLE PRECISION :: sec
    DOUBLE PRECISION, INTENT(IN) :: years
    sec = years*365.25_kdp*24.0_kdp*60.0_kdp*60.0_kdp
  END FUNCTION sec

  FUNCTION year(secs)
    IMPLICIT NONE
    DOUBLE PRECISION :: year
    DOUBLE PRECISION, INTENT(IN) :: secs
    year = secs/(365.25_kdp*24.0_kdp*60.0_kdp*60.0_kdp)
  END FUNCTION year

  FUNCTION cmyr(cmsec)
    IMPLICIT NONE
    DOUBLE PRECISION :: cmyr
    DOUBLE PRECISION, INTENT(IN) :: cmsec
    cmyr = cmsec*365.25_kdp*24.0_kdp*60.0_kdp*60.0_kdp
  END FUNCTION cmyr

  FUNCTION akm(cmr)
    IMPLICIT NONE
    DOUBLE PRECISION :: akm
    DOUBLE PRECISION, INTENT(IN) :: cmr
    akm = cmr*1.0e-5_kdp
  END FUNCTION akm

  FUNCTION amtr(cmr)
    IMPLICIT NONE
    DOUBLE PRECISION :: amtr
    DOUBLE PRECISION, INTENT(IN) :: cmr
    amtr = cmr*1.0e-2_kdp
  END FUNCTION amtr

  FUNCTION atm(dynes)
    IMPLICIT NONE
    DOUBLE PRECISION :: atm
    DOUBLE PRECISION, INTENT(IN) :: dynes
    atm = dynes/1.013e6_kdp
  END FUNCTION atm
END MODULE units

MODULE variables
  ! ... Dependent and independent variables
  USE machine_constants, ONLY: kdp
  IMPLICIT NONE
  SAVE
  ! ... Dependent variables
  INTEGER, DIMENSION(:), ALLOCATABLE :: ind, indoldnr, indoldt
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: dens, denw, viss, visw, satnw, wpot, spot, &
       swoldt, hspot, hwpot, fs_elev
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: tc
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: dhn, dpn
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: en, enoldt, enoldt2, xm, xmoldt, xmoldt2, &
       h, holdt, holdnr, p, poldt, poldnr
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: xsvel, xwvel, ysvel, ywvel, zsvel, zwvel, &
       xsmflx, xwmflx, ysmflx, ywmflx, zsmflx, zwmflx
  DOUBLE PRECISION :: xsvelmax, xwvelmax, ysvelmax, ywvelmax, zsvelmax, zwvelmax, &
       xsmflxmax, xwmflxmax, ysmflxmax, ywmflxmax, zsmflxmax, zwmflxmax
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: hrock
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: xsflux, xwflux, ysflux, ywflux, zsflux, zwflux
  ! ... Dimensionless numbers
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: pe, nu
  !...Independent variables
  DOUBLE PRECISION :: delt, deltoldt, time
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: rsq, xx, yy, zz, zzz, zztop, zls
  INTEGER, DIMENSION(:), ALLOCATABLE :: ktop
  ! ... Underrelaxation of pressure and enthalpy with seepage
  DOUBLE PRECISION :: maxenthic, maxpressic, wrelaxh, wrelaxp
  ! ... Cumulative amount variables
  DOUBLE PRECISION :: totfi, totfp, tothi, tothp, tfres, thres, tcfsbc, tcfpfbc, tcfsepbc,  &
       totwfi, totwfp, tchsbc, tchpfbc, tchsepbc, tchhcbc, totwhi, totwhp
  DOUBLE PRECISION :: fir0, ehir0, dfs_l2norm
CONTAINS
  SUBROUTINE alloc_variables(a_status)
    USE mesh
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: a_status
    !...
    ALLOCATE(ind(nxyz), indoldnr(nxyz), indoldt(nxyz), &
         dens(nxyz), denw(nxyz), viss(nxyz), visw(nxyz), satnw(nxyz), wpot(nxyz), spot(nxyz), &
         swoldt(nxyz), hspot(nxyz), hwpot(nxyz), fs_elev(nxyz),  &
         tc(nxyz), &
         dhn(nxyz), dpn(nxyz), &
         en(nxyz), enoldt(nxyz), enoldt2(nxyz), xm(nxyz), xmoldt(nxyz), xmoldt2(nxyz), &
         h(nxyz), holdt(nxyz), holdnr(nxyz), p(nxyz), pe(nxyz), poldt(nxyz), poldnr(nxyz), &
         nu(nxyz),  &
         xsvel(nxyz), xwvel(nxyz), ysvel(nxyz), ywvel(nxyz), zsvel(nxyz), zwvel(nxyz), &
         xsmflx(nxyz), xwmflx(nxyz), ysmflx(nxyz), ywmflx(nxyz), zsmflx(nxyz), zwmflx(nxyz), &
         hrock(nxyz), &
         xsflux(nxxyz), xwflux(nxxyz), ysflux(nxyyz), ywflux(nxyyz), zsflux(nxyzz), &
         zwflux(nxyzz), &
         rsq(nx+1), xx(nx), yy(ny), zz(nz), zzz(nz-1), zztop(nxy), zls(nxy),  &
         ktop(nxy), &
         STAT=a_status)
    IF(a_status == 0) THEN
       ind = 0
       indoldnr = 0
       indoldt = 0
       dens = 0._kdp
       denw = 0._kdp
       viss = 0._kdp
       visw = 0._kdp
       satnw = 0._kdp
       fs_elev = 0._kdp
       wpot = 0._kdp
       spot = 0._kdp
       swoldt = 0._kdp
       hspot = 0._kdp
       hwpot = 0._kdp
       tc = 0._kdp
       dhn = 0._kdp
       dpn = 0._kdp
       en = 0._kdp
       enoldt = 0._kdp
       enoldt2 = 0._kdp
       xm = 0._kdp
       xmoldt = 0._kdp
       xmoldt2 = 0._kdp
       h = 0._kdp
       holdt = 0._kdp
       holdnr = 0._kdp
       p = 0._kdp
       pe = 0._kdp
       poldt = 0._kdp
       poldnr = 0._kdp
       nu = 0._kdp
       xsvel = 0._kdp
       xwvel = 0._kdp
       ysvel = 0._kdp
       ywvel = 0._kdp
       zsvel = 0._kdp
       zwvel = 0._kdp
       xsmflx = 0._kdp
       xwmflx = 0._kdp
       ysmflx = 0._kdp
       ywmflx = 0._kdp
       zsmflx = 0._kdp
       zwmflx = 0._kdp
       hrock = 0._kdp
       xsflux = 0._kdp
       xwflux = 0._kdp
       ysflux = 0._kdp
       ywflux = 0._kdp
       zsflux = 0._kdp
       zwflux = 0._kdp
       rsq = 0._kdp
       xx = 0._kdp
       yy = 0._kdp
       zz = 0._kdp
       zzz = 0._kdp
       zztop = 0._kdp
       ktop = 0
       tcfpfbc = 0._kdp
       tchpfbc = 0._kdp
    ENDIF
  END SUBROUTINE alloc_variables
END MODULE variables
