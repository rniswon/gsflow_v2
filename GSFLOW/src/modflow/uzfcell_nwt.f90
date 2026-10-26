module UZFCELL
! Cell-based unsaturated-zone package translated from MODFLOW 6 UzfCellGroup.
! Waves and Brooks-Corey properties are stored per groundwater cell.
! Vertical neighbors are linked with ivertcon. Land-surface cells have landflag = 1.
  use GLOBAL, only: ITMUNI
  use GWFBASMODULE, only: DELT
  implicit none

  double precision, parameter :: DZERO = 0.0d0
  double precision, parameter :: DHALF = 0.5d0
  double precision, parameter :: DONE = 1.0d0
  double precision, parameter :: DTWO = 2.0d0
  double precision, parameter :: DTHREE = 3.0d0
  double precision, parameter :: DFOUR = 4.0d0
  double precision, parameter :: DSIX = 6.0d0
  double precision, parameter :: DEP20 = 1.0d20
  double precision, parameter :: DEM3 = 1.0d-3
  double precision, parameter :: DEM4 = 1.0d-4
  double precision, parameter :: DEM5 = 1.0d-5
  double precision, parameter :: DEM6 = 1.0d-6
  double precision, parameter :: DEM7 = 1.0d-7
  double precision, parameter :: DEM9 = 1.0d-9
  double precision, parameter :: DEM10 = 1.0d-10
  double precision, parameter :: DEM12 = 1.0d-12
  double precision, parameter :: DEM14 = 1.0d-14
  double precision, parameter :: DEM15 = 1.0d-15
  double precision, parameter :: DEM20 = 1.0d-20
  double precision, parameter :: DEM30 = 1.0d-30
  double precision, parameter :: DPREC = epsilon(1.0d0)

  integer :: IUZFCB, IETFLG, ISEEPFLG, IGWETFLG
  integer :: NTRAILWAV, NWAVESETS, NUZF
  integer :: UZF_KPER, ISSFLAGOLD
  integer :: nwav_iter, nwav_et

  integer, allocatable :: IUZFBND(:)
  integer, allocatable :: NODENUM(:)
  integer, allocatable :: UZIDX(:)
  integer, allocatable :: ISOLVE(:)

  double precision, allocatable :: thtr(:), thts(:), thti(:), eps(:)
  double precision, allocatable :: extwc(:), ha(:), hroot(:), rootact(:)
  double precision, allocatable :: etact(:)
  double precision, allocatable :: uzspst(:, :), uzthst(:, :), uzflst(:, :)
  double precision, allocatable :: uzdpst(:, :)
  integer, allocatable :: nwavst(:), nwav(:), ntrail(:)
  double precision, allocatable :: totflux(:), sinf(:), finf(:)
  double precision, allocatable :: pet(:), petmax(:), extdp(:), extdpuz(:)
  double precision, allocatable :: finf_rej(:), gwet(:)
  double precision, allocatable :: uzfarea(:), cellarea(:)
  double precision, allocatable :: celtop(:), celbot(:), landtop(:)
  double precision, allocatable :: watab(:), watabold(:)
  double precision, allocatable :: vks(:), seepk(:), surfdep(:)
  double precision, allocatable :: surflux(:), surfluxbelow(:), surfseep(:)
  ! Lagged gravity-reservoir depth, in MODFLOW length, added to the land surface.
  double precision, allocatable :: soilheadd(:)
  double precision, allocatable :: gwpet(:)
  integer, allocatable :: landflag(:), ivertcon(:)
  ! 1 when the water table is below this cell at the start of the time
  ! step. That choice stays fixed until the next time step.
  integer, allocatable :: wtbelow(:)

  double precision, allocatable :: pet_spec(:), extdp_spec(:), extwc_spec(:)
  double precision, allocatable :: ha_spec(:), hroot_spec(:), rootact_spec(:)
  double precision, allocatable :: wcnew(:), wcold(:)
  double precision, allocatable :: rchrate(:), gwdrate(:), rejrate(:)
  double precision, allocatable :: uzetrate(:), qstorate(:)
  ! Cumulative and current UZF budget terms, same layout as VBVL:
  ! (1) volume in, (2) volume out, (3) rate in, (4) rate out.
  ! Terms: infiltration, UZ storage, GW recharge, rejected infiltration,
  ! UZ ET, GW discharge, GW ET.
  double precision :: UZBVL(4, 7) = 0.0d0
  ! Log-spaced infiltration bins (L/T). PRMS drainage is snapped to these
  ! rates so small changes do not create a new kinematic wave every step.
  double precision :: fbins(51) = 0.0d0

  double precision, allocatable :: uzth_iter(:), uzdp_iter(:)
  double precision, allocatable :: uzfl_iter(:), uzsp_iter(:)
  double precision, allocatable :: uzth_et(:), uzdp_et(:)
  double precision, allocatable :: uzfl_et(:), uzsp_et(:)

contains

  subroutine sSCurve(x, range, dydx, y)
    double precision, intent(in) :: x
    double precision, intent(in) :: range
    double precision, intent(inout) :: dydx
    double precision, intent(inout) :: y
    double precision :: s, xs
    s = range
    if (s < DPREC) s = DPREC
    xs = x / s
    if (xs < DZERO) xs = DZERO
    if (xs <= DZERO) then
      y = DZERO
      dydx = DZERO
    else if (xs < DONE) then
      y = -DTWO * xs**DTHREE + DTHREE * xs**DTWO
      dydx = -DSIX * xs**DTWO + DSIX * xs
    else
      y = DONE
      dydx = DZERO
    end if
  end subroutine sSCurve

  subroutine sCubicLinear(x, range, dydx, y)
    double precision, intent(in) :: x
    double precision, intent(in) :: range
    double precision, intent(inout) :: dydx
    double precision, intent(inout) :: y
    double precision :: s, xs
    s = range
    if (s < DPREC) s = DPREC
    xs = x / s
    if (xs < DZERO) xs = DZERO
    if (xs <= DZERO) then
      y = DZERO
      dydx = DZERO
    else if (xs < DONE) then
      y = -DONE * xs**DTHREE + DTWO * xs**DTWO
      dydx = -DTHREE * xs**DTWO + DFOUR * xs
    else
      y = DONE
      dydx = DZERO
    end if
    ! dydx above is dy/d(x/range). Callers need dy/dx.
    dydx = dydx / s
  end subroutine sCubicLinear

  subroutine sCubic(x, range, dydx, y)
    double precision, intent(inout) :: x
    double precision, intent(inout) :: range
    double precision, intent(inout) :: dydx
    double precision, intent(inout) :: y
    double precision :: s, aa, bb
    double precision :: cof1, cof2, cof3
    dydx = DZERO
    y = DZERO
    if (range < DPREC) range = DPREC
    if (x < DPREC) x = DPREC
    s = range
    aa = -DSIX / (s**DTHREE)
    bb = -DSIX / (s**DTWO)
    cof1 = x**DTWO
    cof2 = -(DTWO * x) / (s**DTHREE)
    cof3 = DTHREE / (s**DTWO)
    y = cof1 * (cof2 + cof3)
    dydx = (aa * x**DTWO - bb * x)
    if (x <= DZERO) then
      y = DZERO
      dydx = DZERO
    else if ((x - s) > -DPREC) then
      y = DONE
      dydx = DZERO
    end if
  end subroutine sCubic

  subroutine sLinear(x, range, dydx, y)
    double precision, intent(inout) :: x
    double precision, intent(inout) :: range
    double precision, intent(inout) :: dydx
    double precision, intent(inout) :: y
    double precision :: s
    dydx = DZERO
    y = DZERO
    if (range < DPREC) range = DPREC
    if (x < DPREC) x = DPREC
    s = range
    y = DONE - (s - x) / s
    dydx = DONE / s
    if (y > DONE) then
      y = DONE
      dydx = DZERO
    end if
  end subroutine sLinear

  function etfunc_lin(efflndsrf, extdp_in, resid_pet, deriv_et, trhs, &
                      thcof, hgwf, celtop_in, celbot_in)
    double precision :: etfunc_lin
    double precision, intent(in) :: efflndsrf
    double precision, intent(in) :: extdp_in
    double precision, intent(in) :: resid_pet
    double precision, intent(inout) :: deriv_et
    double precision, intent(inout) :: trhs
    double precision, intent(inout) :: thcof
    double precision, intent(in) :: hgwf
    double precision, intent(in) :: celtop_in
    double precision, intent(in) :: celbot_in
    double precision :: etgw, range, depth, scale, thick, lin_scaling_fac
    etgw = DZERO
    if (hgwf > (efflndsrf - extdp_in) .and. hgwf < efflndsrf) then
      lin_scaling_fac = calc_lin_scaling_fac(hgwf, efflndsrf, extdp_in)
      etgw = resid_pet * lin_scaling_fac
      trhs = resid_pet - resid_pet * efflndsrf / extdp_in
      thcof = -resid_pet / extdp_in
      etgw = trhs - (thcof * hgwf)
    else if (hgwf >= efflndsrf) then
      trhs = resid_pet
      etgw = resid_pet
    else
      etfunc_lin = DZERO
      return
    end if
    depth = hgwf - (efflndsrf - extdp_in)
    thick = celtop_in - celbot_in
    if (depth > thick) depth = thick
    if (depth < DZERO) depth = DZERO
    range = DEM4 * extdp_in
    call sCubic(depth, range, deriv_et, scale)
    trhs = scale * trhs
    thcof = scale * thcof
    etgw = trhs - (thcof * hgwf)
    deriv_et = -deriv_et * etgw
    etfunc_lin = etgw
  end function etfunc_lin

  function etfunc_nlin(efflndsrf, extdp_in, resid_pet, deriv_et, trhs, &
                       thcof, hgwf)
    double precision :: etfunc_nlin
    double precision, intent(in) :: efflndsrf
    double precision, intent(in) :: extdp_in
    double precision, intent(in) :: resid_pet
    double precision, intent(inout) :: deriv_et
    double precision, intent(inout) :: trhs
    double precision, intent(inout) :: thcof
    double precision, intent(in) :: hgwf
    double precision :: etgw, range, depth, scale
    depth = hgwf - (efflndsrf - extdp_in)
    if (depth < DZERO) depth = DZERO
    etgw = resid_pet
    range = DEM3 * extdp_in
    call sCubic(depth, range, deriv_et, scale)
    etgw = etgw * scale
    trhs = etgw
    deriv_et = -deriv_et * etgw
    etfunc_nlin = etgw
  end function etfunc_nlin

  pure function calc_lin_scaling_fac(hgwf, lndsrf, extdp_in) result(sclfac)
    double precision, intent(in) :: hgwf
    double precision, intent(in) :: lndsrf
    double precision, intent(in) :: extdp_in
    double precision :: sclfac
    sclfac = (hgwf - (lndsrf - extdp_in)) / extdp_in
    if (sclfac > DONE) sclfac = DONE
  end function calc_lin_scaling_fac

  function cell_unsat_storage(icell, head) result(fm)
    ! Mobile unsaturated storage (length) from kinematic waves above head.
    ! Must be called with the wave state that belongs to that head/time.
    integer, intent(in) :: icell
    double precision, intent(in) :: head
    double precision :: fm
    double precision :: thk, d
    thk = celtop(icell) - max(celbot(icell), head)
    fm = DZERO
    if (thk > DZERO) then
      d = thk
      fm = unsat_stor(icell, d)
      if (fm < DZERO) fm = DZERO
    end if
  end function cell_unsat_storage

  function get_storage_change(carea, storold, stornew, delt_in, iss) result(qsto)
    ! Storage-change rate from start/end wave storage lengths (L3/T).
    integer, intent(in) :: iss
    double precision, intent(in) :: carea, storold, stornew, delt_in
    double precision :: qsto
    if (iss == 0 .and. delt_in > DZERO) then
      qsto = (stornew - storold) * carea / delt_in
    else
      qsto = DZERO
    end if
  end function get_storage_change

  subroutine wave_get(istor, icell, j, th, dp, fl, sp)
    integer, intent(in) :: istor, icell, j
    double precision, intent(out) :: th, dp, fl, sp
    if (istor == 0) then
      th = uzthst(j, icell)
      dp = uzdpst(j, icell)
      fl = uzflst(j, icell)
      sp = uzspst(j, icell)
    else if (istor == 1) then
      th = uzth_iter(j)
      dp = uzdp_iter(j)
      fl = uzfl_iter(j)
      sp = uzsp_iter(j)
    else
      th = uzth_et(j)
      dp = uzdp_et(j)
      fl = uzfl_et(j)
      sp = uzsp_et(j)
    end if
  end subroutine wave_get

  subroutine wave_put(istor, icell, j, th, dp, fl, sp)
    integer, intent(in) :: istor, icell, j
    double precision, intent(in) :: th, dp, fl, sp
    if (istor == 0) then
      uzthst(j, icell) = th
      uzdpst(j, icell) = dp
      uzflst(j, icell) = fl
      uzspst(j, icell) = sp
    else if (istor == 1) then
      uzth_iter(j) = th
      uzdp_iter(j) = dp
      uzfl_iter(j) = fl
      uzsp_iter(j) = sp
    else
      uzth_et(j) = th
      uzdp_et(j) = dp
      uzfl_et(j) = fl
      uzsp_et(j) = sp
    end if
  end subroutine wave_put

  subroutine wave_copy_count(isrc, ics, idst, icd)
    integer, intent(in) :: isrc, ics, idst, icd
    integer :: ncount
    if (isrc == 0) then
      ncount = nwavst(ics)
    else if (isrc == 1) then
      ncount = nwav_iter
    else
      ncount = nwav_et
    end if
    if (idst == 0) then
      nwavst(icd) = ncount
    else if (idst == 1) then
      nwav_iter = ncount
    else
      nwav_et = ncount
    end if
  end subroutine wave_copy_count

  subroutine wave_shift(idst, isrc, icd, ics, shft, strt, stp, cntr)
    integer, intent(in) :: idst, isrc, icd, ics, shft, strt, stp, cntr
    integer :: j, js
    double precision :: th, dp, fl, sp
    if (cntr == 0) return
    do j = strt, stp, cntr
      js = j + shft
      call wave_get(isrc, ics, js, th, dp, fl, sp)
      call wave_put(idst, icd, j, th, dp, fl, sp)
    end do
    call wave_copy_count(isrc, ics, idst, icd)
  end subroutine wave_shift


  subroutine setdata(icell, area, top, bot, loc_surfdep, loc_vks, loc_thtr, loc_thts, loc_thti, loc_eps, &
        loc_ntrail, loc_landflag, loc_ivertcon)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(in) :: area
    double precision, intent(in) :: top
    double precision, intent(in) :: bot
    double precision, intent(in) :: loc_surfdep
    double precision, intent(in) :: loc_vks
    double precision, intent(in) :: loc_thtr
    double precision, intent(in) :: loc_thts
    double precision, intent(in) :: loc_thti
    double precision, intent(in) :: loc_eps
    integer, intent(in) :: loc_ntrail
    integer, intent(in) :: loc_landflag
    integer, intent(in) :: loc_ivertcon
    !
    ! -- set the values for uzf cell icell
    landflag(icell) = loc_landflag
    ivertcon(icell) = loc_ivertcon
    surfdep(icell) = loc_surfdep
    uzfarea(icell) = area
    cellarea(icell) = area
    if (landflag(icell) == 1) then
      celtop(icell) = top - DHALF * surfdep(icell)
    else
      celtop(icell) = top
    end if
    celbot(icell) = bot
    vks(icell) = loc_vks
    seepk(icell) = loc_vks
    thtr(icell) = loc_thtr
    thts(icell) = loc_thts
    thti(icell) = loc_thti
    eps(icell) = loc_eps
    ntrail(icell) = loc_ntrail
    pet(icell) = DZERO
    extdp(icell) = DZERO
    extwc(icell) = DZERO
    ha(icell) = DZERO
    hroot(icell) = DZERO
  end subroutine setdata


  subroutine sethead(icell, hgwf)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(in) :: hgwf
    !
    ! -- set initial head
    watab(icell) = celbot(icell)
    if (hgwf > celbot(icell)) watab(icell) = hgwf
    if (watab(icell) > celtop(icell)) watab(icell) = celtop(icell)
    watabold(icell) = watab(icell)
  end subroutine sethead


  subroutine setdatafinf(icell, loc_finf)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(in) :: loc_finf
    !
    if (landflag(icell) == 1) then
      sinf(icell) = loc_finf
      finf(icell) = loc_finf
    else
      sinf(icell) = DZERO
      finf(icell) = DZERO
    end if
    finf_rej(icell) = DZERO
    surflux(icell) = DZERO
    surfluxbelow(icell) = DZERO
  end subroutine setdatafinf


  function bin_finf(finfprms, vks_in, hgwf, celtop_in) result(finf_temp)
    ! Snap a land-surface infiltration rate onto the log bins. Rates above
    ! vks are limited to vks and are not binned. Rates are left unchanged
    ! when the water table is already at the land-surface cell top.
    double precision, intent(in) :: finfprms, vks_in, hgwf, celtop_in
    double precision :: finf_temp
    integer :: ij1, ij2, ij2m1
    finf_temp = finfprms
    if (finfprms > vks_in) then
      finf_temp = vks_in
    else if (hgwf < celtop_in) then
      if (finf_temp < fbins(1)) then
        finf_temp = DZERO
      else
        if (finf_temp < fbins(25)) then
          ij1 = 25
          ij2 = 1
        else
          ij1 = 51
          ij2 = 25
        end if
        ij2m1 = ij2 - 1
        do while (ij1 > ij2m1)
          if (finf_temp >= fbins(ij1)) then
            finf_temp = fbins(ij1)
            exit
          end if
          ij1 = ij1 - 1
        end do
      end if
    end if
  end function bin_finf


  subroutine setdatauzfarea(icell, areamult)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(in) :: areamult
    !
    ! -- set uzf area
    uzfarea(icell) = cellarea(icell) * areamult
  end subroutine setdatauzfarea


  subroutine setdataet(icell, jbelow, loc_pet, loc_extdp)
    ! -- dummy
    integer, intent(in) :: icell
    integer, intent(in) :: jbelow
    double precision, intent(in) :: loc_pet
    double precision, intent(in) :: loc_extdp
    ! -- local
    double precision :: thick
    !
    if (landflag(icell) == 1) then
      pet(icell) = loc_pet
      gwpet(icell) = loc_pet
    else
      pet(icell) = DZERO
      gwpet(icell) = DZERO
    end if
    thick = celtop(icell) - celbot(icell)
    extdp(icell) = loc_extdp
    if (landflag(icell) > 0) then
      landtop(icell) = celtop(icell)
      petmax(icell) = pet(icell)
    end if
    !
    ! -- set uz extinction depth
    if (landtop(icell) - extdp(icell) < celbot(icell)) then
      extdpuz(icell) = thick
    else
      extdpuz(icell) = celtop(icell) - (landtop(icell) - extdp(icell))
    end if
    if (extdpuz(icell) < DZERO) extdpuz(icell) = DZERO
    if (extdpuz(icell) > DEM7 .and. extdp(icell) < DEM7) extdp(icell) = extdpuz(icell)
    !
    ! -- set loc_pet for underlying cell
    if (jbelow > 0) then
      landtop(jbelow) = landtop(icell)
      petmax(jbelow) = petmax(icell)
    end if
  end subroutine setdataet


  subroutine setgwpet(icell)
    ! -- modules
    ! -- dummy
    integer, intent(in) :: icell
    ! -- dummy
    double precision :: loc_pet
    !
    loc_pet = DZERO
    !
    ! -- reduce loc_pet for gw by uzet
    loc_pet = pet(icell) - etact(icell) / dble(DELT)
    if (loc_pet < DZERO) loc_pet = DZERO
    gwpet(icell) = loc_pet
  end subroutine setgwpet


  subroutine setbelowpet(icell, jbelow)
    ! -- modules
    ! -- dummy
    integer, intent(in) :: icell
    integer, intent(in) :: jbelow
    ! -- dummy
    double precision :: loc_pet
    !
    loc_pet = DZERO
    !
    ! -- transfer unmet loc_pet to lower cell
    !
    if (extdpuz(jbelow) > DEM3) then
      loc_pet = pet(icell) - etact(icell) / dble(DELT) - gwet(icell) / uzfarea(icell)
      if (loc_pet < DZERO) loc_pet = DZERO
    end if
    pet(jbelow) = loc_pet
  end subroutine setbelowpet


  subroutine setdataetwc(icell, jbelow, loc_extwc)
    ! -- dummy
    integer, intent(in) :: icell
    integer, intent(in) :: jbelow
    double precision, intent(in) :: loc_extwc
    !
    ! -- set extinction water content
    extwc(icell) = loc_extwc
    if (jbelow > 0) extwc(jbelow) = loc_extwc
  end subroutine setdataetwc


  subroutine setdataetha(icell, jbelow, loc_ha, loc_hroot, loc_rootact)
    ! -- dummy
    integer, intent(in) :: icell
    integer, intent(in) :: jbelow
    double precision, intent(in) :: loc_ha
    double precision, intent(in) :: loc_hroot
    double precision, intent(in) :: loc_rootact
    !
    ! -- set variables
    ha(icell) = loc_ha
    hroot(icell) = loc_hroot
    rootact(icell) = loc_rootact
    if (jbelow > 0) then
      ha(jbelow) = loc_ha
      hroot(jbelow) = loc_hroot
      rootact(jbelow) = loc_rootact
    end if
  end subroutine setdataetha


  subroutine advance(icell)
    ! -- dummy
    integer, intent(in) :: icell
    !
    ! -- set variables
    surfseep(icell) = DZERO
  end subroutine advance


  subroutine solve(jbelow, icell, totfluxtot, ietflag, issflag, iseepflag, hgwf, qfrommvr, ierr, reset_state, &
        trhs, thcof, deriv, watercontent)
    ! -- modules
    ! -- dummy
    integer, intent(in) :: jbelow !< number of underlying uzf object or 0 if none
    integer, intent(in) :: icell !< number of this uzf object
    double precision, intent(inout) :: totfluxtot !<
    integer, intent(in) :: ietflag !< et is off (0) or based one water content (1) or pressure (2)
    integer, intent(in) :: issflag !< steady state flag
    integer, intent(in) :: iseepflag !< discharge to land is active (1) or not (0)
    double precision, intent(in) :: hgwf !< head for cell icell
    double precision, intent(in) :: qfrommvr !< water inflow from mover
    integer, intent(inout) :: ierr !< flag indicating not enough waves
    logical, intent(in) :: reset_state !< flag indicating that waves should be reset after solution
    double precision, intent(inout), optional :: trhs !< total uzf rhs contribution to GWF model
    double precision, intent(inout), optional :: thcof !< total uzf hcof contribution to GWF model
    double precision, intent(inout), optional :: deriv !< derivate term for contribution to GWF model
    double precision, intent(inout), optional :: watercontent !< calculated water content
    ! -- local
    double precision :: test
    double precision :: scale
    double precision :: seep
    double precision :: finfact
    double precision :: derivfinf
    double precision :: trhsfinf
    double precision :: thcoffinf
    double precision :: trhsseep
    double precision :: thcofseep
    double precision :: deriv1
    double precision :: deriv2
    double precision :: fmrise
    double precision :: dtime
    !
    ! -- NWT stores the time-step length as default real.
    dtime = dble(DELT)
    ! -- initialize variables
    totfluxtot = DZERO
    fmrise = DZERO
    trhsfinf = DZERO
    thcoffinf = DZERO
    trhsseep = DZERO
    thcofseep = DZERO
    finf_rej(icell) = DZERO
    surflux(icell) = finf(icell) + qfrommvr / uzfarea(icell)
    watab(icell) = hgwf
    surfseep(icell) = DZERO
    seep = DZERO
    finfact = DZERO
    etact(icell) = DZERO
    surfluxbelow(icell) = DZERO
    if (ivertcon(icell) > 0) then
      finf(jbelow) = DZERO
    end if
    if (watab(icell) < celbot(icell)) watab(icell) = celbot(icell)
    !
    ! -- initialize derivative variables
    deriv1 = DZERO
    deriv2 = DZERO
    derivfinf = DZERO
    !
    ! -- save wave states for resetting after iteration.
    if (reset_state) then
      call wave_shift(1, 0, 1, icell, 0, 1, nwavst(icell), 1)
    end if
    !
    if (watab(icell) > celtop(icell)) watab(icell) = celtop(icell)
    !
    ! -- add water from mover to applied infiltration.
    if (surflux(icell) > vks(icell)) then
      surflux(icell) = vks(icell)
    end if
    !
    ! -- saturation excess rejected infiltration
    if (landflag(icell) == 1) then
      call rejfinf(icell, deriv1, hgwf, trhsfinf, thcoffinf, finfact)
      surflux(icell) = finfact
    end if
    !
    ! -- calculate rejected infiltration
    finf_rej(icell) = finf(icell) + (qfrommvr / uzfarea(icell)) - surflux(icell)
    !
    ! -- calculate groundwater discharge
    if (iseepflag > 0 .and. landflag(icell) == 1) then
      call gwseep(icell, deriv2, scale, hgwf, trhsseep, thcofseep, seep)
      surfseep(icell) = seep
    end if
    !
    ! -- route water through unsat zone, calc. storage change and recharge
    test = watab(icell)
    if (watabold(icell) - test < -DEM15) test = watabold(icell)
    if (celtop(icell) - test > DEM15) then
      if (issflag == 0) then
        call routewaves(totfluxtot, dtime, ietflag, icell, ierr)
        if (ierr > 0) return
        call uz_rise(icell, totfluxtot, fmrise)
        totflux(icell) = totfluxtot
        call freeze_rech(icell, jbelow, fmrise, dtime)
      else
        totflux(icell) = surflux(icell) * dtime
        totfluxtot = surflux(icell) * dtime
        call freeze_rech(icell, jbelow, DZERO, dtime)
      end if
      thcoffinf = DZERO
      trhsfinf = totflux(icell) * uzfarea(icell) / dtime
      if (.not. reset_state) then
        call update_wav(icell, dtime, issflag, 0)
      end if
    else
      totflux(icell) = surflux(icell) * dtime
      totfluxtot = surflux(icell) * dtime
      ! A cell that started dry still passes through-flow downward.
      ! Only storage swallowed by a rising water table stays here.
      if (wtbelow(icell) == 1) then
        call freeze_rech(icell, jbelow, DZERO, dtime)
      end if
      ! The cell is saturated, so the incoming flux is groundwater
      ! recharge. A land cell already has that rate, and its surfdep
      ! linearization, from rejfinf. A deeper cell does not.
      if (landflag(icell) /= 1) then
        thcoffinf = DZERO
        trhsfinf = totflux(icell) * uzfarea(icell) / dtime
      end if
      if (.not. reset_state) then
        call update_wav(icell, dtime, issflag, 1)
      end if
    end if
    !
    ! -- If formulating, then these variables will be present
    if (present(deriv)) deriv = deriv1 + deriv2 + derivfinf
    if (present(trhs)) trhs = trhsfinf + trhsseep
    if (present(thcof)) thcof = thcoffinf + thcofseep
    !
    ! -- Assign water content prior to resetting waves
    if (present(watercontent)) then
      watercontent = get_wcnew(icell)
    end if
    !
    ! -- reset waves to previous state for next iteration
    if (reset_state) then
      call wave_shift(0, 1, icell, 1, 0, 1, nwav_iter, 1)
    end if
  end subroutine solve


  subroutine rejfinf(icell, deriv, hgwf, trhs, thcof, finfact)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(inout) :: deriv
    double precision, intent(inout) :: finfact
    double precision, intent(inout) :: thcof
    double precision, intent(inout) :: trhs
    double precision, intent(in) :: hgwf
    ! -- local
    double precision :: x, range, scale, q
    !
    range = surfdep(icell)
    q = surflux(icell)
    finfact = q
    trhs = finfact * uzfarea(icell)
    x = celtop(icell) - hgwf
    call sLinear(x, range, deriv, scale)
    deriv = -q * deriv * uzfarea(icell) * scale
    if (scale < DONE) then
      finfact = q * scale
      trhs = finfact * uzfarea(icell) * celtop(icell) / range
      thcof = finfact * uzfarea(icell) / range
    end if
  end subroutine rejfinf


  subroutine set_seepk(icell, sk)
    integer, intent(in) :: icell
    double precision, intent(in) :: sk
    seepk(icell) = sk
  end subroutine set_seepk

  subroutine clear_uzf_soil_head()
    if (allocated(soilheadd)) soilheadd = DZERO
  end subroutine clear_uzf_soil_head

  subroutine set_uzf_soil_head(icell, depth)
    ! Full lagged gravity depth. The next groundwater pass sees this
    ! value; it is not blended.
    integer, intent(in) :: icell
    double precision, intent(in) :: depth
    if (.not. allocated(soilheadd)) return
    if (icell < 1 .or. icell > size(soilheadd)) return
    soilheadd(icell) = depth
  end subroutine set_uzf_soil_head

  subroutine gwseep(icell, deriv, scale, hgwf, trhs, thcof, seep)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(inout) :: deriv
    double precision, intent(inout) :: trhs
    double precision, intent(inout) :: thcof
    double precision, intent(inout) :: seep
    double precision, intent(out) :: scale
    double precision, intent(in) :: hgwf
    ! -- local
    double precision :: depth, thick, csepmx, t, f, dfdt, dqseepdh, land
    double precision, parameter :: SEEP_SMOOTH = 1.0d-2
    !
    seep = DZERO
    deriv = DZERO
    trhs = DZERO
    thcof = DZERO
    scale = DZERO
    ! Same discharge face as GSFLOW-USG. Conductance is K*area / (0.5 *
    ! thickness) with K = seepk (cell vertical K, same as legacy gwseep).
    ! The ramp is 1 cm. SURFDEP stays an infiltration property. A land
    ! cell stores its top half of SURFDEP below the land surface, so the
    ! face is put back on the land surface and then raised by the lagged
    ! gravity depth.
    land = celtop(icell)
    thick = land - celbot(icell)
    if (landflag(icell) == 1) then
      land = land + DHALF * surfdep(icell)
      thick = thick + DHALF * surfdep(icell)
    end if
    depth = hgwf - land
    if (allocated(soilheadd)) then
      if (icell >= 1 .and. icell <= size(soilheadd)) depth = depth - soilheadd(icell)
    end if
    if (thick <= DZERO .or. uzfarea(icell) <= DZERO) return
    if (depth <= DZERO) return
    csepmx = seepk(icell) * uzfarea(icell) / (DHALF * thick)
    if (csepmx <= DZERO) return
    if (depth < SEEP_SMOOTH) then
      t = depth / SEEP_SMOOTH
      f = DTWO * t * t - t * t * t
      dfdt = DFOUR * t - DTHREE * t * t
      seep = csepmx * SEEP_SMOOTH * f
      dqseepdh = csepmx * dfdt
      scale = f
    else
      seep = csepmx * depth
      dqseepdh = csepmx
      scale = DONE
    end if
    ! deriv stays zero. The caller adds thcof to HCOF and -trhs to RHS.
    thcof = -dqseepdh
    trhs = dqseepdh * hgwf - seep
  end subroutine gwseep


  subroutine simgwet(igwetflag, icell, hgwf, trhs, thcof, det)
    ! -- dummy
    integer, intent(in) :: igwetflag
    integer, intent(in) :: icell
    double precision, intent(in) :: hgwf
    double precision, intent(inout) :: trhs
    double precision, intent(inout) :: thcof
    double precision, intent(inout) :: det
    ! -- local
    double precision :: s, x, c, b, et
    !
    gwet(icell) = DZERO
    trhs = DZERO
    thcof = DZERO
    det = DZERO
    s = landtop(icell)
    x = extdp(icell)
    c = gwpet(icell)
    b = celbot(icell)
    if (b > hgwf) return
    if (x < DEM6) return
    if (igwetflag == 1) then
      et = etfunc_lin(s, x, c, det, trhs, thcof, hgwf, celtop(icell), celbot(icell))
    else if (igwetflag == 2) then
      et = etfunc_nlin(s, x, c, det, trhs, thcof, hgwf)
    end if
    ! gwet(icell) = et * uzfarea(icell)
    trhs = trhs * uzfarea(icell)
    thcof = thcof * uzfarea(icell)
    gwet(icell) = trhs - (thcof * hgwf)
    ! write(99,*)'in group', icell, gwet(icell)
  end subroutine simgwet


  subroutine uz_rise(icell, totfluxtot, fmrise)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(inout) :: totfluxtot
    double precision, intent(out) :: fmrise
    ! -- local
    double precision :: fm1, fm2, d1
    !
    ! -- storage swallowed by a rising water table
    fmrise = DZERO
    if (watab(icell) - watabold(icell) > DEM30) then
      d1 = celtop(icell) - watabold(icell)
      fm1 = unsat_stor(icell, d1)
      d1 = celtop(icell) - watab(icell)
      fm2 = unsat_stor(icell, d1)
      fmrise = fm1 - fm2
      if (fmrise < DZERO) fmrise = DZERO
      totfluxtot = totfluxtot + fmrise
    end if
  end subroutine uz_rise


  subroutine freeze_rech(icell, jbelow, absorb, delt)
    ! Keep recharge on the cell that held the water table at the start
    ! of the time step. A cell that started dry still passes through-flow
    ! downward. Storage swallowed by a rise stays on that cell.
    integer, intent(in) :: icell
    integer, intent(in) :: jbelow
    double precision, intent(in) :: absorb
    double precision, intent(in) :: delt
    double precision :: kept, down, fcheck
    kept = totflux(icell)
    if (jbelow > 0 .and. wtbelow(icell) == 1) then
      kept = absorb
      if (kept < DZERO) kept = DZERO
      if (kept > totflux(icell)) kept = totflux(icell)
      down = totflux(icell) - kept
      if (delt > DZERO) then
        finf(jbelow) = down / delt
      else
        finf(jbelow) = DZERO
      end if
      fcheck = finf(jbelow) - vks(jbelow)
      if (fcheck < DZERO) fcheck = DZERO
      finf(jbelow) = finf(jbelow) - fcheck
      surfluxbelow(icell) = finf(jbelow)
      kept = kept + fcheck * delt
    else if (jbelow > 0) then
      finf(jbelow) = DZERO
      surfluxbelow(icell) = DZERO
    end if
    totflux(icell) = kept
  end subroutine freeze_rech


  subroutine setwaves(icell)
    ! -- dummy
    ! -- local
    integer, intent(in) :: icell
    double precision :: bottom, top
    integer :: jk
    double precision :: thick
    !
    ! -- initialize
    totflux(icell) = DZERO
    nwavst(icell) = 1
    uzdpst(:, icell) = DZERO
    thick = celtop(icell) - watab(icell)
    do jk = 1, nwav(icell)
      uzthst(jk, icell) = thtr(icell)
    end do
    !
    ! -- initialize waves for first stress period
    if (thick > DZERO) then
      uzdpst(1, icell) = thick
      uzthst(1, icell) = thti(icell)
      top = uzthst(1, icell) - thtr(icell)
      if (top < DZERO) top = DZERO
      bottom = thts(icell) - thtr(icell)
      if (bottom < DZERO) bottom = DZERO
      uzflst(1, icell) = vks(icell) * (top / bottom)**eps(icell)
      if (uzthst(1, icell) < thtr(icell)) uzthst(1, icell) = thtr(icell)
      !
      ! -- calculate water stored in the unsaturated zone
      if (top > DZERO) then
        uzspst(1, icell) = DZERO
      else
        uzflst(1, icell) = DZERO
        uzspst(1, icell) = DZERO
      end if
      !
      ! no unsaturated zone
    else
      uzflst(1, icell) = DZERO
      uzdpst(1, icell) = DZERO
      uzspst(1, icell) = DZERO
      uzthst(1, icell) = thtr(icell)
    end if
  end subroutine


  subroutine routewaves(totfluxtot, delt, ietflag, icell, ierr)
    ! -- dummy
    double precision, intent(inout) :: totfluxtot
    double precision, intent(in) :: delt
    integer, intent(in) :: ietflag
    integer, intent(in) :: icell
    integer, intent(inout) :: ierr
    ! -- local
    double precision :: thick, thickold
    integer :: idelt, iwav, ik
    !
    ! -- initialize
    totflux(icell) = DZERO
    etact(icell) = DZERO
    thick = celtop(icell) - watab(icell)
    thickold = celtop(icell) - watabold(icell)
    !
    ! -- no uz, clear waves
    if (thickold < DZERO) then
      do iwav = 1, 6
        uzthst(iwav, icell) = thtr(icell)
        uzdpst(iwav, icell) = DZERO
        uzspst(iwav, icell) = DZERO
        uzflst(iwav, icell) = DZERO
        nwavst(icell) = 1
      end do
    end if
    idelt = 1
    do ik = 1, idelt
      call uzflow(thick, thickold, delt, ietflag, icell, ierr)
      if (ierr > 0) return
      totfluxtot = totfluxtot + totflux(icell)
    end do
  end subroutine routewaves


  subroutine uzflow(thick, thickold, delt, ietflag, icell, ierr)
    ! -- dummy
    double precision, intent(inout) :: thickold
    double precision, intent(inout) :: thick
    double precision, intent(in) :: delt
    integer, intent(in) :: ietflag
    integer, intent(in) :: icell
    integer, intent(inout) :: ierr
    ! -- local
    double precision :: ffcheck, time, feps1, feps2
    double precision :: thetadif, thetab, fluxb, oldsflx
    integer :: itrailflg, itester
    !
    time = DZERO
    totflux(icell) = DZERO
    itrailflg = 0
    oldsflx = uzflst(nwavst(icell), icell)
    call factors(feps1, feps2)
    !
    ! -- check for falling or rising water table
    if ((thick - thickold) > feps1) then
      thetadif = abs(uzthst(1, icell) - thtr(icell))
      if (thetadif > DEM6) then
        call wave_shift(0, 0, icell, icell, -1, nwavst(icell) + 1, 2, -1)
        if (uzdpst(2, icell) < DEM30) uzdpst(2, icell) = (ntrail(icell) + DTWO) * DEM6
        if (uzthst(2, icell) > thtr(icell)) then
          uzspst(2, icell) = uzflst(2, icell) / (uzthst(2, icell) - thtr(icell))
        else
          uzspst(2, icell) = DZERO
        end if
        uzthst(1, icell) = thtr(icell)
        uzflst(1, icell) = DZERO
        uzspst(1, icell) = DZERO
        uzdpst(1, icell) = thick
        nwavst(icell) = nwavst(icell) + 1
        if (nwavst(icell) >= nwav(icell)) then
          ! -- too many waves error
          ierr = 1
          return
        end if
      else
        uzdpst(1, icell) = thick
      end if
    end if
    thetab = uzthst(1, icell)
    fluxb = uzflst(1, icell)
    totflux(icell) = DZERO
    itester = 0
    ffcheck = (surflux(icell) - uzflst(nwavst(icell), icell))
    !
    ! -- increase new waves in infiltration changes
    if (ffcheck > feps2 .OR. ffcheck < -feps2) then
      nwavst(icell) = nwavst(icell) + 1
      if (nwavst(icell) >= nwav(icell)) then
        !
        ! -- too many waves error
        ierr = 1
        return
      end if
    else if (nwavst(icell) == 1) then
      itester = 1
    end if
    if (nwavst(icell) > 1) then
      if (ffcheck < -feps2) then
        call trailwav(icell, ierr)
        if (ierr > 0) return
        itrailflg = 1
      end if
      call leadwav(time, itester, itrailflg, thetab, fluxb, ffcheck, feps2, delt, icell)
    end if
    if (itester == 1) then
      totflux(icell) = totflux(icell) + (delt - time) * uzflst(1, icell)
      time = DZERO
      itester = 0
    end if
    !
    ! -- simulate et
    if (ietflag > 0) call uzet(icell, delt, ietflag, ierr)
    if (ierr > 0) return
  end subroutine uzflow


  subroutine factors(feps1, feps2)
    ! -- dummy
    double precision, intent(out) :: feps1
    double precision, intent(out) :: feps2
    double precision :: factor1
    double precision :: factor2
    !
    ! calculate constants for uzflow
    factor1 = DONE
    factor2 = DONE
    feps1 = DEM9
    feps2 = DEM9
    if (ITMUNI == 1) then
      factor1 = DONE / 86400.D0
    else if (ITMUNI == 2) then
      factor1 = DONE / 1440.D0
    else if (ITMUNI == 3) then
      factor1 = DONE / 24.0D0
    else if (ITMUNI == 5) then
      factor1 = 365.0D0
    end if
    factor2 = DONE / 0.3048
    feps1 = feps1 * factor1 * factor2
    feps2 = feps2 * factor1 * factor2
  end subroutine factors


  subroutine trailwav(icell, ierr)
    ! -- dummy
    integer, intent(in) :: icell
    integer, intent(inout) :: ierr
    ! -- local
    double precision :: smoist, smoistinc, ftrail, eps_m1
    double precision :: thtsrinv
    double precision :: flux1, flux2, theta1, theta2
    double precision :: fnuminc
    integer :: j, jj, jk, nwavstm1
    !
    ! -- initialize
    eps_m1 = dble(eps(icell)) - DONE
    thtsrinv = DONE / (thts(icell) - thtr(icell))
    nwavstm1 = nwavst(icell) - 1
    !
    ! -- initialize trailwaves
    smoist = (((surflux(icell) / vks(icell))** (DONE / eps(icell))) * (thts(icell) - thtr(icell))) + &
        thtr(icell)
    if (uzthst(nwavstm1, icell) - smoist > DEM9) then
      fnuminc = DZERO
      do jk = 1, ntrail(icell)
        fnuminc = fnuminc + float(jk)
      end do
      smoistinc = (uzthst(nwavstm1, icell) - smoist) / (fnuminc - DONE)
      jj = ntrail(icell)
      ftrail = dble(ntrail(icell)) + DONE
      do j = nwavst(icell), nwavst(icell) + ntrail(icell) - 1
        if (j > nwav(icell)) then
          ! -- too many waves error
          ierr = 1
          return
        end if
        if (j > nwavst(icell)) then
          uzthst(j, icell) = uzthst(j - 1, icell) - ((ftrail - float(jj)) * smoistinc)
        else
          uzthst(j, icell) = uzthst(j - 1, icell) - DEM9
        end if
        jj = jj - 1
        if (uzthst(j, icell) <= thtr(icell) + DEM9) uzthst(j, icell) = thtr(icell) + DEM9
        uzflst(j, icell) = vks(icell) * (((uzthst(j, icell) - thtr(icell)) * thtsrinv)**eps(icell))
        theta2 = uzthst(j - 1, icell)
        flux2 = uzflst(j - 1, icell)
        flux1 = uzflst(j, icell)
        theta1 = uzthst(j, icell)
        uzspst(j, icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), thtr(icell), eps(icell), &
        vks(icell))
        uzdpst(j, icell) = DZERO
        if (j == nwavst(icell)) then
          uzdpst(j, icell) = uzdpst(j, icell) + (ntrail(icell) + 1) * DEM9
        else
          uzdpst(j, icell) = uzdpst(j - 1, icell) - DEM9
        end if
      end do
      nwavst(icell) = nwavst(icell) + ntrail(icell) - 1
      if (nwavst(icell) >= nwav(icell)) then
        ! -- too many waves error
        ierr = 1
        return
      end if
    else
      uzdpst(nwavst, icell) = DZERO
      uzflst(nwavst, icell) = vks(icell) * (((uzthst(nwavst, icell) - thtr(icell)) * thtsrinv)**eps(icell))
      uzthst(nwavst, icell) = smoist
      theta2 = uzthst(nwavst(icell) - 1, icell)
      flux2 = uzflst(nwavst(icell) - 1, icell)
      flux1 = uzflst(nwavst(icell), icell)
      theta1 = uzthst(nwavst(icell), icell)
      uzspst(nwavst(icell), icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), thtr(icell), &
        eps(icell), vks(icell))
    end if
  end subroutine trailwav


  subroutine leadwav(time, itester, itrailflg, thetab, fluxb, ffcheck, feps2, delt, icell)
    ! -- dummy
    double precision, intent(inout) :: thetab
    double precision, intent(inout) :: fluxb
    double precision, intent(in) :: feps2
    double precision, intent(inout) :: time
    integer, intent(inout) :: itester
    integer, intent(inout) :: itrailflg
    double precision, intent(inout) :: ffcheck
    double precision, intent(in) :: delt
    integer, intent(in) :: icell
    ! -- local
    double precision :: bottomtime, shortest, fcheck
    double precision :: eps_m1, timenew, bottom, timedt
    double precision :: thtsrinv, diff, fluxhld2
    double precision :: flux1, flux2, theta1, theta2, ftest
    double precision, allocatable, dimension(:) :: checktime
    integer :: iflx, iremove, j, l
    integer :: nwavp1, jshort
    integer, allocatable, dimension(:) :: more
    !
    allocate (checktime(nwavst(icell)))
    allocate (more(nwavst(icell)))
    ftest = DZERO
    eps_m1 = dble(eps(icell)) - DONE
    thtsrinv = DONE / (thts(icell) - thtr(icell))
    !
    ! -- initialize new wave
    if (itrailflg == 0) then
      if (ffcheck > feps2) then
        uzflst(nwavst(icell), icell) = surflux(icell)
        if (uzflst(nwavst(icell), icell) < DEM30) uzflst(nwavst(icell), icell) = DZERO
        uzthst(nwavst(icell), icell) = (((uzflst(nwavst(icell), &
        icell) / vks(icell))** (DONE / eps(icell))) * (thts(icell) - thtr(icell))) + thtr(icell)
        theta2 = uzthst(nwavst(icell), icell)
        flux2 = uzflst(nwavst(icell), icell)
        flux1 = uzflst(nwavst(icell) - 1, icell)
        theta1 = uzthst(nwavst(icell) - 1, icell)
        uzspst(nwavst(icell), icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), thtr(icell), &
        eps(icell), vks(icell))
        uzdpst(nwavst(icell), icell) = DZERO
      end if
    end if
    !
    ! -- route all waves and interception of waves over times step
    diff = DONE
    timedt = DZERO
    iflx = 0
    fluxhld2 = uzflst(1, icell)
    if (nwavst(icell) == 0) itester = 1
    if (itester /= 1) then
      do while (diff > DEM6)
        nwavp1 = nwavst(icell) + 1
        timedt = delt - Time
        do j = 1, nwavst(icell)
          checktime(j) = DEP20
          more(j) = 0
        end do
        shortest = timedt
        if (nwavst(icell) > 2) then
          j = 2
          !
          ! -- calculate time until wave overtakes wave ahead
          nwavp1 = nwavst(icell) + 1
          do while (j < nwavp1)
            ftest = uzspst(j - 1, icell) - uzspst(j, icell)
            if (abs(ftest) > DEM30) then
              checktime(j) = (uzdpst(j, icell) - uzdpst(j - 1, icell)) / ftest
              if (checktime(j) < DEM30) checktime(j) = DEP20
            end if
            j = j + 1
          end do
        end if
        !
        ! - calc time until wave reaches bottom of cell
        bottomtime = DEP20
        if (nwavst(icell) > 1) then
          if (uzspst(2, icell) > DZERO) then
            bottom = uzspst(2, icell)
            if (bottom < DEM15) bottom = DEM15
            bottomtime = (uzdpst(1, icell) - uzdpst(2, icell)) / bottom
            if (bottomtime < DZERO) bottomtime = DEM12
          end if
        end if
        !
        ! -- calc time for wave interception
        jshort = 0
        do j = nwavst(icell), 3, -1
          if (shortest - checktime(j) > -DEM9) then
            more(j) = 1
            jshort = j
            shortest = checktime(j)
          end if
        end do
        do j = 3, nwavst(icell)
          if (shortest - checktime(j) < DEM9) then
            if (j /= jshort) more(j) = 0
          end if
        end do
        !
        ! -- what happens first, waves hits bottom or interception
        iremove = 0
        timenew = Time
        fcheck = (Time + shortest) - delt
        if (shortest < DEM7) fcheck = -DONE
        if (bottomtime < shortest .AND. Time + bottomtime < delt) then
          j = 2
          do while (j < nwavp1)
            !
            ! -- route waves
            uzdpst(j, icell) = uzdpst(j, icell) + uzspst(j, icell) * bottomtime
            j = j + 1
          end do
          fluxb = uzflst(2, icell)
          thetab = uzthst(2, icell)
          iflx = 1
          call wave_shift(0, 0, icell, icell, 1, 1, nwavst(icell) - 1, 1)
          iremove = 1
          timenew = time + bottomtime
          uzspst(1, icell) = DZERO
          !
          ! -- do waves intercept before end of time step
        else if (fcheck < DZERO .AND. nwavst(icell) > 2) then
          j = 2
          do while (j < nwavp1)
            uzdpst(j, icell) = uzdpst(j, icell) + uzspst(j, icell) * shortest
            j = j + 1
          end do
          !
          ! -- combine waves that intercept, remove a wave
          j = 3
          l = j
          do while (j < nwavst(icell) + 1)
            if (more(j) == 1) then
              l = j
              theta2 = uzthst(j, icell)
              flux2 = uzflst(j, icell)
              if (j == 3) then
                flux1 = fluxb
                theta1 = thetab
              else
                flux1 = uzflst(j - 2, icell)
                theta1 = uzthst(j - 2, icell)
              end if
              uzspst(j, icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), thtr(icell), &
        eps(icell), vks(icell))
              !
              ! -- update waves
              call wave_shift(0, 0, icell, icell, 1, l - 1, nwavst(icell) - 1, 1)
              l = nwavst(icell) + 1
              iremove = iremove + 1
            end if
            j = j + 1
          end do
          timenew = timenew + shortest
          !
          ! -- calc. total flux to bottom during remaining time in step
        else
          j = 2
          do while (j < nwavp1)
            uzdpst(j, icell) = uzdpst(j, icell) + uzspst(j, icell) * timedt
            j = j + 1
          end do
          timenew = delt
        end if
        totflux(icell) = totflux(icell) + fluxhld2 * (timenew - time)
        if (iflx == 1) then
          fluxhld2 = uzflst(1, icell)
          iflx = 0
        end if
        !
        ! -- remove dead waves
        nwavst(icell) = nwavst(icell) - iremove
        time = timenew
        diff = delt - Time
        if (nwavst(icell) == 1) then
          itester = 1
          exit
        end if
      end do
    end if
    deallocate (checktime)
    deallocate (more)
  end subroutine leadwav


  function leadspeed(theta1, theta2, flux1, flux2, loc_thts, loc_thtr, loc_eps, loc_vks)
    ! -- Return
    double precision :: leadspeed
    ! -- dummy
    double precision, intent(in) :: theta1
    double precision, intent(in) :: theta2
    double precision, intent(in) :: flux1
    double precision, intent(inout) :: flux2
    double precision, intent(in) :: loc_thts
    double precision, intent(in) :: loc_thtr
    double precision, intent(in) :: loc_eps
    double precision, intent(in) :: loc_vks
    ! -- local
    double precision :: comp1, comp2, thsrinv, epsfksths
    double precision :: eps_m1, comp3
    double precision :: fhold
    !
    eps_m1 = loc_eps - DONE
    thsrinv = DONE / (loc_thts - loc_thtr)
    epsfksths = loc_eps * loc_vks * thsrinv
    comp1 = theta2 - theta1
    comp2 = abs(flux2 - flux1)
    comp3 = theta1 - loc_thtr
    if (comp2 < DEM15) flux2 = flux1 + DEM15
    if (abs(comp1) < DEM30) then
      fhold = DEM30
      if (comp3 > DEM30) fhold = (comp3 * thsrinv)**loc_eps
      if (fhold < DEM30) fhold = DEM30
      leadspeed = epsfksths * (fhold**eps_m1)
    else
      leadspeed = (flux2 - flux1) / (theta2 - theta1)
    end if
    if (leadspeed < DEM30) leadspeed = DEM30
  end function leadspeed


  function unsat_stor(icell, d1)
    ! -- Return
    double precision :: unsat_stor
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(inout) :: d1
    ! -- local
    double precision :: fm
    integer :: j, k, nwavm1, jj
    !
    fm = DZERO
    j = nwavst(icell) + 1
    k = nwavst(icell)
    nwavm1 = k - 1
    if (d1 > uzdpst(1, icell)) d1 = uzdpst(1, icell)
    !
    ! -- find deepest wave above depth d1, counter held as j
    do while (k > 0)
      if (uzdpst(k, icell) - d1 < -DEM30) j = k
      k = k - 1
    end do
    if (j > nwavst(icell)) then
      fm = fm + (uzthst(nwavst(icell), icell) - thtr(icell)) * d1
    elseif (nwavst(icell) > 1) then
      if (j > 1) then
        fm = fm + (uzthst(j - 1, icell) - thtr(icell)) * (d1 - uzdpst(j, icell))
      end if
      do jj = j, nwavm1
        fm = fm + (uzthst(jj, icell) - thtr(icell)) * (uzdpst(jj, icell) - uzdpst(jj + 1, icell))
      end do
      fm = fm + (uzthst(nwavst(icell), icell) - thtr(icell)) * (uzdpst(nwavst(icell), icell))
    else
      fm = fm + (uzthst(1, icell) - thtr(icell)) * d1
    end if
    unsat_stor = fm
  end function unsat_stor


  subroutine update_wav(icell, delt, iss, itest)
    ! -- dummy
    integer, intent(in) :: icell
    integer, intent(in) :: itest
    integer, intent(in) :: iss
    double precision, intent(in) :: delt
    ! -- local
    double precision :: bot, depthsave, top
    double precision :: thick, thtsrinv
    integer :: nwavhld, k, j
    !
    bot = watab(icell)
    top = celtop(icell)
    thick = top - bot
    nwavhld = nwavst(icell)
    if (itest == 1) then
      uzflst(1, icell) = DZERO
      uzthst(1, icell) = thtr(icell)
      return
    end if
    if (iss == 1) then
      if (thts(icell) - thtr(icell) < DEM7) then
        thtsrinv = DONE / DEM7
      else
        thtsrinv = DONE / (thts(icell) - thtr(icell))
      end if
      totflux(icell) = surflux(icell) * delt
      watabold(icell) = watab(icell)
      uzthst(1, icell) = thti(icell)
      uzflst(1, icell) = vks(icell) * (((uzthst(1, icell) - thtr(icell)) * thtsrinv)**eps(icell))
      uzdpst(1, icell) = thick
      uzspst(1, icell) = thick
      nwavst(icell) = 1
    else
      !
      ! -- water table rises through waves
      if (watab(icell) - watabold(icell) > DEM30) then
        depthsave = uzdpst(1, icell)
        j = 0
        k = nwavst(icell)
        do while (k > 0)
          if (uzdpst(k, icell) - thick < -DEM30) j = k
          k = k - 1
        end do
        uzdpst(1, icell) = thick
        if (j > 1) then
          uzspst(1, icell) = DZERO
          nwavst(icell) = nwavst(icell) - j + 2
          uzthst(1, icell) = uzthst(j - 1, icell)
          uzflst(1, icell) = uzflst(j - 1, icell)
          if (j > 2) call wave_shift(0, 0, icell, icell, j - 2, 2, nwavhld - (j - 2), 1)
        elseif (j == 0) then
          uzspst(1, icell) = DZERO
          uzthst(1, icell) = uzthst(nwavst(icell), icell)
          uzflst(1, icell) = uzflst(nwavst(icell), icell)
          nwavst(icell) = 1
        end if
      end if
      !
      ! -- calculate new unsat. storage
      if (thick <= DZERO) then
        uzspst(1, icell) = DZERO
        nwavst(icell) = 1
        uzthst(1, icell) = thtr(icell)
        uzflst(1, icell) = DZERO
      end if
      watabold(icell) = watab(icell)
    end if
  end subroutine update_wav


  subroutine uzet(icell, delt, ietflag, ierr)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(in) :: delt
    integer, intent(in) :: ietflag
    integer, intent(inout) :: ierr
    ! -- local
    double precision :: diff
    double precision :: thetaout
    double precision :: fm
    double precision :: st
    double precision :: thtsrinv
    double precision :: epsfksthts
    double precision :: fmp
    double precision :: fktho
    double precision :: theta1
    double precision :: theta2
    double precision :: flux1
    double precision :: flux2
    double precision :: hcap
    double precision :: factor
    double precision :: tho
    double precision :: depth
    double precision :: extwc1
    double precision :: petsub
    integer :: i
    integer :: j
    integer :: jhold
    integer :: jk
    integer :: kj
    integer :: kk
    integer :: numadd
    integer :: k
    integer :: nwv
    integer :: itest
    !
    ! -- initialize
    etact(icell) = DZERO
    if (extdpuz(icell) < DEM7) return
    petsub = rootact(icell) * pet(icell) * extdpuz(icell) / extdp(icell)
    thetaout = delt * petsub / extdp(icell)
    if (ietflag == 1) thetaout = delt * pet(icell) / extdp(icell)
    if (thetaout < DEM10) return
    depth = uzdpst(1, icell)
    st = unsat_stor(icell, depth)
    if (st < DEM4) return
    !
    ! -- allocate temporary wave storage.
    nwv = nwavst(icell)
    itest = 0
    !
    ! store original wave characteristics
    call wave_shift(2, 0, 1, icell, 0, 1, nwv, 1)
    factor = DONE
    etact(icell) = DZERO
    if (thts(icell) - thtr(icell) < DEM7) then
      thtsrinv = 1.0 / DEM7
    else
      thtsrinv = DONE / (thts(icell) - thtr(icell))
    end if
    epsfksthts = eps(icell) * vks(icell) * thtsrinv
    etact(icell) = DZERO
    fmp = DZERO
    extwc1 = extwc(icell) - thtr(icell)
    if (extwc1 < DEM6) extwc1 = DEM7
    numadd = 0
    fm = st
    k = 0
    !
    ! -- loop for reducing aet to pet when et is head dependent
    do while (itest == 0)
      k = k + 1
      if (k > 1 .AND. ABS(fmp - petsub) > DEM5 * petsub) then
        factor = factor / (fm / petsub)
      end if
      !
      ! -- one wave shallower than extdp
      if (nwavst(icell) == 1 .AND. uzdpst(1, icell) <= extdpuz(icell)) then
        if (ietflag == 2) then
          tho = uzthst(1, icell)
          fktho = uzflst(1, icell)
          hcap = caph(icell, tho)
          thetaout = rate_et_z(icell, factor, fktho, hcap)
        end if
        if ((uzthst(1, icell) - thetaout) > thtr(icell) + extwc1) then
          uzthst(1, icell) = uzthst(1, icell) - thetaout
          uzflst(1, icell) = vks(icell) * (((uzthst(1, icell) - thtr(icell)) * thtsrinv)**eps(icell))
        else if (uzthst(1, icell) > thtr(icell) + extwc1) then
          uzthst(1, icell) = thtr(icell) + extwc1
          uzflst(1, icell) = vks(icell) * (((uzthst(1, icell) - thtr(icell)) * thtsrinv)**eps(icell))
        end if
        !
        ! -- all waves shallower than extinction depth
      else if (nwavst(icell) > 1 .AND. uzdpst(nwavst(icell), icell) > extdpuz(icell)) then
        if (ietflag == 2) then
          tho = uzthst(nwavst(icell), icell)
          fktho = uzflst(nwavst(icell), icell)
          hcap = caph(icell, tho)
          thetaout = rate_et_z(icell, factor, fktho, hcap)
        end if
        if (uzthst(nwavst(icell), icell) - thetaout > thtr(icell) + extwc1) then
          uzthst(nwavst(icell) + 1, icell) = uzthst(nwavst(icell), icell) - thetaout
          numadd = 1
        else if (uzthst(nwavst(icell), icell) > thtr(icell) + extwc1) then
          uzthst(nwavst(icell) + 1, icell) = thtr(icell) + extwc1
          numadd = 1
        end if
        if (numadd == 1) then
          uzflst(nwavst(icell) + 1, icell) = vks(icell) * (((uzthst(nwavst(icell) + 1, &
        icell) - thtr(icell)) * thtsrinv)**eps(icell))
          theta2 = uzthst(nwavst(icell) + 1, icell)
          flux2 = uzflst(nwavst(icell) + 1, icell)
          flux1 = uzflst(nwavst(icell), icell)
          theta1 = uzthst(nwavst(icell), icell)
          uzspst(nwavst(icell) + 1, icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), &
        thtr(icell), eps(icell), vks(icell))
          uzdpst(nwavst(icell) + 1, icell) = extdpuz(icell)
          nwavst(icell) = nwavst(icell) + 1
          if (nwavst(icell) > nwav(icell)) then
            !
            ! -- too many waves error, deallocate temp arrays and return
            ierr = 1
            goto 500
          end if
        else
          numadd = 0
        end if
        !
        ! -- one wave below extinction depth
      else if (nwavst(icell) == 1) then
        if (ietflag == 2) then
          tho = uzthst(1, icell)
          fktho = uzflst(1, icell)
          hcap = caph(icell, tho)
          thetaout = rate_et_z(icell, factor, fktho, hcap)
        end if
        if ((uzthst(1, icell) - thetaout) > thtr(icell) + extwc1) then
          if (thetaout > DEM30) then
            uzthst(2, icell) = uzthst(1, icell) - thetaout
            uzflst(2, icell) = vks(icell) * (((uzthst(2, icell) - thtr(icell)) * thtsrinv)**eps(icell))
            uzdpst(2, icell) = extdpuz(icell)
            theta2 = uzthst(2, icell)
            flux2 = uzflst(2, icell)
            flux1 = uzflst(1, icell)
            theta1 = uzthst(1, icell)
            uzspst(2, icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), thtr(icell), eps(icell), &
        vks(icell))
            nwavst(icell) = nwavst(icell) + 1
            if (nwavst(icell) > nwav(icell)) then
              !
              ! -- too many waves error
              ierr = 1
              goto 500
            end if
          end if
        else if (uzthst(1, icell) > thtr(icell) + extwc1) then
          if (thetaout > DEM30) then
            uzthst(2, icell) = thtr(icell) + extwc1
            uzflst(2, icell) = vks(icell) * (((uzthst(2, icell) - thtr(icell)) * thtsrinv)**eps(icell))
            uzdpst(2, icell) = extdpuz(icell)
            theta2 = uzthst(2, icell)
            flux2 = uzflst(2, icell)
            flux1 = uzflst(1, icell)
            theta1 = uzthst(1, icell)
            uzspst(2, icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), thtr(icell), eps(icell), &
        vks(icell))
            nwavst(icell) = nwavst(icell) + 1
            if (nwavst(icell) > nwav(icell)) then
              !
              ! -- too many waves error
              ierr = 1
              goto 500
            end if
          end if
        end if
      else
        !
        ! -- extinction depth splits waves
        if (uzdpst(1, icell) - extdpuz(icell) > DEM7) then
          j = 2
          jk = 0
          !
          ! -- locate extinction depth between waves
          do while (jk == 0)
            diff = uzdpst(j, icell) - extdpuz(icell)
            if (diff > dzero) then
              j = j + 1
            else
              jk = 1
            end if
          end do
          kk = j
          if (uzthst(j, icell) > thtr(icell) + extwc1) then
            !
            ! -- create a wave at extinction depth
            if (abs(diff) > DEM5) then
              call wave_shift(0, 0, icell, icell, -1, nwavst(icell) + 1, j, -1)
              uzdpst(j, icell) = extdpuz(icell)
              nwavst(icell) = nwavst(icell) + 1
              if (nwavst(icell) > nwav(icell)) then
                !
                ! -- too many waves error
                ierr = 1
                goto 500
              end if
            end if
            kk = j
          else
            jhold = nwavst(icell)
            i = j + 1
            do while (i < nwavst(icell))
              if (uzthst(i, icell) > thtr(icell) + extwc1) then
                jhold = i
                i = nwavst(icell) + 1
              end if
              i = i + 1
            end do
            j = jhold
            kk = jhold
          end if
        else
          kk = 1
        end if
        !
        ! -- all waves above extinction depth
        do while (kk <= nwavst(icell))
          if (ietflag == 2) then
            tho = uzthst(kk, icell)
            fktho = uzflst(kk, icell)
            hcap = caph(icell, tho)
            thetaout = rate_et_z(icell, factor, fktho, hcap)
          end if
          if (uzthst(kk, icell) > thtr(icell) + extwc1) then
            if (uzthst(kk, icell) - thetaout > thtr(icell) + extwc1) then
              uzthst(kk, icell) = uzthst(kk, icell) - thetaout
            else if (uzthst(kk, icell) > thtr(icell) + extwc1) then
              uzthst(kk, icell) = thtr(icell) + extwc1
            end if
            if (kk == 1) then
              uzflst(kk, icell) = vks(icell) * (((uzthst(kk, icell) - thtr(icell)) * thtsrinv)**eps(icell))
            end if
            if (kk > 1) then
              flux1 = vks(icell) * ((uzthst(kk - 1, icell) - thtr(icell)) * thtsrinv)**eps(icell)
              flux2 = vks(icell) * ((uzthst(kk, icell) - thtr(icell)) * thtsrinv)**eps(icell)
              uzflst(kk, icell) = flux2
              theta2 = uzthst(kk, icell)
              theta1 = uzthst(kk - 1, icell)
              uzspst(kk, icell) = leadspeed(theta1, theta2, flux1, flux2, thts(icell), thtr(icell), &
        eps(icell), vks(icell))
            end if
          end if
          kk = kk + 1
        end do
      end if
      !
      ! -- calculate aet
      kj = 1
      do while (kj <= nwavst(icell) - 1)
        if (abs(uzthst(kj, icell) - uzthst(kj + 1, icell)) < DEM6) then
          call wave_shift(0, 0, icell, icell, 1, kj + 1, nwavst(icell) - 1, 1)
          kj = kj - 1
          nwavst(icell) = nwavst(icell) - 1
        end if
        kj = kj + 1
      end do
      depth = uzdpst(1, icell)
      fm = unsat_stor(icell, depth)
      etact(icell) = st - fm
      fm = etact(icell) / dble(DELT)
      if (etact(icell) < dzero) then
        call wave_shift(0, 2, icell, 1, 0, 1, nwv, 1)
        nwavst(icell) = nwv
        etact(icell) = DZERO
      elseif (petsub - fm < -DEM15 .AND. ietflag == 2) then
        !
        ! -- aet greater than pet, reset and try again
        call wave_shift(0, 2, icell, 1, 0, 1, nwv, 1)
        nwavst(icell) = nwv
        etact(icell) = DZERO
      else
        itest = 1
      end if
      !
      ! -- end aet-pet loop for head dependent et
      fmp = fm
      if (k > 100) then
        itest = 1
      elseif (ietflag < 2) then
        fmp = petsub
        itest = 1
      end if
    end do
500 continue
    !
    ! -- deallocate temporary worker
  end subroutine uzet


  function caph(icell, tho)
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(in) :: tho
    ! -- local
    double precision :: caph, lambda, star
    !
    caph = -DEM6
    star = (tho - thtr(icell)) / (thts(icell) - thtr(icell))
    if (star < DEM15) star = DEM15
    lambda = DTWO / (eps(icell) - DTHREE)
    if (star > DEM15) then
      if (tho - thts(icell) < DEM15) then
        caph = ha(icell) * star**(-DONE / lambda)
      else
        caph = DZERO
      end if
    end if
  end function caph


  function rate_et_z(icell, factor, fktho, h)
    ! -- Return
    double precision :: rate_et_z
    ! -- dummy
    integer, intent(in) :: icell
    double precision, intent(in) :: factor, fktho, h
    !
    rate_et_z = factor * fktho * (h - hroot(icell))
    if (rate_et_z < DZERO) rate_et_z = DZERO
  end function rate_et_z


  function get_water_content_at_depth(icell, depth) result(theta_at_depth)
    ! -- dummy
    integer, intent(in) :: icell !< uzf cell containing depth
    double precision, intent(in) :: depth !< depth within the cell
    ! -- return
    double precision :: theta_at_depth
    ! -- local
    double precision :: d1
    double precision :: d2
    double precision :: f1
    double precision :: f2
    !
    if (watab(icell) < celtop(icell)) then
      if (celtop(icell) - depth > watab(icell)) then
        d1 = depth - DEM3
        d2 = depth + DEM3
        f1 = unsat_stor(icell, d1)
        f2 = unsat_stor(icell, d2)
        theta_at_depth = thtr(icell) + (f2 - f1) / (d2 - d1)
      else
        theta_at_depth = thts(icell)
      end if
    else
      theta_at_depth = thts(icell)
    end if
  end function get_water_content_at_depth


  function get_wcnew(icell) result(watercontent)
    ! -- dummy
    integer, intent(in) :: icell !< uzf cell containing depth
    ! -- return
    double precision :: watercontent
    ! -- local
    double precision :: top
    double precision :: bot
    double precision :: theta_r
    double precision :: thk
    double precision :: hgwf
    double precision :: fm
    double precision :: d
    !
    hgwf = watab(icell)
    top = celtop(icell)
    bot = celbot(icell)
    thk = top - max(bot, hgwf)
    theta_r = thtr(icell)
    if (thk > DZERO) then
      d = thk
      fm = unsat_stor(icell, d)
      watercontent = fm / thk
      watercontent = watercontent + theta_r
    else
      ! Saturated cell: no UZ column; report saturated content.
      watercontent = thts(icell)
    end if
  end function get_wcnew


end module UZFCELL
