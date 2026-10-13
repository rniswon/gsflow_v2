module uzf_bycell_nwt
! Per-layer kinematic routing for the structured UZF package.
  implicit none
  public :: uzflow_bycell, set_rechlay, sync_column_props
contains
subroutine uzflow_bycell(l, ic, ir, ktop, kwt, celtop, hwtab, surfin, &
                          deltinc, idelt, thr, ths, fks, epsilon, &
                          rateud, rootdp, wiltwc, ipersist, &
                          totflux, etact, fmstor, kper)
! Route one kinematic profile per model layer. The flux that drains out
! the bottom of a layer is infiltration into the layer below. Recharge
! to the starting water-table layer is the flux that leaves that layer.
! Storage swallowed where the water table has risen into a shallower
! layer is recharge to that shallower layer, kept in RECHABS.
  use gwfuzfmodule
  use global, only: nlay, botm, hnew, ibound
  implicit none
  integer, intent(in) :: l, ic, ir, ktop, kwt, idelt, ipersist, kper
  integer :: k, j, ik, numwaves
  real, intent(in) :: ths, fks, epsilon, wiltwc, rootdp
  real :: thsk, fksk, epsk
  double precision, intent(in) :: celtop, hwtab, surfin, deltinc, thr, rateud
  double precision :: thrk
  double precision, intent(out) :: totflux, etact, fmstor
  double precision :: ztop, zbot, hk, dlength, zoldist, tflx, eact
  double precision :: surflux, oldsflx, ru, absorb, down
  double precision, allocatable :: dlen(:), zold(:), osflx(:)
  real :: rd, ww
  interface
    subroutine uzflow2(i, surflux, dlength, zoldist, depth, theta, &
                       flux, speed, itrwave, ltrail, totalflux, &
                       numwaves, thetar, thetas, fksat, eps, oldsflx, &
                       jpnt, rateud, etout, wiltwc, rootdepth, delt, &
                       ic, ir, land, h, celtop, mtflg, kper)
      integer i, numwaves, jpnt, ic, ir, land, mtflg, kper
      integer itrwave(*), ltrail(*)
      real thetas, fksat, eps, wiltwc, rootdepth
      double precision surflux, dlength, zoldist, totalflux, thetar
      double precision oldsflx, rateud, etout, delt, h, celtop
      double precision depth(*), theta(*), flux(*), speed(*)
    end subroutine uzflow2
  end interface

  allocate(dlen(nlay), zold(nlay), osflx(nlay))
  totflux = 0.0d0
  etact = 0.0d0
  fmstor = 0.0d0
  colstor0 = 0.0d0
  rechabsum = 0.0d0
  if (allocated(rechabs)) rechabs = 0.0d0
  if (ktop < 1 .or. kwt < ktop) return
  if (.not. allocated(uzthlay)) return

  if (layready(l) == 0) then
    call init_uzf_layers(l, ic, ir, ktop, thr)
    layready(l) = 1
  end if

  do k = ktop, kwt
    numwaves = nwavlay(k, l)
    if (numwaves < 1) numwaves = 1
    wnwav(k) = numwaves
    do j = 1, nwav
      wdp(j, k) = uzdplay(j, k, l)
      wth(j, k) = uzthlay(j, k, l)
      wfl(j, k) = uzfllay(j, k, l)
      wsp(j, k) = uzsplay(j, k, l)
      witr(j, k) = itrlay(j, k, l)
      wltr(j, k) = ltrlay(j, k, l)
    end do
    ztop = layer_top(ic, ir, k, ktop, celtop)
    zbot = botm(ic, ir, k)
    if (k < kwt) then
      dlen(k) = ztop - zbot
      ! Head above this bottom means the water table has risen into the
      ! layer. Shorten the profile so that storage is absorbed here.
      if (ibound(ic, ir, k) > 0) then
        hk = hnew(ic, ir, k)
        if (hk > zbot) dlen(k) = ztop - hk
        if (dlen(k) > ztop - zbot) dlen(k) = ztop - zbot
      end if
    else
      dlen(k) = ztop - hwtab
      ! A head below this cell is not a water table in the cell.
      if (dlen(k) > ztop - zbot) dlen(k) = ztop - zbot
    end if
    if (dlen(k) < 0.0d0) dlen(k) = 0.0d0
    zold(k) = wdp(1, k)
    if (zold(k) < 0.0d0) zold(k) = 0.0d0
    osflx(k) = uzolslay(k, l)
  end do

  ! Starting storage is the sum of every unsaturated layer, not the
  ! single profile saved for the uppermost cell.
  colstor0 = 0.0d0
  do k = ktop, kwt
    colstor0 = colstor0 + layer_storage(wdp(:, k), wth(:, k), &
                                        wltr(:, k), witr(:, k), &
                                        wnwav(k), layer_thr(ic, ir, k, thr))
  end do

  do ik = 1, idelt
    surflux = surfin
    do k = ktop, kwt
      dlength = dlen(k)
      zoldist = zold(k)
      oldsflx = osflx(k)
      numwaves = wnwav(k)
      ztop = layer_top(ic, ir, k, ktop, celtop)
      if (k < kwt) then
        hk = ztop - dlen(k)
      else
        hk = hwtab
      end if
      if (k == ktop) then
        ru = rateud
        rd = rootdp
        ww = wiltwc
      else
        ru = 0.0d0
        rd = 0.0
        ww = 0.0
      end if
      tflx = 0.0d0
      eact = 0.0d0
      call layer_props(ic, ir, k, thr, ths, fks, epsilon, &
                       thrk, thsk, fksk, epsk)
      call uzflow2(l, surflux, dlength, zoldist, wdp(:, k), wth(:, k), &
                   wfl(:, k), wsp(:, k), witr(:, k), wltr(:, k), &
                   tflx, numwaves, thrk, thsk, fksk, epsk, &
                   oldsflx, 1, ru, eact, ww, rd, deltinc, &
                   ic, ir, ktop, hk, ztop, ipersist, kper)
      wnwav(k) = numwaves
      dlen(k) = dlength
      zold(k) = dlength
      osflx(k) = surflux
      etact = etact + eact
      if (k < kwt) then
        absorb = fmrising
        if (absorb < 0.0d0) absorb = 0.0d0
        if (absorb > tflx) absorb = tflx
        if (allocated(rechabs)) rechabs(k) = rechabs(k) + absorb
        rechabsum = rechabsum + absorb
        down = tflx - absorb
        if (down < 0.0d0) down = 0.0d0
        if (deltinc > 0.0d0) then
          surflux = down / deltinc
        else
          surflux = 0.0d0
        end if
      else
        totflux = totflux + tflx
      end if
    end do
  end do

  do k = ktop, kwt
    fmstor = fmstor + layer_storage(wdp(:, k), wth(:, k), wltr(:, k), &
                                    witr(:, k), wnwav(k), &
                                    layer_thr(ic, ir, k, thr))
  end do

  if (ipersist == 1) then
    do k = ktop, kwt
      nwavlay(k, l) = wnwav(k)
      uzolslay(k, l) = osflx(k)
      do j = 1, nwav
        uzdplay(j, k, l) = wdp(j, k)
        uzthlay(j, k, l) = wth(j, k)
        uzfllay(j, k, l) = wfl(j, k)
        uzsplay(j, k, l) = wsp(j, k)
        itrlay(j, k, l) = witr(j, k)
        ltrlay(j, k, l) = wltr(j, k)
      end do
    end do
    do k = kwt + 1, nlay
      nwavlay(k, l) = 1
      uzdplay(1, k, l) = 0.0d0
      uzthlay(1, k, l) = layer_thr(ic, ir, k, thr)
      uzfllay(1, k, l) = 0.0d0
    end do
    nwavst(ic, ir) = wnwav(kwt)
    do j = 1, nwav
      uzdpst(j, l) = wdp(j, kwt)
      uzthst(j, l) = wth(j, kwt)
      uzflst(j, l) = wfl(j, kwt)
      uzspst(j, l) = wsp(j, kwt)
      itrlst(j, l) = witr(j, kwt)
      ltrlst(j, l) = wltr(j, kwt)
    end do
  end if
end subroutine uzflow_bycell

subroutine init_uzf_layers(l, ic, ir, ktop, thr)
  use gwfuzfmodule
  use global, only: nlay, hnew, botm
  implicit none
  integer, intent(in) :: l, ic, ir, ktop
  double precision, intent(in) :: thr
  integer :: k, j
  double precision :: ztop, zbot, hk, dlength
  do k = 1, nlay
    nwavlay(k, l) = 1
    do j = 1, nwav
      uzthlay(j, k, l) = thr
      uzdplay(j, k, l) = 0.0d0
      uzsplay(j, k, l) = 0.0d0
      uzfllay(j, k, l) = 0.0d0
      itrlay(j, k, l) = 0
      ltrlay(j, k, l) = 0
    end do
    if (k == ktop) then
      ztop = botm(ic, ir, k - 1) - 0.5d0 * surfdep
    else
      ztop = botm(ic, ir, k - 1)
    end if
    zbot = botm(ic, ir, k)
    hk = hnew(ic, ir, k)
    if (hk >= ztop) then
      dlength = 0.0d0
    else if (hk <= zbot) then
      dlength = ztop - zbot
    else
      dlength = ztop - hk
    end if
    if (dlength < 0.0d0) dlength = 0.0d0
    uzdplay(1, k, l) = dlength
    call layer_initial(ic, ir, k, l, uzthst(1, l), uzflst(1, l))
    uzolslay(k, l) = uzfllay(1, k, l)
  end do
end subroutine init_uzf_layers

function layer_thr(ic, ir, k, thr) result(thrk)
  use gwfuzfmodule, only: thtrlay
  implicit none
  integer, intent(in) :: ic, ir, k
  double precision, intent(in) :: thr
  double precision :: thrk
  if (allocated(thtrlay)) then
    thrk = thtrlay(ic, ir, k)
  else
    thrk = thr
  end if
end function layer_thr

subroutine layer_props(ic, ir, k, thr, ths, fks, epsilon, &
                       thrk, thsk, fksk, epsk)
  use gwfuzfmodule, only: thtrlay, thtslay, vkslay, epslay
  implicit none
  integer, intent(in) :: ic, ir, k
  real, intent(in) :: ths, fks, epsilon
  double precision, intent(in) :: thr
  double precision, intent(out) :: thrk
  real, intent(out) :: thsk, fksk, epsk
  thrk = layer_thr(ic, ir, k, thr)
  if (allocated(thtslay)) then
    thsk = thtslay(ic, ir, k)
    fksk = vkslay(ic, ir, k)
    epsk = epslay(ic, ir, k)
  else
    thsk = ths
    fksk = fks
    epsk = epsilon
  end if
end subroutine layer_props

subroutine layer_initial(ic, ir, k, l, thcol, flcol)
! Start this layer at its own THTI. The Brooks-Corey flux uses this
! layer's vertical conductivity, residual, and saturated content.
  use gwfuzfmodule
  implicit none
  integer, intent(in) :: ic, ir, k, l
  double precision, intent(in) :: thcol, flcol
  double precision :: thrk, thik, top, bot
  if (allocated(thtilay) .and. thtilay(ic, ir, k) > 0.0) then
    thrk = thtrlay(ic, ir, k)
    thik = thtilay(ic, ir, k)
    if (thik < thrk) thik = thrk
    uzthlay(1, k, l) = thik
    top = thik - thrk
    bot = thtslay(ic, ir, k) - thrk
    if (top > 0.0d0 .and. bot > 0.0d0 .and. vkslay(ic, ir, k) > 0.0 &
        .and. epslay(ic, ir, k) > 0.0) then
      uzfllay(1, k, l) = vkslay(ic, ir, k) * (top / bot)**epslay(ic, ir, k)
    else
      uzfllay(1, k, l) = 0.0d0
    end if
  else
    uzthlay(1, k, l) = thcol
    uzfllay(1, k, l) = flcol
  end if
end subroutine layer_initial

subroutine sync_column_props()
! Copy the land-layer value into the 2-D arrays used by the
! single-profile path. By-cell routing reads the layer arrays.
  use gwfuzfmodule
  use global, only: ncol, nrow, nlay, ibound
  implicit none
  integer :: ic, ir, il, ill
  if (.not. allocated(thtslay)) return
  do ir = 1, nrow
    do ic = 1, ncol
      il = 1
      if (nuztop == 1 .or. nuztop == 2 .or. nuztop == 5) then
        il = abs(iuzfbnd(ic, ir))
      else if (nuztop == 3) then
        ill = 1
        do while (ill <= nlay)
          if (ibound(ic, ir, ill) > 0) then
            il = ill
            exit
          end if
          ill = ill + 1
        end do
      else if (nuztop == 4) then
        il = laynum(ic, ir)
      end if
      if (il < 1) il = 1
      if (il > nlay) il = nlay
      if (allocated(vkslay) .and. iuzfopt == 1) &
        vks(ic, ir) = vkslay(ic, ir, il)
      eps(ic, ir) = epslay(ic, ir, il)
      thts(ic, ir) = thtslay(ic, ir, il)
      thti(ic, ir) = thtilay(ic, ir, il)
      thtr(ic, ir) = thtrlay(ic, ir, il)
    end do
  end do
end subroutine sync_column_props

function layer_top(ic, ir, k, ktop, celtop) result(ztop)
  use global, only: botm
  implicit none
  integer, intent(in) :: ic, ir, k, ktop
  double precision, intent(in) :: celtop
  double precision :: ztop
  if (k == ktop) then
    ztop = celtop
  else
    ztop = botm(ic, ir, k - 1)
  end if
end function layer_top

function layer_storage(depth, theta, ltrail, itrwave, nwave, thr) result(fm)
! Water content above residual, integrated over the layer profile.
  implicit none
  integer, intent(in) :: nwave
  integer, intent(in) :: ltrail(:), itrwave(:)
  double precision, intent(in) :: depth(:), theta(:), thr
  double precision :: fm
  integer :: j, k
  fm = 0.0d0
  if (nwave < 1) return
  j = 1
  do while (j <= nwave - 1)
    if (ltrail(j) == 1 .and. itrwave(j + 1) > 0) then
      k = j
      do while (k < j + itrwave(j + 1) .and. k < nwave)
        fm = fm + (theta(k) - thr) * (depth(k) - depth(k + 1))
        k = k + 1
      end do
      if (k == nwave) then
        fm = fm + (theta(k) - thr) * depth(k)
      else if (k < nwave) then
        fm = fm + (theta(k) - thr) * (depth(k) - depth(k + 1))
      end if
      j = k
    else
      fm = fm + (theta(j) - thr) * (depth(j) - depth(j + 1))
    end if
    j = j + 1
  end do
  if (j == nwave) fm = fm + (theta(nwave) - thr) * depth(nwave)
  if (fm < 0.0d0) fm = 0.0d0
end function layer_storage

subroutine set_rechlay()
! Record the layer that contains the water table from the heads at the
! start of the time step. Formulation and budget keep recharge on this
! layer until the next time step.
  use gwfuzfmodule
  use global, only: nlay, ibound, hnew, botm
  implicit none
  integer :: ll, ic, ir, il, ill, kwt, ibnd, nlayp1
  double precision :: hwtab
  if (.not. allocated(kwthold)) return
  kwthold = 0
  nlayp1 = nlay + 1
  do ll = 1, numcells
    ir = iuzhold(1, ll)
    ic = iuzhold(2, ll)
    ibnd = iuzfbnd(ic, ir)
    il = 0
    if (nuztop == 1 .or. nuztop == 2) then
      il = abs(ibnd)
      if (il > 0) then
        if (ibound(ic, ir, il) < 1) il = 0
      end if
    else if (nuztop == 3 .or. nuztop == 5) then
      ill = 1
      do while (ill < nlayp1)
        if (ibound(ic, ir, ill) > 0) then
          il = ill
          exit
        else if (ibound(ic, ir, ill) < 0) then
          exit
        end if
        ill = ill + 1
      end do
    else if (nuztop == 4) then
      il = laynum(ic, ir)
    end if
    if (ibnd == 0) il = 0
    if (nuztop == 5) il = abs(iuzfbnd(ic, ir))
    if (il < 1) cycle
    kwt = il
    if (ibound(ic, ir, il) > 0) then
      hwtab = hnew(ic, ir, il)
    else if (il < nlay) then
      hwtab = hnew(ic, ir, il + 1)
    else
      hwtab = botm(ic, ir, nlay)
    end if
    do while (kwt < nlay)
      if (hwtab > botm(ic, ir, kwt)) exit
      if (ibound(ic, ir, kwt + 1) < 1) exit
      kwt = kwt + 1
      if (ibound(ic, ir, kwt) > 0) then
        hwtab = hnew(ic, ir, kwt)
      else
        hwtab = botm(ic, ir, kwt)
      end if
    end do
    kwthold(ic, ir) = kwt
  end do
end subroutine set_rechlay
end module uzf_bycell_nwt
