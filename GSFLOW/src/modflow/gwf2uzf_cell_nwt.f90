!> @brief NWT driver for the cell unsaturated-zone solver.
!!
!! One profile is stored for each active UZF layer. Infiltration is
!! applied only to the land-surface cell, and the result is written to
!! the structured right-hand side and conductance.
module UZF_NWT_MAP
  integer, save :: nuzf = 0
  integer, save :: nwave = 0
  integer, save :: nuzf_built = 0
  integer, allocatable, save :: cell_ic(:)
  integer, allocatable, save :: cell_ir(:)
  integer, allocatable, save :: cell_k(:)
contains

  subroutine uzf_nwt_release_cells()
    use UZFCELL
    if (allocated(cell_ic)) deallocate(cell_ic, cell_ir, cell_k)
    if (allocated(thtr)) deallocate(thtr, thts, thti, eps)
    if (allocated(extwc)) deallocate(extwc, ha, hroot, rootact, etact)
    if (allocated(uzspst)) deallocate(uzspst, uzthst, uzflst, uzdpst)
    if (allocated(nwavst)) deallocate(nwavst, nwav, ntrail)
    if (allocated(totflux)) deallocate(totflux, sinf, finf)
    if (allocated(pet)) deallocate(pet, petmax, extdp, extdpuz)
    if (allocated(finf_rej)) deallocate(finf_rej, gwet)
    if (allocated(uzfarea)) deallocate(uzfarea, cellarea)
    if (allocated(celtop)) deallocate(celtop, celbot, landtop)
    if (allocated(watab)) deallocate(watab, watabold)
    if (allocated(vks)) deallocate(vks, seepk, surfdep)
    if (allocated(surflux)) deallocate(surflux, surfluxbelow, surfseep)
    if (allocated(gwpet)) deallocate(gwpet, landflag, ivertcon, wtbelow)
    if (allocated(pet_spec)) deallocate(pet_spec, extdp_spec, extwc_spec)
    if (allocated(ha_spec)) deallocate(ha_spec, hroot_spec, rootact_spec)
    if (allocated(wcnew)) deallocate(wcnew, wcold)
    if (allocated(rchrate)) deallocate(rchrate, gwdrate, rejrate)
    if (allocated(uzetrate)) deallocate(uzetrate, qstorate)
    if (allocated(uzth_iter)) deallocate(uzth_iter, uzdp_iter, uzfl_iter, uzsp_iter)
    if (allocated(uzth_et)) deallocate(uzth_et, uzdp_et, uzfl_et, uzsp_et)
    nuzf = 0
    nwave = 0
    nuzf_built = 0
  end subroutine uzf_nwt_release_cells

  subroutine uzf_nwt_allocate_cells(n, nw)
    use UZFCELL
    integer, intent(in) :: n, nw
    allocate(cell_ic(n), cell_ir(n), cell_k(n))
    allocate(thtr(n), thts(n), thti(n), eps(n))
    allocate(extwc(n), ha(n), hroot(n), rootact(n), etact(n))
    allocate(uzspst(nw, n), uzthst(nw, n))
    allocate(uzflst(nw, n), uzdpst(nw, n))
    allocate(nwavst(n), nwav(n), ntrail(n))
    allocate(totflux(n), sinf(n), finf(n))
    allocate(pet(n), petmax(n), extdp(n), extdpuz(n))
    allocate(finf_rej(n), gwet(n))
    allocate(uzfarea(n), cellarea(n))
    allocate(celtop(n), celbot(n), landtop(n))
    allocate(watab(n), watabold(n))
    allocate(vks(n), seepk(n), surfdep(n))
    allocate(surflux(n), surfluxbelow(n), surfseep(n))
    allocate(gwpet(n), landflag(n), ivertcon(n), wtbelow(n))
    allocate(pet_spec(n), extdp_spec(n), extwc_spec(n))
    allocate(ha_spec(n), hroot_spec(n), rootact_spec(n))
    allocate(wcnew(n), wcold(n))
    allocate(rchrate(n), gwdrate(n), rejrate(n))
    allocate(uzetrate(n), qstorate(n))
    allocate(uzth_iter(nw), uzdp_iter(nw), uzfl_iter(nw), uzsp_iter(nw))
    allocate(uzth_et(nw), uzdp_et(nw), uzfl_et(nw), uzsp_et(nw))
    thtr = 0.0d0
    thts = 0.0d0
    thti = 0.0d0
    eps = 0.0d0
    finf = 0.0d0
    totflux = 0.0d0
    wcnew = 0.0d0
    wcold = 0.0d0
    nwav = nw
    ivertcon = 0
    landflag = 0
    wtbelow = 1
  end subroutine uzf_nwt_allocate_cells

end module UZF_NWT_MAP


subroutine UZF_NWT_FREE()
  use UZF_NWT_MAP, only: uzf_nwt_release_cells
  call uzf_nwt_release_cells()
end subroutine UZF_NWT_FREE


subroutine UZF_NWT_BUILD()
  use UZF_NWT_MAP, only: nuzf, nwave, nuzf_built, cell_ic, cell_ir, cell_k, &
                         uzf_nwt_release_cells, uzf_nwt_allocate_cells
  use UZFCELL, only: setdata, sethead, setwaves, set_seepk, get_wcnew, &
                     ivertcon, nwav, wcnew, wcold, celbot
  use GWFUZFMODULE, only: IUZFBND, NSETS, NTRAIL, SURFDEP, Iseepreject, &
                          SURFK, VKS, EPS, THTS, THTR, THTI, LAYNUM
  use GLOBAL, only: NCOL, NROW, NLAY, IBOUND, HNEW, DELR, DELC, BOTM, IOUT
  use GWFBASMODULE, only: HDRY
  implicit none
  integer :: ic, ir, k, n, k1, k2, kland, ntr, prev
  double precision :: area, top, bot, vk, tr, ts, ti, ep, sd, h
  interface
    logical function uzf_nwt_active(ic, ir, k)
      integer, intent(in) :: ic, ir, k
    end function uzf_nwt_active
  end interface

  call uzf_nwt_release_cells()
  n = 0
  do ir = 1, NROW
    do ic = 1, NCOL
      if (IUZFBND(ic, ir) == 0) cycle
      call uzf_nwt_limits(ic, ir, k1, k2)
      do k = k1, k2
        if (uzf_nwt_active(ic, ir, k)) n = n + 1
      end do
    end do
  end do
  nuzf = n
  nuzf_built = 1
  if (nuzf == 0) return

  nwave = NTRAIL * (NSETS + 1)
  if (nwave < 2) nwave = 2
  ntr = NTRAIL
  if (ntr < 1) ntr = 1
  if (ntr > nwave - 1) ntr = nwave - 1
  call uzf_nwt_allocate_cells(nuzf, nwave)

  n = 0
  do ir = 1, NROW
    do ic = 1, NCOL
      if (IUZFBND(ic, ir) == 0) cycle
      call uzf_nwt_limits(ic, ir, k1, k2)
      prev = 0
      kland = 0
      do k = k1, k2
        if (.not. uzf_nwt_active(ic, ir, k)) cycle
        n = n + 1
        cell_ic(n) = ic
        cell_ir(n) = ir
        cell_k(n) = k
        if (prev > 0) ivertcon(prev) = n
        call uzf_nwt_props(ic, ir, k, vk, tr, ts, ti, ep)
        if (ti < tr) ti = tr
        if (ti > ts) ti = ts
        top = dble(BOTM(ic, ir, k - 1))
        bot = dble(BOTM(ic, ir, k))
        area = dble(DELR(ic)) * dble(DELC(ir))
        sd = 0.0d0
        if (prev == 0) then
          sd = dble(SURFDEP)
          if (sd > 0.9d0 * (top - bot)) sd = 0.9d0 * (top - bot)
          if (sd < 0.0d0) sd = 0.0d0
          kland = k
        end if
        call setdata(n, area, top, bot, sd, vk, tr, ts, ti, ep, ntr, &
                     merge(1, 0, prev == 0), 0)
        if (prev > 0) ivertcon(prev) = n
        if (prev == 0 .and. Iseepreject > 0 .and. associated(SURFK)) then
          if (SURFK(ic, ir) > 0.0) call set_seepk(n, dble(SURFK(ic, ir)))
        end if
        h = celbot(n)
        if (IBOUND(ic, ir, k) /= 0) then
          if (HNEW(ic, ir, k) /= dble(HDRY) .and. HNEW(ic, ir, k) < 1.0d29) &
            h = HNEW(ic, ir, k)
        end if
        call sethead(n, h)
        prev = n
      end do
      if (kland > 0) then
        call uzf_nwt_props(ic, ir, kland, vk, tr, ts, ti, ep)
        VKS(ic, ir) = real(vk)
        EPS(ic, ir) = real(ep)
        THTS(ic, ir) = real(ts)
        THTR(ic, ir) = real(tr)
        THTI(ic, ir) = real(ti)
        LAYNUM(ic, ir) = kland
      end if
    end do
  end do

  do n = 1, nuzf
    nwav(n) = nwave
    call setwaves(n)
    wcnew(n) = get_wcnew(n)
    wcold(n) = wcnew(n)
  end do
  write(IOUT, '(1X,A,I8,A)') 'UZF CELL PROFILES BUILT:', nuzf, &
    ' (ONE PER ACTIVE LAYER)'
end subroutine UZF_NWT_BUILD


subroutine UZF_NWT_STRESS()
  use UZF_NWT_MAP, only: nuzf, nuzf_built
  if (nuzf_built == 0 .or. nuzf == 0) return
  call uzf_nwt_apply_land()
end subroutine UZF_NWT_STRESS


subroutine GWF2UZF1AD(In, Kkper, Kkstp, Igrid)
  use UZF_NWT_MAP, only: nuzf, cell_ic, cell_ir, cell_k
  use UZFCELL, only: sethead, advance, watab, watabold, celbot, wtbelow, &
                     wcnew, wcold
  use GWFUZFMODULE, only: NUZTOP
  use GLOBAL, only: IBOUND, HNEW
  use GWFBASMODULE, only: HDRY
  implicit none
  integer, intent(in) :: In, Kkper, Kkstp, Igrid
  integer :: n, ic, ir, k
  double precision :: h

  call SGWF2UZF1PNT(Igrid)
  if (NUZTOP == 4 .and. Kkper + Kkstp > 2) call SETLAY(Kkstp)
  if (nuzf == 0) return
  do n = 1, nuzf
    ic = cell_ic(n)
    ir = cell_ir(n)
    k = cell_k(n)
    h = celbot(n)
    wtbelow(n) = 1
    if (IBOUND(ic, ir, k) > 0) then
      if (HNEW(ic, ir, k) /= dble(HDRY) .and. HNEW(ic, ir, k) < 1.0d29) then
        h = HNEW(ic, ir, k)
        if (h > celbot(n)) wtbelow(n) = 0
      end if
    end if
    call sethead(n, h)
    watabold(n) = watab(n)
    wcold(n) = wcnew(n)
    call advance(n)
  end do
end subroutine GWF2UZF1AD


subroutine GWF2UZF1FM(Kkper, Kkstp, Kkiter, Iunitsfr, Iunitlak, &
                      Iunitnwt, Iunitswr, Igrid)
  use UZF_NWT_MAP, only: nuzf
  use GWFUZFMODULE, only: IRUNFLG
  implicit none
  integer, intent(in) :: Kkper, Kkstp, Kkiter, Iunitsfr, Iunitlak
  integer, intent(in) :: Iunitnwt, Iunitswr, Igrid

  call SGWF2UZF1PNT(Igrid)
  if (nuzf == 0) return
  call uzf_nwt_apply_land()
  call uzf_nwt_solve(.true., Kkper)
  if (IRUNFLG > 0 .and. (Iunitsfr > 0 .or. Iunitlak > 0 .or. Iunitswr > 0)) &
    call SGWF2UZF1OLF(Iunitsfr, Iunitlak, Iunitswr, Igrid)
end subroutine GWF2UZF1FM


subroutine GWF2UZF1BD(Kkstp, Kkper, Iunitlak, Iunitsfr, Igrid)
  use UZF_NWT_MAP, only: nuzf, cell_ic, cell_ir, cell_k
  use UZFCELL, only: totflux, uzfarea, surfseep, cell_finf => finf, &
                     cell_rej => finf_rej, etact, cell_gwet => gwet, celtop, &
                     celbot, watab, watabold, wcold, cell_thtr => thtr, landflag, &
                     get_storage_change, get_wcnew, wcnew
  use GWFUZFMODULE, only: UZFLWT, SEEPOUT, REJ_INF, UZFETOUT, GWET, DELSTOR, &
                          UZSTOR, EXCESPP, UZTSRAT, CUMUZVOL, IUZFOPT, IUZFCB1
  use GLOBAL, only: NCOL, NROW, NLAY, IOUT, ISSFLG, IBOUND, HNEW, BUFF
  use GWFBASMODULE, only: ICBCFL, IBUDFL, DELT, MSUM, VBNM, VBVL
  implicit none
  integer, intent(in) :: Kkstp, Kkper, Iunitlak, Iunitsfr, Igrid
  integer :: n, ic, ir, k, iss, ibd
  double precision :: rch, etr, sto, infl, gwr, seep, rej, qsto, h, uzvol, thk, uzvolsum
  double precision, allocatable :: holdtab(:)
  double precision :: ratin, ratout, ratout2
  double precision :: vin, vout, ds, resid
  character(len=16) :: textrch, textet, textexfl
  data textrch/'    UZF RECHARGE'/
  data textet/'           GW ET'/
  data textexfl/' SURFACE LEAKAGE'/

  call SGWF2UZF1PNT(Igrid)
  if (nuzf == 0) return
  iss = 0
  if (ISSFLG(Kkper) /= 0) iss = 1
  UZFLWT = 0.0
  SEEPOUT = 0.0
  REJ_INF = 0.0
  UZFETOUT = 0.0
  GWET = 0.0
  DELSTOR = 0.0
  UZSTOR = 0.0
  EXCESPP = 0.0
  UZTSRAT = 0.0
  allocate(holdtab(nuzf))
  holdtab = watabold
  call uzf_nwt_apply_land()
  call uzf_nwt_solve(.false., Kkper)

  infl = 0.0d0
  etr = 0.0d0
  rch = 0.0d0
  sto = 0.0d0
  gwr = 0.0d0
  seep = 0.0d0
  rej = 0.0d0
  uzvolsum = 0.0d0
  do n = 1, nuzf
    ic = cell_ic(n)
    ir = cell_ir(n)
    k = cell_k(n)
    h = watab(n)
    qsto = get_storage_change(celtop(n), celbot(n), uzfarea(n), holdtab(n), &
                              h, wcold(n), get_wcnew(n), cell_thtr(n), dble(DELT), iss)
    wcnew(n) = get_wcnew(n)
    UZFLWT(ic, ir) = UZFLWT(ic, ir) + real(totflux(n) * uzfarea(n))
    SEEPOUT(ic, ir) = SEEPOUT(ic, ir) + real(surfseep(n))
    UZFETOUT(ic, ir) = UZFETOUT(ic, ir) + real(etact(n) * uzfarea(n) / DELT)
    GWET(ic, ir) = GWET(ic, ir) + real(cell_gwet(n))
    DELSTOR(ic, ir) = DELSTOR(ic, ir) + real(qsto * DELT)
    thk = celtop(n) - max(celbot(n), watab(n))
    uzvol = 0.0d0
    if (thk > 0.0d0) uzvol = thk * (get_wcnew(n) - cell_thtr(n)) * uzfarea(n)
    if (uzvol < 0.0d0) uzvol = 0.0d0
    UZSTOR(ic, ir) = UZSTOR(ic, ir) + real(uzvol)
    uzvolsum = uzvolsum + uzvol
    if (landflag(n) == 1) then
      infl = infl + (cell_finf(n) - cell_rej(n)) * uzfarea(n)
      rej = rej + cell_rej(n) * uzfarea(n)
      REJ_INF(ic, ir) = REJ_INF(ic, ir) + real(cell_rej(n) * uzfarea(n))
    end if
    rch = rch + totflux(n) * uzfarea(n) / DELT
    etr = etr + etact(n) * uzfarea(n) / DELT
    sto = sto + qsto
    gwr = gwr + cell_gwet(n)
    seep = seep + surfseep(n)
  end do

  UZTSRAT(1) = real(infl)
  UZTSRAT(2) = real(etr)
  UZTSRAT(3) = real(rch)
  UZTSRAT(4) = real(sto)
  UZTSRAT(5) = real(seep)
  UZTSRAT(6) = real(uzvolsum / DELT)
  UZTSRAT(7) = real(gwr)
  CUMUZVOL(1) = CUMUZVOL(1) + real(infl * DELT)
  CUMUZVOL(2) = CUMUZVOL(2) + real(etr * DELT)
  CUMUZVOL(3) = CUMUZVOL(3) + real(rch * DELT)
  CUMUZVOL(4) = CUMUZVOL(4) + real(sto * DELT)
  CUMUZVOL(5) = CUMUZVOL(5) + real(seep * DELT)

  ratin = rch
  ratout = gwr
  ratout2 = seep
  VBVL(4, MSUM) = 0.0
  VBVL(3, MSUM) = real(ratin)
  VBVL(2, MSUM) = VBVL(2, MSUM)
  VBVL(1, MSUM) = VBVL(1, MSUM) + real(ratin * DELT)
  VBNM(MSUM) = textrch
  MSUM = MSUM + 1
  VBVL(4, MSUM) = real(ratout)
  VBVL(3, MSUM) = 0.0
  VBVL(2, MSUM) = VBVL(2, MSUM) + real(ratout * DELT)
  VBVL(1, MSUM) = VBVL(1, MSUM)
  VBNM(MSUM) = textet
  MSUM = MSUM + 1
  VBVL(4, MSUM) = real(ratout2)
  VBVL(3, MSUM) = 0.0
  VBVL(2, MSUM) = VBVL(2, MSUM) + real(ratout2 * DELT)
  VBVL(1, MSUM) = VBVL(1, MSUM)
  VBNM(MSUM) = textexfl
  MSUM = MSUM + 1

  if (IBUDFL > 0 .and. IUZFOPT > 0) then
    vin = infl * DELT
    vout = etr * DELT + rch * DELT
    ds = sto * DELT
    resid = vin - vout - ds
    write(IOUT, '(/1X,A,I6,A,I6)') 'UZF CELL BUDGET  TIME STEP', Kkstp, &
      '  STRESS PERIOD', Kkper
    write(IOUT, '(1X,A,1PE14.6,A,1PE14.6)') '    INFILTRATION', infl, &
      '   VOLUME', vin
    write(IOUT, '(1X,A,1PE14.6,A,1PE14.6)') '          UZF ET', etr, &
      '   VOLUME', etr * DELT
    write(IOUT, '(1X,A,1PE14.6,A,1PE14.6)') '    UZF RECHARGE', rch, &
      '   VOLUME', rch * DELT
    write(IOUT, '(1X,A,1PE14.6,A,1PE14.6)') '  STORAGE CHANGE', sto, &
      '   VOLUME', ds
    write(IOUT, '(1X,A,1PE14.6)') ' UZ BALANCE (IN-ET-RECHARGE-STORAGE)', resid
    write(IOUT, '(1X,A,1PE14.6,A,1PE14.6)') ' SURFACE LEAKAGE', seep, &
      '   GW ET', gwr
  end if

  ibd = 0
  if (IUZFCB1 /= 0) ibd = ICBCFL
  if (ibd > 0) then
    BUFF = 0.0
    do n = 1, nuzf
      ic = cell_ic(n)
      ir = cell_ir(n)
      k = cell_k(n)
      if (IBOUND(ic, ir, k) == 0) cycle
      BUFF(ic, ir, k) = real(totflux(n) * uzfarea(n) / DELT)
    end do
    call UBUDSV(Kkstp, Kkper, textrch, abs(IUZFCB1), BUFF, NCOL, NROW, &
                NLAY, IOUT)
    BUFF = 0.0
    do n = 1, nuzf
      if (landflag(n) /= 1) cycle
      ic = cell_ic(n)
      ir = cell_ir(n)
      k = cell_k(n)
      BUFF(ic, ir, k) = SEEPOUT(ic, ir)
    end do
    call UBUDSV(Kkstp, Kkper, textexfl, abs(IUZFCB1), BUFF, NCOL, NROW, &
                NLAY, IOUT)
  end if
end subroutine GWF2UZF1BD


subroutine uzf_nwt_solve(reset_state, Kkper)
  use UZF_NWT_MAP, only: nuzf, cell_ic, cell_ir, cell_k
  use UZFCELL, only: solve, setgwpet, simgwet, setbelowpet, celbot, ivertcon
  use GWFUZFMODULE, only: IETFLG, Iseepsupress
  use GLOBAL, only: IBOUND, HNEW, RHS, HCOF, ISSFLG, IOUT
  use GWFBASMODULE, only: HDRY
  implicit none
  logical, intent(in) :: reset_state
  integer, intent(in) :: Kkper
  integer :: n, ic, ir, k, ierr, iss, iet, iseep, jbelow
  double precision :: h, tot, trhs1, thcof1, deriv1, trhs2, thcof2, deriv2, wc
  double precision :: rhsadd, hcofadd

  iss = 0
  if (ISSFLG(Kkper) /= 0) iss = 1
  iet = 0
  if (IETFLG > 0) iet = 1
  iseep = 1
  if (Iseepsupress > 0) iseep = 0
  ierr = 0
  do n = 1, nuzf
    ic = cell_ic(n)
    ir = cell_ir(n)
    k = cell_k(n)
    h = celbot(n)
    if (IBOUND(ic, ir, k) /= 0) then
      if (HNEW(ic, ir, k) /= dble(HDRY) .and. HNEW(ic, ir, k) < 1.0d29) &
        h = HNEW(ic, ir, k)
    end if
    jbelow = ivertcon(n)
    tot = 0.0d0
    trhs1 = 0.0d0
    thcof1 = 0.0d0
    deriv1 = 0.0d0
    trhs2 = 0.0d0
    thcof2 = 0.0d0
    deriv2 = 0.0d0
    wc = 0.0d0
    call solve(jbelow, n, tot, iet, iss, iseep, h, 0.0d0, ierr, &
               reset_state, trhs1, thcof1, deriv1, wc)
    if (ierr > 0) then
      write(IOUT, '(1X,A,3I6)') 'UZF CELL SOLVER RAN OUT OF WAVES AT', ic, ir, k
      call USTOP('UZF CELL SOLVER RAN OUT OF WAVES')
    end if
    if (iet > 0) then
      call setgwpet(n)
      call simgwet(1, n, h, trhs2, thcof2, deriv2)
      if (jbelow > 0) call setbelowpet(n, jbelow)
    end if
    if (reset_state .and. IBOUND(ic, ir, k) > 0) then
      rhsadd = (-trhs1 + trhs2) + (deriv1 + deriv2) * HNEW(ic, ir, k)
      hcofadd = (thcof1 + thcof2) + (deriv1 + deriv2)
      RHS(ic, ir, k) = RHS(ic, ir, k) + rhsadd
      HCOF(ic, ir, k) = HCOF(ic, ir, k) + hcofadd
    end if
  end do
end subroutine uzf_nwt_solve


subroutine uzf_nwt_apply_land()
  use UZF_NWT_MAP, only: nuzf, cell_ic, cell_ir, cell_k
  use UZFCELL, only: landflag, ivertcon, setdatafinf, setdataet, setdataetwc
  use GWFUZFMODULE
  use GLOBAL, only: IUNIT, DELR, DELC
  use GWFAGMODULE, only: DIVERSIONIRRUZF, NUMIRRDIVERSION, WELLIRRUZF, NUMIRRWEL
  implicit none
  integer :: n, ic, ir, jbelow
  real :: hold, cap, pet, depth, wilt

  do n = 1, nuzf
    if (landflag(n) /= 1) cycle
    ic = cell_ic(n)
    ir = cell_ir(n)
    hold = FINF(ic, ir)
    if (ISAVEFINF + IGSFLOW == 2) then
      if (associated(FINFSAVE)) hold = hold + FINFSAVE(ic, ir)
    end if
    if (IUNIT(44) > 0 .and. IUNIT(66) > 0) then
      if (NUMIRRDIVERSION > 0) hold = hold + DIVERSIONIRRUZF(ic, ir)
    end if
    if (IUNIT(66) > 0) then
      if (NUMIRRWEL > 0) hold = hold + WELLIRRUZF(ic, ir)
    end if
    cap = VKS(ic, ir)
    if (Isurfkreject > 0) then
      if (associated(SURFK)) cap = SURFK(ic, ir)
    end if
    EXCESPP(ic, ir) = 0.0
    if (.not. (IGSFLOW > 0 .and. ISAVEFINF == 0)) then
      if (hold > cap) then
        EXCESPP(ic, ir) = (hold - cap) * DELR(ic) * DELC(ir)
        hold = cap
      end if
    end if
    if (hold < 0.0) hold = 0.0
    call setdatafinf(n, dble(hold))
  end do

  if (IETFLG <= 0) return
  do n = 1, nuzf
    ic = cell_ic(n)
    ir = cell_ir(n)
    jbelow = ivertcon(n)
    pet = 0.0
    depth = 0.0
    wilt = 0.0
    if (associated(PETRATE)) pet = PETRATE(ic, ir)
    if (associated(ROOTDPTH)) depth = ROOTDPTH(ic, ir)
    if (associated(WCWILT)) wilt = WCWILT(ic, ir)
    call setdataet(n, jbelow, dble(pet), dble(depth))
    call setdataetwc(n, jbelow, dble(wilt))
  end do
end subroutine uzf_nwt_apply_land


subroutine uzf_nwt_limits(ic, ir, k1, k2)
  use GWFUZFMODULE, only: NUZTOP, IUZFBND
  use GLOBAL, only: NLAY
  implicit none
  integer, intent(in) :: ic, ir
  integer, intent(out) :: k1, k2
  k1 = 1
  k2 = NLAY
  if (NUZTOP == 1) then
    k2 = 1
  else if (NUZTOP == 2) then
    k1 = abs(IUZFBND(ic, ir))
    if (k1 < 1) k1 = 1
    if (k1 > NLAY) k1 = NLAY
  end if
end subroutine uzf_nwt_limits


logical function uzf_nwt_active(ic, ir, k)
  use GWFUZFMODULE, only: IUZFBND
  use GLOBAL, only: IBOUND, BOTM
  implicit none
  integer, intent(in) :: ic, ir, k
  double precision :: vk, tr, ts, ti, ep, top, bot
  uzf_nwt_active = .false.
  if (IUZFBND(ic, ir) == 0) return
  if (IBOUND(ic, ir, k) == 0) return
  top = dble(BOTM(ic, ir, k - 1))
  bot = dble(BOTM(ic, ir, k))
  if (top - bot <= 1.0d-4) return
  call uzf_nwt_props(ic, ir, k, vk, tr, ts, ti, ep)
  if (ts <= tr) return
  if (vk <= 0.0d0) return
  uzf_nwt_active = .true.
end function uzf_nwt_active


subroutine uzf_nwt_props(ic, ir, k, vk, tr, ts, ti, ep)
  use GWFUZFMODULE
  implicit none
  integer, intent(in) :: ic, ir, k
  double precision, intent(out) :: vk, tr, ts, ti, ep
  vk = dble(VKS(ic, ir))
  tr = dble(THTR(ic, ir))
  ts = dble(THTS(ic, ir))
  ti = dble(THTI(ic, ir))
  ep = dble(EPS(ic, ir))
  if (allocated(VKSLAY)) vk = dble(VKSLAY(ic, ir, k))
  if (allocated(THTRLAY)) tr = dble(THTRLAY(ic, ir, k))
  if (allocated(THTSLAY)) ts = dble(THTSLAY(ic, ir, k))
  if (allocated(THTILAY)) ti = dble(THTILAY(ic, ir, k))
  if (allocated(EPSLAY)) ep = dble(EPSLAY(ic, ir, k))
end subroutine uzf_nwt_props
