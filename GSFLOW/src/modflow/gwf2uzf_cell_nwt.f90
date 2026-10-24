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
    if (allocated(soilheadd)) deallocate(soilheadd)
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
    allocate(soilheadd(n))
    soilheadd = 0.0d0
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
                          SURFK, VKS, EPS, THTS, THTR, THTI, LAYNUM, IWCRD
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

  if (IWCRD > 0) call uzf_nwt_read_wcavg()
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
                     celbot, watab, watabold, wcold, landflag, &
                     cell_unsat_storage, get_storage_change, get_wcnew, wcnew
  use GWFUZFMODULE, only: UZFLWT, SEEPOUT, REJ_INF, UZFETOUT, GWET, DELSTOR, &
                          UZSTOR, EXCESPP, UZTSRAT, CUMUZVOL, IUZFOPT, IUZFCB1
  use GLOBAL, only: NCOL, NROW, NLAY, IOUT, ISSFLG, IBOUND, HNEW, BUFF
  use GWFBASMODULE, only: ICBCFL, IBUDFL, DELT, MSUM, VBNM, VBVL
  implicit none
  integer, intent(in) :: Kkstp, Kkper, Iunitlak, Iunitsfr, Igrid
  integer :: n, ic, ir, k, iss, ibd
  double precision :: rch, etr, sto, infl, gwr, seep, rej, qsto, uzvol, uzvolsum
  double precision :: storold, stornew, deltd
  double precision, allocatable :: stor0(:)
  double precision :: ratin, ratout, ratout2
  character(len=16) :: textrch, textet, textexfl
  data textrch/'    UZF RECHARGE'/
  data textet/'           GW ET'/
  data textexfl/' SURFACE LEAKAGE'/

  call SGWF2UZF1PNT(Igrid)
  if (nuzf == 0) return
  iss = 0
  if (ISSFLG(Kkper) /= 0) iss = 1
  deltd = dble(DELT)
  UZFLWT = 0.0
  SEEPOUT = 0.0
  REJ_INF = 0.0
  UZFETOUT = 0.0
  GWET = 0.0
  DELSTOR = 0.0
  UZSTOR = 0.0
  EXCESPP = 0.0
  UZTSRAT = 0.0
  ! Snapshot start-of-step wave storage before routing (same role as COLSTOR0).
  allocate(stor0(nuzf))
  do n = 1, nuzf
    stor0(n) = cell_unsat_storage(n, watabold(n))
  end do
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
    storold = stor0(n)
    stornew = cell_unsat_storage(n, watab(n))
    qsto = get_storage_change(uzfarea(n), storold, stornew, deltd, iss)
    wcnew(n) = get_wcnew(n)
    wcold(n) = wcnew(n)
    UZFLWT(ic, ir) = UZFLWT(ic, ir) + real(totflux(n) * uzfarea(n))
    SEEPOUT(ic, ir) = SEEPOUT(ic, ir) + real(surfseep(n))
    UZFETOUT(ic, ir) = UZFETOUT(ic, ir) + real(etact(n) * uzfarea(n) / DELT)
    GWET(ic, ir) = GWET(ic, ir) + real(cell_gwet(n))
    DELSTOR(ic, ir) = DELSTOR(ic, ir) + real(qsto * DELT)
    uzvol = stornew * uzfarea(n)
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
  deallocate(stor0)

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

  call uzf_nwt_write_budget(IOUT, IBUDFL, IUZFOPT, Kkstp, Kkper, &
                            CUMUZVOL(1), CUMUZVOL(2), CUMUZVOL(3), &
                            CUMUZVOL(4), UZTSRAT(1), UZTSRAT(2), &
                            UZTSRAT(3), UZTSRAT(4))

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
  call uzf_nwt_save_wcavg(Kkstp, Kkper)
end subroutine GWF2UZF1BD


subroutine uzf_nwt_solve(reset_state, Kkper)
  use UZF_NWT_MAP, only: nuzf, cell_ic, cell_ir, cell_k
  use UZFCELL, only: solve, setgwpet, simgwet, setbelowpet, celbot, ivertcon, &
                     surfseep, landflag
  use GWFUZFMODULE, only: IETFLG, Iseepsupress, SEEPOUT
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
    if (reset_state .and. landflag(n) == 1) SEEPOUT(ic, ir) = real(surfseep(n))
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


subroutine uzf_nwt_save_wcavg(kstp, kper)
! Write one volume-averaged water content per cell when the output
! control file saves heads. The average is taken above the water table.
! A fully saturated cell is stored as the saturated water content.
  use UZF_NWT_MAP, only: nuzf, cell_ic, cell_ir, cell_k
  use UZFCELL, only: celtop, celbot, watab, thts, get_wcnew
  use GWFUZFMODULE, only: IWCUN
  use GLOBAL, only: IOUT, NCOL, NROW, NLAY
  use GWFBASMODULE, only: IHEDUN, IOFLG, PERTIM, TOTIM
  implicit none
  integer, intent(in) :: kstp, kper
  integer :: n, ic, ir, k, layer
  real, allocatable :: wc(:, :, :)
  real, allocatable :: blay(:, :)
  double precision :: thk
  character(len=16) :: text
  logical :: save_any
  if (IWCUN <= 0 .or. IHEDUN <= 0) return
  if (.not. associated(IOFLG)) return
  save_any = .false.
  do k = 1, NLAY
    if (IOFLG(k, 3) /= 0) save_any = .true.
  end do
  if (.not. save_any) return
  allocate(wc(NCOL, NROW, NLAY))
  wc = 0.0
  do n = 1, nuzf
    ic = cell_ic(n)
    ir = cell_ir(n)
    k = cell_k(n)
    thk = celtop(n) - max(celbot(n), watab(n))
    if (thk > 0.0d0) then
      wc(ic, ir, k) = real(get_wcnew(n))
    else
      wc(ic, ir, k) = real(thts(n))
    end if
  end do
  text = 'UZF CELLAVG WC  '
  allocate(blay(NCOL, NROW))
  write(IOUT, '(1X,A,I5,A,I8,A,I8)') &
    'UZF CELL-AVERAGE WATER CONTENT SAVED ON UNIT ', IWCUN, &
    ' AT END OF TIME STEP ', kstp, ', STRESS PERIOD ', kper
  do layer = 1, NLAY
    if (IOFLG(layer, 3) == 0) cycle
    blay = wc(:, :, layer)
    call ULASAV(blay, text, kstp, kper, PERTIM, TOTIM, NCOL, NROW, &
                layer, IWCUN)
  end do
  flush(IWCUN)
end subroutine uzf_nwt_save_wcavg


subroutine uzf_nwt_read_wcavg()
! Read the last saved cell-average water content and store it as the
! initial content. setwaves applies that content above the water table.
  use GWFUZFMODULE, only: IWCRD, THTI, THTILAY, THTRLAY, THTSLAY, THTR, THTS
  use GLOBAL, only: IOUT, NCOL, NROW, NLAY
  implicit none
  integer :: ios, nrec, kstp, kper, ncolr, nrowr, ilay, ic, ir, k, napply
  integer :: lastk, lasts
  real :: pertim, totim, wmin, wmax, w
  real :: tr, ts
  character(len=16) :: text
  real, allocatable :: hold(:, :, :)
  real, allocatable :: blay(:, :)
  allocate(hold(NCOL, NROW, NLAY), blay(NCOL, NROW))
  hold = -1.0
  nrec = 0
  lastk = 0
  lasts = 0
  do
    read(IWCRD, iostat=ios) kstp, kper, pertim, totim, text, ncolr, nrowr, ilay
    if (ios /= 0) exit
    if (ncolr /= NCOL .or. nrowr /= NROW .or. ilay < 1 .or. ilay > NLAY) &
      call USTOP('UZF water-content record does not match the grid')
    read(IWCRD, iostat=ios) ((blay(ic, ir), ic=1, NCOL), ir=1, NROW)
    if (ios /= 0) call USTOP('UZF water-content record is incomplete')
    hold(:, :, ilay) = blay
    nrec = nrec + 1
    lastk = kper
    lasts = kstp
  end do
  if (nrec < 1) call USTOP('UZF water-content file has no records')
  napply = 0
  wmin = 0.0
  wmax = 0.0
  do k = 1, NLAY
    do ir = 1, NROW
      do ic = 1, NCOL
        if (hold(ic, ir, k) < 0.0) cycle
        if (allocated(THTRLAY)) then
          tr = THTRLAY(ic, ir, k)
          ts = THTSLAY(ic, ir, k)
        else
          tr = THTR(ic, ir)
          ts = THTS(ic, ir)
        end if
        if (ts <= tr) cycle
        w = hold(ic, ir, k)
        if (w <= 1.0e-6) cycle
        if (w < tr - 1.0e-5 .or. w > ts + 1.0e-5) cycle
        if (w < tr) w = tr
        if (w > ts) w = ts
        if (allocated(THTILAY)) then
          THTILAY(ic, ir, k) = w
        else
          THTI(ic, ir) = w
        end if
        if (napply == 0) then
          wmin = w
          wmax = w
        else
          if (w < wmin) wmin = w
          if (w > wmax) wmax = w
        end if
        napply = napply + 1
      end do
    end do
  end do
  if (napply < 1) call USTOP('UZF water-content file has no UZF cells')
  write(IOUT, '(1X,A,I5)') &
    'UZF READ CELL-AVERAGE WATER CONTENT FROM UNIT ', IWCRD
  write(IOUT, '(1X,A,I8,A,I8)') &
    'LAST RECORD IS STRESS PERIOD ', lastk, ' TIME STEP ', lasts
  write(IOUT, '(1X,A,I8,A,1PG14.6,A,1PG14.6)') &
    'CELLS SET ABOVE THE WATER TABLE = ', napply, ' MIN = ', wmin, ' MAX = ', wmax
end subroutine uzf_nwt_read_wcavg


subroutine uzf_nwt_write_budget(iout, ibudfl, iuzfopt, kstp, kper, &
                                c1v, c2v, c3v, c4v, r1v, r2v, r3v, r4v)
! Standard unsaturated-zone volumetric budget. Cumulative volumes are
! on the left and rates for this time step are on the right.
  implicit none
  integer, intent(in) :: iout, ibudfl, iuzfopt, kstp, kper
  double precision, intent(in) :: c1v, c2v, c3v, c4v
  double precision, intent(in) :: r1v, r2v, r3v, r4v
  double precision :: totvin, totvot, totrin, totrot, fmax
  double precision :: prcntercum, prcnterrat
  character(len=17) :: c1, r1, c2, r2, c3, r3, c4, r4, cd, rd
  character(len=16) :: infl, uzet, rch, stor
  if (ibudfl <= 0 .or. iuzfopt <= 0) return
  infl = '    INFILTRATION'
  uzet = '          UZF ET'
  rch = '    UZF RECHARGE'
  stor = '  STORAGE CHANGE'
  call uzf_nwt_fmt17(c1v, c1)
  call uzf_nwt_fmt17(r1v, r1)
  call uzf_nwt_fmt17(c2v, c2)
  call uzf_nwt_fmt17(r2v, r2)
  call uzf_nwt_fmt17(c3v, c3)
  call uzf_nwt_fmt17(r3v, r3)
  call uzf_nwt_fmt17(c4v, c4)
  call uzf_nwt_fmt17(r4v, r4)
  if (c4v > 0.0d0) then
    totvin = c1v
    totvot = c2v + c3v + c4v
  else
    totvin = c1v - c4v
    totvot = c2v + c3v
  end if
  if (r4v > 0.0d0) then
    totrin = r1v
    totrot = r2v + r3v + r4v
  else
    totrin = r1v - r4v
    totrot = r2v + r3v
  end if
  prcntercum = 0.0d0
  prcnterrat = 0.0d0
  if (abs(totvin + totvot) > 1.0d-15) &
    prcntercum = 100.0d0 * (totvin - totvot) / (totvin + totvot) / 2.0d0
  if (abs(totrin + totrot) > 1.0d-15) then
    fmax = totrin
    if (fmax < totrot) fmax = totrot
    prcnterrat = 100.0d0 * (totrin - totrot) / fmax
  end if
  call uzf_nwt_fmt17(c1v - (c2v + c3v), cd)
  call uzf_nwt_fmt17(r1v - (r2v + r3v), rd)
  write(iout, 9002) kstp, kper
  write(iout, 9003)
  write(iout, 9004) infl, c1, infl, r1
  write(iout, 9005)
  write(iout, 9004) uzet, c2, uzet, r2
  write(iout, 9004) rch, c3, rch, r3
  write(iout, 9006) cd, rd
  write(iout, 9007)
  write(iout, 9004) stor, c4, stor, r4
  write(iout, 9008)
  write(iout, 9009) prcntercum, prcnterrat
9002 format(1X//,'UNSATURATED ZONE PACKAGE VOLUMETRIC BUDGET FOR ', &
           ' TIME STEP', I6, ' STRESS PERIOD ', I5, /2X, 78('-')//)
9003 format(1X, /5X, 'CUMULATIVE VOLUMES', 6X, 'L**3', 7X, &
           'RATES FOR THIS TIME STEP', 6X, 'L**3/T'/5X, 18('-'), 17X, &
           24('-')//11X, 'IN:', 38X, 'IN:'/11X, '---', 38X, '---')
9004 format(1X, 3X, A16, ' =', A17, 6X, A16, ' =', A17)
9005 format(1X, /10X, 'OUT:', 37X, 'OUT:'/10X, 4('-'), 37X, 4('-'))
9006 format(1X, /12X, 'IN - OUT =', A, 14X, 'IN - OUT =', A)
9007 format(1X, /10X, 'STORAGE:', 33X, 'STORAGE:'/10X, 8('-'), 33X, &
           8('-'))
9008 format(1X, /1X, &
           'PERCENT DISCREPANCY IS DIFFERENCE BETWEEN IN-OUT', &
           ' MINUS CHANGE IN STORAGE', 1X, /1X, &
           'DIVIDED BY THE AVERAGE OF IN', ' AND OUT TIMES 100')
9009 format(1X, /1X, 'PERCENT DISCREPANCY =', F15.2, 5X, &
           'PERCENT DISCREPANCY =', F15.2, ///)
end subroutine uzf_nwt_write_budget


subroutine uzf_nwt_fmt17(val, text)
  implicit none
  double precision, intent(in) :: val
  character(len=17), intent(out) :: text
  character(len=32) :: buf
  integer :: n
  double precision :: a
  a = abs(val)
  if (a == 0.0d0 .or. (a >= 1.0d-3 .and. a < 1.0d8)) then
    write(buf, '(F17.4)') val
  else
    write(buf, '(1PE22.6E4)') val
  end if
  n = len_trim(buf)
  if (n <= 17) then
    text = adjustr(buf(1:n))
  else
    text = buf(n - 16:n)
  end if
end subroutine uzf_nwt_fmt17
