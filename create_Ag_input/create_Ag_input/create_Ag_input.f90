!  create_Ag_input.f90 
!
!  FUNCTIONS:
!  create_Ag_input - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: create_Ag_input
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program create_Ag_input

    implicit none

    ! Variables
    character(len=200) :: line
    character(len=10) :: char1,char2,char3,char4
    integer :: i, numstress,sp,yr,year,ii,numag
    integer :: istart,istop,lloc,IOSTAT,j,nhru,k
    REAL,SAVE, DIMENSION(:),POINTER :: jh_coef,kcrop
    integer,SAVE, DIMENSION(:),POINTER :: hru_id
    real :: r,jh,kr,jh_new
    ! Body of create_Ag_input
    numstress=372
    year=1980
    nhru=5609
    numag = 34
    allocate(hru_id(numag),jh_coef(12),kcrop(12))
    open(1,file='template.in')
    open(2,file='template.out')
    open(3,file='junk.out')
    open(4,file='ag_hrus.in')
!    open(5,file='jh_coef_month_low.in')
    open(5,file='jh_coef_month_high.in')
    open(6,file='jh_coef.out')
!
!   read ag hru ids
    do i=1,numag
      CALL URDCOM(4, 3, line)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,j,R,3,4)
      hru_id(i) = j
    end do
!
! read values by month
    do i=1,12
      CALL URDCOM(5, 3, line)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,j,jh,3,5)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,j,kr,3,5)
      jh_coef(i) = jh
      kcrop(i) = kr
    end do
!
! calculate and write jh_coefs nhru by nmonth
    write(6,*)'jh_coef'
    write(6,*)'2'
    write(6,*)'nhru'
    write(6,*)'nmonths'
    write(6,*)'67308'
    write(6,*)'2'
    do i=1,12
      do j=1,nhru
         jh_new = jh_coef(i)
         do k=1,numag
            if ( hru_id(k)==j ) jh_new = jh_new*kcrop(i)
         end do
         write(6,*)jh_new
      end do
    end do
!
    j=1
    do i=2,numstress
      j=j+1
      do
        read(1,200,IOSTAT=Iostat) line
        if( Iostat < 0 ) then
            rewind(1)
            j = j - 1
            exit
        end if
        lloc=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,II,R,3,1)
        select case(line(istart:istop))
        case ('STRESS')
            char1 = line(istart:istop)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,II,R,3,1)
            char2 = line(istart:istop)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,sp,R,3,1)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,II,R,3,1)
            char3 = line(istart:istop)
            if(char3=='#JAN')year = year + 1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,yr,R,3,1)
          write(2,100)char1,char2,j,char3,year
        case ('END')
            write(2,*)'END'
            exit
        case default
          write(2,200)line
        end select
      end do
    end do
100 format(2a7,3x,i6,5x,a10,i6) 
200 format(a200)

    end program create_Ag_input

