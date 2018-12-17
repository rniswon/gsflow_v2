!  CSV_merge.f90 
!
!  FUNCTIONS:
!  CSV_merge - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: CSV_merge
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program CSV_merge
!    USE IFPORT

    implicit none

    ! Variables
    character*1000 line
    character*60 name, mergfile, namfile
    integer i, j, numfiles, numlines, reason
!    character*200 dir
    ! Body of CSV_merge
    mergfile = '..\output\gsflow_merged.csv'
    namfile = '.\names.dat'
    open(88, file=trim(mergfile), status='unknown')
    open(77, file=trim(namfile), status='old')
    read(77, *) numfiles
    print *, 'number of files to merge:', numfiles
!
!    reason = getcwd(dir)
    numlines = 5000
    do i = 1, numfiles
      read(77, '(a)') name
      print *, 'merge filename: ', name
      open(i+9, file=trim(name))
      read(i+9, '(A)', IOSTAT=reason) line
      IF ( i==1 ) write(88, '(A)') line
      do while (reason==0 )
        read(i+9, '(A)', IOSTAT=reason) line
        IF ( reason==0 ) write(88, '(A)') line
      end do
    end do
    do i = 1, numfiles
      close(i+9)
    end do
    end program CSV_merge

