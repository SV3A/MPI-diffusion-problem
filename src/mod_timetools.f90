module timetools
  ! Module containing methods for timing the program
  implicit none
  private
  public :: timing, timing_mpi, greet

  real(8) :: t1, t2
  integer :: c1, c2, c_rate
  character(len=8) :: date
  character(len=10) :: time

contains

  subroutine timing
    logical :: tic = .false.

    if ( tic .eqv. .false. ) then
      tic = .true.
      call system_clock(c1, c_rate)
      call cpu_time(t1)
    else
      call cpu_time(t2)
      call system_clock(c2)

      write(*,'(/af10.6a)') 'CPU time was:  ', (t2-t1) ,' s'
      write(*,'(af10.6a/)') 'Wall time was: ', real(c2-c1)/real(c_rate),' s'
      tic = .false.
    endif
  end subroutine timing

  subroutine timing_mpi
    use globdata, only: rank
    include "mpif.h"

    integer :: info
    logical :: tic = .false.

    call MPI_Barrier(MPI_Comm_world, info)
    if ( rank == 0 ) then

      if ( tic .eqv. .false. ) then
        tic = .true.
        t1 = MPI_Wtime()
      else
        t2 = MPI_Wtime()

        write(*,'(/af10.6a/)') 'Wall time was: ', t2-t1,' s'
        tic = .false.
      endif

    endif
  end subroutine timing_mpi

  subroutine greet(mode)
    character(len=*), intent(in) :: mode

    call date_and_time(date, time)

    if ( mode .eq. 'hello' ) then
      write(*,'(/atr2a/)') 'EXECUTION STARTED AT ',time(1:2)//':'//&
        &time(3:4)//':'//time(5:6)//' ON '//date(5:6)//'/'//&
        &date(7:8)//'-'//date(1:4)
    elseif ( mode .eq. 'bye' ) then
      write(*,'(atr1a/)') 'EXECUTION FINISHED AT ',time(1:2)//':'//&
        &time(3:4)//':'//time(5:6)//' ON '//date(5:6)//'/'//&
        &date(7:8)//'-'//date(1:4)
    else
      write(*,'(a)') 'Error: Wrong argument to "greet" subroutine.'
      stop
    endif
  end subroutine greet
end module timetools
