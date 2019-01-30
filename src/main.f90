! High Performance Computing F19 (41391)
! Svend Andersen
!   This Fortran program solves a unsteady, two-dimensional diffusion problem.
!   The program is parallelized using the MPI API and thus depends on this.
!
!   BUILD:
!   To build the program simply run 'make', which has been written for the
!   sun compiler mpi90.  Subsequently the program may be executed by:
!   'mpirun -np [number of procs] ./bin'.
!
!   PROGRAM STRUCTURE:
!   The core files of this program is
!   * main.f90
!   * sub_init.f90
!   * mod_iteration.f90
!     - subr: iterate
!     - subr: par_iterate
!     - subr: stitch
!
!   NOTE: If the file 'T_np01.dat' is not present, the error cannot be
!   calculated.  The file may be recreated by running the program with '-np 1'
!
!   The speed-up study was performed using the 'test_utility.sh' script, and
!   the resulting graph is found in 'speed-up.pdf'.  The result is suprising in
!   that it seems that one does not gain much efficiency using more than 3
!   processors.  This is likely due to the increasing communication overhead.

program main
  use globdata
  use iteration
  use timetools, only: timing_mpi
  use io, only: output, print_array, output_t, read_t
  implicit none
  include "mpif.h"

  real(8),  dimension(:, :), allocatable :: T_np1 ! Array used in comparison
  character(len=128) :: p_name ! Processor name
  integer :: pn_le, info       ! Proc name string length

  call MPI_Init(info); if(info .ne. 0) write(*,'("MPI ERROR Init")')
  call MPI_Comm_rank(MPI_comm_world, rank, info)   ! Get rank number
  call MPI_Comm_size(MPI_comm_world, pnum, info)   ! Get number of procs
  call MPI_Get_processor_name(p_name, pn_le, info) ! Get name of processor

  ! Print MPI status
  if ( rank == 0 ) write(*,'(/3ai2.2a/)') 'RUNNING ON PROCESSOR ',&
    p_name(1:pn_le),' WITH ',pnum,' CORE(S)'

  call init             ! Initialize problem
  if ( pnum == 1 ) then
    call timing_mpi     ! Tic
    call iterate        ! Perform single-proc iteration
    call timing_mpi     ! Toc
    call output_T       ! Output T for comparison
  else
    call timing_mpi     ! Tic
    call par_iterate    ! Perform multi-proc iteration
    call timing_mpi     ! Toc
    call stitch         ! Collect and stitch data
  endif

  ! Compute RMS error to validate parallel solution
  !if (rank == 0) then
  !  call read_T(T_np1, 'T_np01.dat') ! Read seq. field for comparison
  !  write(*,'("Error was: "e9.2/)') sqrt(1.0_mkd/real(nx*ny))*(sum(T-T_np1)**2)
  !  !call output
  !endif

  call MPI_Finalize(info); if ( info.ne.0 ) write(*,'("MPI ERROR Finalize")')
end program main
