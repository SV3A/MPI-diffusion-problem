subroutine init
  ! Subroutine called from main.f90 to initialize the problem
  use globdata
  use alloc_mod
  use io, only: read_input, print_array
  implicit none
  include "mpif.h"
  integer :: i, j, info

  ! Read input on master and broadcast
  if ( rank == 0 ) call read_input('inputfile.txt')
  call MPI_Bcast(lx   , 1, MPI_Double_precision, 0, MPI_comm_world, info)
  call MPI_Bcast(ly   , 1, MPI_Double_precision, 0, MPI_comm_world, info)
  call MPI_Bcast(d    , 1, MPI_Double_precision, 0, MPI_comm_world, info)
  call MPI_Bcast(nx   , 1, MPI_Integer, 0, MPI_comm_world, info)
  call MPI_Bcast(ny   , 1, MPI_Integer, 0, MPI_comm_world, info)
  call MPI_Bcast(n_max, 1, MPI_Integer, 0, MPI_comm_world, info)

  ! Calculate local number of grid points
  nx_loc = nint(real(nx)/real(pnum))
  ny_loc = ny

  ! Since 'nint' may round up check if sum of local slabs exceeds glob. points
  if (nx_loc*pnum > nx) nx_loc = floor(real(nx)/real(pnum))

  if ( rank == pnum-1 ) then
    nx_loc = nx-(pnum-1)*nx_loc ! if remainder is present, give to last proc
  endif

  ! Add ghost layers
  if ( pnum > 1 ) then
    if ( rank == 0 .or. rank == pnum-1 ) then
      nx_loc = nx_loc + 1
    else
      nx_loc = nx_loc + 2
    endif
  endif

  ! Allocate temperature fields
  call alloc_field(    T, nx_loc, ny, info); if ( info .ne. 0 ) goto 2
  call alloc_field(T_old, nx_loc, ny, info); if ( info .ne. 0 ) goto 2

  ! Collect field sizes on the root processor
  allocate(field_sizes(pnum*2))
  call MPI_Gather((/size(T,1), size(T,2)/), 2, MPI_Integer, field_sizes, 2, &
    MPI_Integer, 0, MPI_Comm_world, info)

  ! Set initial conditions
  T = 1
  if ( pnum > 1 ) then
    if ( rank == 0 ) then
      do j = 2, nx_loc
        do i = 2, ny_loc-1
          T(i,j) = 0
        enddo
      enddo
    elseif ( rank == pnum-1 ) then
      do j = 1, nx_loc-1
        do i = 2, ny_loc-1
          T(i,j) = 0
        enddo
      enddo
    else
      do j = 1, nx_loc
        do i = 2, ny_loc-1
          T(i,j) = 0
        enddo
      enddo
    endif
  else
    do j = 2, nx_loc-1
      do i = 2, ny_loc-1
        T(i,j) = 0
      enddo
    enddo
  endif

  ! Initiate the n-1 field
  T_old = T

  ! Define grid spacing
  dx = real(lx)/real(nx-1)
  dy = real(ly)/real(ny-1)

  ! Time increment
  dt = min(dx,dy)**2/(4.0_mkd*d)

  ! Set global iteration start variable
  n_start = 1 

  return

  ! Error handling
  2 write(*,'(a)') 'ERROR allocation failed in subroutine "init"'
  stop 1

end subroutine init
