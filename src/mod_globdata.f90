module globdata
  ! Module containing common data used throughout the program
  implicit none

  ! Define single and double precission
  integer, parameter :: mks = kind(1.0e0)
  integer, parameter :: mkd = kind(1.0d0)

  real(mkd), dimension(:,:), allocatable :: T, T_old
  integer, dimension(:), allocatable :: field_sizes
  real(mkd) :: d, dt, lx, ly, dx, dy, t_total
  integer :: nx, ny, n_start, n_max, nx_loc, ny_loc
  integer :: rank, pnum

  ! Parameter containing the type of the field
  integer, parameter :: mkt = kind(T)

end module globdata
