module alloc_mod
  ! The module provides an alias for the allocation procedure 'alloc_field', 
  ! overloaded for single- and double precision.  The module ought to be 
  ! rewritten as it is rather repetitive
  use globdata, only: mks, mkd
  implicit none
  private
  public :: alloc_field

  interface alloc_field
    module procedure alloc_s, alloc_d
  end interface alloc_field

contains

  subroutine alloc_s(field, nx, ny, info)
    real(mks), dimension(:,:), intent(inout), allocatable :: field
    integer, intent(in) :: nx, ny
    integer, intent(out) :: info

    real(mks), dimension(:,:), allocatable :: work
    integer :: nx_old, ny_old, i, j

    if ( .not. allocated(field) ) then
      allocate (field(ny, nx), stat=info); if (info .ne. 0) call al_fail
    else
      ! Get size of old array
      nx_old = size(field, 1)
      ny_old = size(field, 2)

      ! Check that the requested array size is bigger or equal to the orginal
      if ( nx_old .le. nx .and. ny_old .le. ny ) then
        allocate(work(nx_old, ny_old), stat=info)
        if ( info .ne. 0 ) call al_fail

        ! Copy old array to temporary array
        do j = 1, ny_old
          do i = 1, nx_old
            work(i,j) = field(i,j)
          enddo
        enddo

        ! Deallocate old array and allocate a new bigger array and initialize it
        deallocate(field, stat=info);       if ( info .ne. 0 ) call al_fail
        allocate(field(ny, nx), stat=info); if ( info .ne. 0 ) call al_fail
        field = 0

        ! Copy old data to the new bigger array
        do j = 1, ny_old
          do i = 1, nx_old
            field(i,j) = work(i,j)
          enddo
        enddo

        deallocate(work)
      else
        write(*,'(/a)') 'ERROR trying to allocate smaller array - terminating'
        stop 1
      endif
    end if
  end subroutine alloc_s

  subroutine alloc_d(field, nx, ny, info)
    real(mkd), dimension(:,:), intent(inout), allocatable :: field
    integer, intent(in) :: nx, ny
    integer, intent(out) :: info

    real(mkd), dimension(:,:), allocatable :: work
    integer :: nx_old, ny_old, i, j

    if ( .not. allocated(field) ) then
      allocate (field(ny, nx), stat=info); if (info .ne. 0) call al_fail
    else
      ! Get size of old array
      nx_old = size(field, 1)
      ny_old = size(field, 2)

      ! Check that the requested array size is bigger or equal to the orginal
      if ( nx_old .le. nx .and. ny_old .le. ny ) then
        allocate(work(nx_old, ny_old), stat=info)
        if ( info .ne. 0 ) call al_fail

        ! Copy old array to temporary array
        do j = 1, ny_old
          do i = 1, nx_old
            work(i,j) = field(i,j)
          enddo
        enddo

        ! Deallocate old array and allocate a new bigger array and initialize it
        deallocate(field, stat=info);       if ( info .ne. 0 ) call al_fail
        allocate(field(ny, nx), stat=info); if ( info .ne. 0 ) call al_fail
        field = 0

        ! Copy old data to the new bigger array
        do j = 1, ny_old
          do i = 1, nx_old
            field(i,j) = work(i,j)
          enddo
        enddo

        deallocate(work)
      else
        write(*,'(/a)') 'ERROR trying to allocate smaller array - terminating'
        stop 1
      endif
    end if
  end subroutine alloc_d

  subroutine al_fail
    write(*,'(a)') 'ERROR allocation failed in subroutine "alloc_field"'
    stop 1
  end subroutine al_fail

end module alloc_mod
