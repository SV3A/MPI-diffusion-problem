module iteration
  ! Module to perform the field iteration
  use globdata
  use io, only: print_array
  implicit none
  include "mpif.h"

  integer :: info

  private
  public :: iterate, par_iterate, stitch

contains

  subroutine iterate
    real(mkt) :: fac1, fac2, fac3
    integer :: i, j, n

    fac1 = d*dt
    fac2 = 1_mkd/dx**2; fac3 = 1_mkd/dy**2

    do n = n_start, n_max
      if ( mod(n,2) == 0 ) then
        do j = 2, nx_loc-1
          do i = 2, ny_loc-1
            T(i,j) = fac1*( (T_old(i+1,j) - 2*T_old(i,j) + T_old(i-1,j))*fac2 + &
              (T_old(i,j+1) - 2*T_old(i,j) + T_old(i,j-1))*fac3 ) + T_old(i,j)
          enddo
        enddo
      else
        do j = 2, nx_loc-1
          do i = 2, ny_loc-1
            T_old(i,j) = fac1*( (T(i+1,j) - 2*T(i,j) + T(i-1,j))*fac2 + &
              (T(i,j+1) - 2*T(i,j) + T(i,j-1))*fac3 ) + T(i,j)
          enddo
        enddo
      endif
    enddo

    ! In case n_max is not even update the T field one last time
    if ( mod(n_max,2) /= 0 ) then
      do j = 2, nx-1
        do i = 2, ny-1
          T(i,j) = fac1*( (T_old(i+1,j) - 2*T_old(i,j) + T_old(i-1,j))*fac2 + &
            (T_old(i,j+1) - 2*T_old(i,j) + T_old(i,j-1))*fac3 ) + T_old(i,j)
        enddo
      enddo
    endif

  end subroutine iterate

  subroutine par_iterate
    real(mkt) :: fac1, fac2, fac3
    real(mkt), dimension(1:ny_loc-2) :: send_buffer, recv_buffer
    integer :: i, j, n, m, ndest, nsrc, send_column, recv_column
    logical :: even

    fac1 = d*dt
    fac2 = 1_mkd/dx**2; fac3 = 1_mkd/dy**2

    ! Iterate
    do n = n_start, n_max
      if ( mod(n,2) == 0 ) then; even = .true.; else; even = .false.; endif

        if ( even ) then
          do j = 2, nx_loc-1
            do i = 2, ny_loc-1
              T(i,j) = fac1*( (T_old(i+1,j) - 2*T_old(i,j) + T_old(i-1,j))*fac2 + &
                (T_old(i,j+1) - 2*T_old(i,j) + T_old(i,j-1))*fac3 ) + T_old(i,j)
            enddo
          enddo
        else
          do j = 2, nx_loc-1
            do i = 2, ny_loc-1
              T_old(i,j) = fac1*( (T(i+1,j) - 2*T(i,j) + T(i-1,j))*fac2 + &
                (T(i,j+1) - 2*T(i,j) + T(i,j-1))*fac3 ) + T(i,j)
            enddo
          enddo
        endif

        ! Exhange ghost layers, first right then left
        exchange: do m = 1, 2
          ! Define exchange direction
          if ( m == 1 ) then
            send_column = nx_loc-1
            recv_column = 1
            ndest = rank+1
            nsrc  = rank-1
            if (ndest == pnum) ndest = MPI_Proc_null ! Check if we are at end
            if (nsrc == -1)    nsrc  = MPI_Proc_null ! columns and if so nullify
          else
            send_column = 2
            recv_column = nx_loc
            ndest = rank-1
            nsrc  = rank+1
            if (ndest == -1)   ndest = MPI_Proc_null
            if (nsrc == pnum)  nsrc  = MPI_Proc_null
          endif

          ! Define buffers to be sent
          if ( even ) then
            do i = 1, ny_loc-2
              send_buffer(i) = T(i+1,send_column)
            enddo
          else
            do i = 1, ny_loc-2
              send_buffer(i) = T_old(i+1,send_column)
            enddo
          endif

          ! Send/recv ghost layers
          call MPI_Sendrecv(send_buffer, ny_loc-2, MPI_Double_precision, &
            ndest, 0, recv_buffer, ny_loc-2, MPI_Double_precision, nsrc, 0, &
            MPI_Comm_world, MPI_Status_ignore, info)
          if ( info /= 0 ) print *, 'Sendrecv error'

          if ( nsrc == -2 ) cycle exchange ! Cycle if source is null

          ! Update ghost layers
          if ( even ) then
            do i = 1, ny_loc-2
              T(i+1,recv_column) = recv_buffer(i)
            enddo
          else
            do i = 1, ny_loc-2
              T_old(i+1,recv_column) = recv_buffer(i)
            enddo
          endif
        enddo exchange
      enddo

      ! In case n_max is not even update the T field one last time
      if ( mod(n_max,2) /= 0 ) then
        do j = 2, nx-1
          do i = 2, ny-1
            T(i,j) = fac1*( (T_old(i+1,j) - 2*T_old(i,j) + T_old(i-1,j))*fac2 + &
              (T_old(i,j+1) - 2*T_old(i,j) + T_old(i,j-1))*fac3 ) + T_old(i,j)
          enddo
        enddo
      endif
    end subroutine par_iterate

    subroutine stitch
      use alloc_mod

      real(8), dimension(:,:), allocatable :: rbuf_T
      integer :: info, irank, cur_nx, cur_ny, i, j, col_count

      if ( rank > 0 ) then
        call MPI_Send(T, size(T,1)*size(T,2), MPI_Double_precision, 0, 0, &
          MPI_Comm_world, info)
      endif

      if ( rank == 0 ) then
        ! Expand the root field array to global size (nx x ny)
        call alloc_field(T, nx, ny, info)

        ! Increment counter denoting the last inserted column (-1 removes g. layer)
        col_count = field_sizes(2)-1

        ! Insert remaining fields
        do irank = 1, pnum-1
          ! Get size of the current array (irank+1 to skip root's index)
          cur_ny = field_sizes((irank+1)*2-1)
          cur_nx = field_sizes((irank+1)*2)

          if ( irank == 1 ) then
            allocate(rbuf_T(cur_ny, cur_nx))
          elseif (cur_ny /= field_sizes(irank*2-1) .or. &
              cur_nx /= field_sizes(irank*2  )) then
              deallocate(rbuf_T)
              allocate(rbuf_T(cur_ny, cur_nx))
            endif

            call MPI_Recv(rbuf_T, cur_ny*cur_nx, MPI_Double_precision, irank, 0,&
              MPI_Comm_world, MPI_Status_ignore, info)

            do j = 2, cur_nx
              do i = 1, cur_ny
                T(i, col_count+j-1) = rbuf_T(i,j)
              enddo
            enddo

            ! Update last column (-2 since internal slices have two ghost layers)
            col_count = col_count+cur_nx-2

          enddo
          deallocate(rbuf_T)
        endif
      end subroutine stitch
    end module iteration

