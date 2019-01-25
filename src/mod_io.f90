module io
  ! Module containing procedures for I/O operations
  implicit none
contains

  subroutine read_input(filename)
    ! Reads a input file and assigns given variables
    use globdata
    character(len=*), intent(in) :: filename

    character(len=80) :: line, lhs, rhs
    logical :: in_ex
    integer :: n, ios, i_es

    ! Check wether input file exists
    inquire(file = filename, exist = in_ex)

    if ( in_ex .eqv. .false. ) then
      write(*,'(/3a/)') 'Input file "',filename,'" not found - terminating'
      stop
    endif

    write(*,'(a/)') 'READING INPUT FILE...'

    ! Open file for reading
    open(15, file=filename, form='formatted', status='old', action='read', &
      iostat=ios)

    ! Loop over each line in the input file
    n = 0
    do
      read (15, '(a)', iostat=ios, err=100, end=200) line
      n = n + 1

      ! Flush left
      line = adjustl(line)

      ! Ignore empty line or comments
      if ( line(1:1) .eq. ' ' .or. line(1:1) .eq. '#' ) cycle

      ! Find position of equal sign
      i_es = index(line, '=')

      ! Handle no, or multiple, equal sign(s) found
      if ( i_es .eq. 0 .or. index(line(i_es+1: len(line)), '=') .ne. 0 ) then
        write(*,'("ERROR pertaining to equal sign in input file (line ",&
          &i0.0,")")') n
        stop
      endif

      ! Store left- and right hand side (after sanitisation)
      lhs = to_lower( adjustl( line(1:i_es-1) ) )
      rhs = adjustl( line(i_es+1: len(line)) )

      ! Check if the key-value is a complete pair
      if ( lhs(1:1) .eq. ' ' .or. rhs(1:1) .eq. ' ' ) then
        write(*,'("ERROR incomplete key-value pair in input file (line ",&
          &i0.0,")")') n
        stop
      endif

      ! Assign variables
      select case (trim(lhs))
      case ("lx")
        read(rhs,*) lx
      case ("ly")
        read(rhs,*) ly
      case ("nx")
        read(rhs,*) nx
      case ("ny")
        read(rhs,*) ny
      case ("d")
        read(rhs,*) d
      case ("n_max")
        read(rhs,*) n_max
      case ("t_total")
        read(rhs,*) t_total
      case default 
        write(*,'(3ai0.0a)') 'WARNING unknown variable "',trim(lhs),&
          &'" declared in input file (line ',n,') - skipping this'
      end select
    enddo

    ! Labels for read statement
    100 continue
    write(*,'("ERROR while reading the input file on line ", i0.0)') n 

    200 continue
    ! Report assignments
    write(*,'(a)', advance='no') 'Read from input file: '
    write(*,'(tr2f5.3", "f5.3", "i0.0", "i0.0", "f5.3", "i0.0", "f5.3)', &
      advance='no') lx, ly, nx, ny, d, n_max, t_total
    write(*,'(a)') ' for (lx, ly, nx, ny, d, n_max, t_total)'

    ! Close file
    close(15)
  end subroutine read_input

  subroutine read_restart(filename)
    use globdata
    use alloc_mod

    character(len=*), intent(in) :: filename
    integer :: info
    integer :: n_stopped

    open(12, file=filename, form='unformatted')
    read(12) n_stopped, d, lx, ly, nx, ny, n_max, t_total
    n_start = n_stopped + 1

    ! Allocate
    call alloc_field(T, nx, ny, info)
    if ( info .ne. 0 ) then
      print *, 'ERROR allocation failed reading restart file'
      stop
    endif

    read(12) T
    close(12)
  end subroutine read_restart

  subroutine output(tstep)
    ! Writes the temperature field to disc
    use globdata

    integer, intent(in), optional :: tstep
    character(len=20) :: string
    integer :: i, j

    if ( present(tstep) ) then
      write(string, '(a,i6.6,a)') 'diff.',tstep,'.dat'
    else
      write(string, '(a)') 'diff_final.dat'
    end if

    open(10, file = string)
    do j = 1, ny
      do i = 1, nx
        write(10,'(3e12.4)') real(i-1)*dx, real(j-1)*dy, T(i,j)
      enddo
      write(10,'(a)') ! Produces a new empty line - make gnuplot lift the pen
    enddo
    close(10)
  end subroutine output

  subroutine output_restart(n)
    use globdata
    integer, intent(in) :: n
    open(12, file = 'restart.input', form='unformatted')
    write(12) n, d, lx, ly, nx, ny, n_max, t_total
    write(12) T
    close(12)
  end subroutine output_restart

  subroutine output_T
    use globdata
    character(len=20) :: filename
    write(filename, '(ai2.2a)') 'T_np',pnum,'.dat'
    open(12, file = filename, form='unformatted')
    write(12) nx, ny
    write(12) T
    close(12)
  end subroutine output_T

  subroutine read_T(m, filename)
    character(len=*), intent(in) :: filename
    real(8), dimension(:,:), intent(inout), allocatable :: m
    integer :: nx, ny

    open(12, file=filename, form='unformatted')
    read(12) nx, ny

    allocate(m(ny,nx))

    read(12) m
    close(12)
  end subroutine read_T

  subroutine diagnose(a, n, close_flag)
    use globdata, only: mkt
    implicit none

    real(mkt), dimension(:,:), intent(in) :: a
    integer, intent(in), optional :: n
    logical, intent(in), optional :: close_flag

    character(len=8) :: filename = 'diag.dat'
    logical :: first = .true.

    ! Open file for writing if called for the first time
    if ( first ) then
      open(20, file = filename)
      first = .false.
    endif

    ! Write to disc
    if ( present(n) ) then
      write(20,'(f11.9ai6.6a)') minval(a),' (Iteration: ',n,')'
    else
      write(20,'(f11.9)') minval(a)
    endif

    ! Close file
    if ( present(close_flag) .and. (close_flag .eqv. .true.)  ) then
      close(20)
      first = .true.
    endif
  end subroutine diagnose

  function to_lower(str_in) result(str_out)
    ! Takes a string, converts all characters to lower case and returns a new 
    ! string
    character(len=*), intent(in) :: str_in
    character(len=len(str_in)) :: str_out
    integer :: i, j

    do i = 1, len(str_in)
      j = iachar( str_in(i:i) )
      if ( j >= iachar("A") .and. j <= iachar("Z") ) then
        str_out(i:i) = achar( j+32 )
      else
        str_out(i:i) = str_in(i:i)
      end if
    end do

  end function to_lower

  subroutine print_array(m)
    ! Debug routine for printing array of arbitrary size
    real(8), dimension(:,:), intent(in) :: m
    character(len=10) :: prt_format
    character(len=10) :: nr_cols
    integer :: i

    ! Define format
    write(nr_cols, "(i0.0)") int(size(m,2))
    prt_format = '('//trim(nr_cols)//'f6.3)'

    ! Print to console
    write(*, '(/"-----")')
    do i = 1, int(size(m,1))
      print trim(prt_format), m(i,:)
    end do
    write(*, '("-----"/)')
  end subroutine print_array

end module io

