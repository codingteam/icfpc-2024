module space_m
  implicit none
  type :: star_t
    integer :: x
    integer :: y
  end type
contains
  subroutine initial_pairs(ship, stars)
    type(star_t), intent(in) :: ship
    type(star_t), intent(in) :: stars(:)
    ! integer
    integer :: i, j, dx1, dx2, dy1, dy2
    logical :: found
    do i = 1, size(stars)
      dx1 = stars(i)%x - ship%x
      dy1 = stars(i)%y - ship%y
      if (abs(dx1) <= 1 .and. abs(dy1) <= 1) then
        do j = 1, size(stars)
          if (i == j) cycle
          dx2 = stars(j)%x - stars(i)%x
          dy2 = stars(j)%y - stars(i)%y
          if (abs(dx2-dx1) <= 1 .and. abs(dy2-dy1) <= 1) then
            print "(3(A,I0))", " ship - ", i, " : ", i, " - ", j
            found = .true.
          end if
        end do
      end if
    end do
    if(.not.found) then
      print '(A)', "No star are available from start!"
    end if
  end subroutine initial_pairs
  subroutine print_pairs(stars, star_id)
    type(star_t), intent(in) :: stars(:)
    integer, intent(in) :: star_id
    !
    type(star_t) :: star
    integer :: dx1, dx2, dy1, dy2
    integer :: i, j
    logical :: found
    star = stars(star_id)
    found = .false.
    do i = 1, size(stars)
      if (i == star_id) cycle
      dx1 = star%x - stars(i)%x
      dy1 = star%y - stars(i)%y
      do j = i, size(stars)
        if (j == star_id) cycle
        dx2 = stars(j)%x - star%x
        dy2 = stars(j)%y - star%y
        if (abs(dx2-dx1) <= 1 .and. abs(dy2-dy1) <= 1) then
          print "(4(A,I0))", "", i, " - ", star_id, " : ", star_id, " - ", j
          print "(4(A,I0))", "", j, " - ", star_id, " : ", star_id, " - ", i
          found = .true.
        end if
      end do
    end do
    if(.not.found) then
      print '(A,I0,A)', "Star ", star_id, " is unreachable!"
    end if
  end subroutine print_pairs
end module space_m

program main
  use space_m
  implicit none
  type(star_t), allocatable :: stars(:)
  type(star_t) :: ship
  integer :: istar
  integer :: task_id
  ship%x = 0
  ship%y = 0
  read(*,*) task_id
  stars = load_stars(task_id)
  call initial_pairs(ship, stars)
  do istar = 1, size(stars)
    call print_pairs(stars, istar)
  end do
contains
  function load_stars(task_id) result(stars)
    integer, parameter :: N_size(25) = &
      (/ 5, 50, 10, 100, 116, 85, 50, 50, 100, 100, &
         8192, 8192, 8192, 100, 19, 497, 100, 100, &
         8832, 900, 900, 300, 40000, 4096, 65530 /)
    integer, intent(in) :: task_id
    type(star_t), allocatable :: stars(:)
    character(len=2) :: task_id_chr
    integer :: fd, i

    write(task_id_chr, "(I2)") task_id
    open(newunit=fd, file = "spaceship" // trim(adjustl(task_id_chr)) // ".txt", &
         form="formatted", status="old")
    allocate(stars(N_size(task_id)))
    do i = 1, N_size(task_id)
      read(fd, *) stars(i)%x, stars(i)%y
    end do
  end function load_stars
end program main
