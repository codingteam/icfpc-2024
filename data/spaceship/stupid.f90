module space_m
  implicit none
  integer, allocatable :: triangle_numbers(:)
  type :: ship_t
    integer :: x = 0
    integer :: y = 0
    integer :: vx = 0
    integer :: vy = 0
  end type
  type :: star_t
    integer :: x = 0
    integer :: y = 0
  end type
contains
  integer function walk(ship, stars, steps) result(nsteps)
    type(ship_t), intent(inout) :: ship
    type(star_t), intent(in) :: stars(:)
    integer(1) :: steps(-1:)
    !
    integer :: istar
    type(star_t) :: next_star
    logical :: do_walk_x
    !
    do_walk_x = .true.
    nsteps = 1
    do istar = 1, size(stars)
      next_star = stars(istar)
      nsteps = nsteps + 1
      steps(nsteps) = 0
      if (do_walk_x) then
        call walk_x()
        call walk_y()
      else
        call walk_x()
        call walk_y()
      end if
    end do
  contains
    subroutine walk_x()
      if (next_star%x < ship%x) then
        if (steps(nsteps-1) == 0 .and. &
            steps(nsteps-2) == 6) then
          nsteps = nsteps - 1
          steps(nsteps-1) = 5
          steps(nsteps) = 0
        else
          steps(nsteps) = 4
        end if
        nsteps = nsteps + 1
        ship%x = ship%x - 1
        do while (next_star%x < ship%x)
          ship%x = ship%x - 1
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 6
        nsteps = nsteps + 1
        do_walk_x = .true.
      else if (next_star%x > ship%x) then
        if (steps(nsteps-1) == 0 .and. &
            steps(nsteps-2) == 4) then
          nsteps = nsteps - 1
          steps(nsteps-1) = 5
          steps(nsteps) = 0
        else
          steps(nsteps) = 6
        end if
        nsteps = nsteps + 1
        ship%x = ship%x + 1
        do while (next_star%x > ship%x)
          ship%x = ship%x + 1
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 4
        nsteps = nsteps + 1
        do_walk_x = .true.
      end if
    end subroutine walk_x
    subroutine walk_y()
      if (next_star%y < ship%y) then
        if (steps(nsteps-1) == 0 .and. &
            steps(nsteps-2) == 8) then
          nsteps = nsteps - 1
          steps(nsteps-1) = 5
          steps(nsteps) = 0
        else
          steps(nsteps) = 2
        end if
        nsteps = nsteps + 1
        ship%y = ship%y - 1
        do while (next_star%y < ship%y)
          ship%y = ship%y - 1
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 8
        nsteps = nsteps + 1
        do_walk_x = .false.
      else if (next_star%y > ship%y) then
        if (steps(nsteps-1) == 0 .and. &
            steps(nsteps-2) == 2) then
          nsteps = nsteps - 1
          steps(nsteps-1) = 5
          steps(nsteps) = 0
        else
          steps(nsteps) = 8
        end if
        nsteps = nsteps + 1
        ship%y = ship%y + 1
        do while (next_star%y > ship%y)
          ship%y = ship%y + 1
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 2
        nsteps = nsteps + 1
        do_walk_x = .false.
      end if
    end subroutine walk_y
  end function walk
  subroutine init_space_m()
    integer, parameter :: tr_max = 1000
    integer :: i
    allocate(triangle_numbers(0:tr_max), source = 0)
    do i = 0, tr_max
      triangle_numbers(i) = i * (i + 1) / 2
    end do
  end subroutine init_space_m
end module space_m

program main
  use space_m
  implicit none
  integer, parameter :: max_steps = 1000000
  type(star_t), allocatable :: stars(:)
  type(ship_t) :: ship
  integer(1), allocatable :: steps(:)
  integer :: nsteps, task_id, i
  call init_space_m()
  allocate(steps(-1:max_steps), source = 0_1)
  print *, triangle_numbers
  read(*,*) task_id
  stars = load_stars(task_id)
  nsteps = walk(ship, stars, steps) - 1
  do i = 1, nsteps
    if (steps(i) /= 0) &
      write(*, "(I0)", advance="no") steps(i)
  end do
contains
  function load_stars(task_id) result(stars)
    integer, parameter :: N_size(26) = &
      (/ 5, 50, 10, 100, 116, 85, 50, 50, 100, 100, &
         8192, 8192, 8192, 100, 19, 497, 100, 100, &
         8832, 900, 900, 300, 40000, 4096, 65530, 3 /)
!    integer, parameter :: N_size(25) = &
!      (/ 5, 49, 10, 99, 113, 85, 49, 50, 99, 100, &
!        8192, 8192, 8192, 100, 19, 497, 100, 100, &
!        8832, 900, 900, 299, 39996, 4096, 65530 /)
    integer, intent(in) :: task_id
    type(star_t), allocatable :: stars(:)
    character(len=2) :: task_id_chr
    integer :: fd, i

    write(task_id_chr, "(I2)") task_id
    open(newunit=fd, file = "spaceship" // trim(adjustl(task_id_chr)) // ".txt_sort", &
         form="formatted", status="old")
    allocate(stars(N_size(task_id)))
    do i = 1, N_size(task_id)
      read(fd, *) stars(i)%x, stars(i)%y
    end do
  end function load_stars
end program main
