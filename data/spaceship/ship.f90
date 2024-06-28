module space_m
  implicit none
  type :: ship_t
    integer :: x
    integer :: y
    integer :: vx
    integer :: vy
  contains
    procedure :: travel
  end type
  type :: star_t
    integer :: x
    integer :: y
    integer :: visited
  end type
contains
  recursive subroutine travel(ship, stars, steps, current_step, max_steps)
    class(ship_t), intent(inout) :: ship
    type(star_t) :: stars(:)
    integer(1) :: steps(:)
    integer, intent(in) :: current_step, max_steps
    ! local variables
    type(ship_t) :: ship_copy
    integer(1) :: ipos
    integer :: istar
    integer :: dist, star_id, tdist
    ! fast exit
    if (current_step > max_steps) return
    ! choose impulse
    select case(steps(current_step))
      case(1)
        ship%vx = ship%vx - 1
        ship%vy = ship%vy - 1
      case(2)
        ship%vy = ship%vy - 1
      case(3)
        ship%vx = ship%vx + 1
        ship%vy = ship%vy - 1
      case(4)
        ship%vx = ship%vx - 1
      case(5)
      case(6)
        ship%vx = ship%vx + 1
      case(7)
        ship%vx = ship%vx - 1
        ship%vy = ship%vy + 1
      case(8)
        ship%vy = ship%vy + 1
      case(9)
        ship%vx = ship%vx + 1
        ship%vy = ship%vy + 1
      case default
        error stop "HUITA"
    end select
    ! find closest star in direction
    dist = 2**30
    star_id = -1
    do istar = 1, size(stars)
      if (ship%x + ship%vx == stars(istar)%x .and. &
          ship%y + ship%vy == stars(istar)%y) then
        star_id = istar
        exit
      end if
    end do
    ! setup closest star
    if (star_id < 0) return
    stars(star_id)%visited = stars(star_id)%visited + 1
    ship%x = stars(star_id)%x
    ship%y = stars(star_id)%y
    ! if done, print solution to stdout
    if (all(stars%visited > 0)) then
      write(6, "(I0,': ',10000000I0)") current_step, steps(1:current_step)
    end if
    ! recursion!!!
    do ipos = 1, 9
      ship_copy = ship
      steps(current_step + 1) = ipos
      call ship_copy%travel(stars, steps, current_step + 1, max_steps)
    end do
    ! roll-back of visiting
    stars(star_id)%visited = stars(star_id)%visited - 1
  end subroutine travel
end module space_m

program main
  use space_m
  implicit none
  integer, parameter :: max_steps = 100000
  type(ship_t) :: ship, ship_copy
  type(star_t), allocatable :: stars(:)
  integer(1), allocatable :: steps(:)
  integer(1) :: ipos
  ship%x = 0
  ship%y = 0
  ship%vx = 0
  ship%vy = 0
  stars = load_stars(3)
  allocate(steps(max_steps), source = 0_1)
  do ipos = 1, 9
    ship_copy = ship
    steps(1) = ipos
    call ship_copy%travel(stars, steps, 1, 10)
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
      stars(i)%visited = 0
      if (stars(i)%x == 0 .and. stars(i)%y == 0) stars%visited = 1
    end do
  end function load_stars
end program main
