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
        call walk_y()
        call walk_x()
      end if
    end do
  contains
    subroutine walk_x()
      logical :: do_warp
      if (next_star%x < ship%x) then
        if (steps(nsteps-1) == 0 .and. &
            steps(nsteps-2) == 6) then
          nsteps = nsteps - 1
          steps(nsteps-1) = 5
          steps(nsteps) = 0
        else
          steps(nsteps) = 4
        end if
        ship%vx = ship%vx - 1
        nsteps = nsteps + 1
        ship%x = ship%x + ship%vx
        do
          do_warp = warp_x(4, 6, -1)
          if(.not. do_warp) exit
        end do
        do while (next_star%x < ship%x)
          ship%x = ship%x + ship%vx
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 6
        ship%vx = ship%vx + 1
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
        ship%vx = ship%vx + 1
        nsteps = nsteps + 1
        ship%x = ship%x + ship%vx
        do
          do_warp = warp_x(6, 4, +1)
          if(.not. do_warp) exit
        end do
        do while (next_star%x > ship%x)
          ship%x = ship%x + ship%vx
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 4
        ship%vx = ship%vx - 1
        nsteps = nsteps + 1
        do_walk_x = .true.
      end if
    end subroutine walk_x
    subroutine walk_y()
      logical :: do_warp
      if (next_star%y < ship%y) then
        if (steps(nsteps-1) == 0 .and. &
            steps(nsteps-2) == 8) then
          nsteps = nsteps - 1
          steps(nsteps-1) = 5
          steps(nsteps) = 0
        else
          steps(nsteps) = 2
        end if
        ship%vy = ship%vy - 1
        nsteps = nsteps + 1
        ship%y = ship%y + ship%vy
        do
          do_warp = warp_y(2, 8, -1)
          if(.not. do_warp) exit
        end do
        do while (next_star%y < ship%y)
          ship%y = ship%y + ship%vy
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 8
        ship%vy = ship%vy + 1
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
        ship%vy = ship%vy + 1
        nsteps = nsteps + 1
        ship%y = ship%y + ship%vy
        do
          do_warp = warp_y(8, 2, +1)
          if(.not. do_warp) exit
        end do
        do while (next_star%y > ship%y)
          ship%y = ship%y + ship%vy
          steps(nsteps) = 5
          nsteps = nsteps + 1
        end do
        steps(nsteps) = 2
        ship%vy = ship%vy - 1
        nsteps = nsteps + 1
        do_walk_x = .false.
      end if
    end subroutine walk_y
    logical function warp_x(acc, dec, dir)
      integer :: acc, dec, dir
      integer :: dist, i, j
      warp_x = .false.
      dist = abs(next_star%x - ship%x)
      if (dist <= 1) return
      do i = 1, size(triangle_numbers)
        if (triangle_numbers(i+1) + triangle_numbers(i) > dist) then
          exit
        end if
      end do
      i = i - 1
      if (i == 0) return
      warp_x = .true.
      do j = 1, i
        steps(nsteps) = acc
        ship%vx = ship%vx + dir
        ship%x = ship%x + ship%vx
        nsteps = nsteps + 1
      end do
      do j = 1, i
        steps(nsteps) = dec
        ship%vx = ship%vx - dir
        ship%x = ship%x + ship%vx
        nsteps = nsteps + 1
      end do
    end function warp_x
    logical function warp_y(acc, dec, dir)
      integer :: acc, dec, dir
      integer :: dist, i, j
      warp_y = .false.
      dist = abs(next_star%y - ship%y)
      if (dist <= 1) return
      do i = 1, size(triangle_numbers)
        if (triangle_numbers(i+1) + triangle_numbers(i) > dist) then
          exit
        end if
      end do
      i = i - 1
      if (i == 0) return
      warp_y = .true.
      do j = 1, i
        steps(nsteps) = acc
        ship%vy = ship%vy + dir
        ship%y = ship%y + ship%vy
        nsteps = nsteps + 1
      end do
      do j = 1, i
        steps(nsteps) = dec
        ship%vy = ship%vy - dir
        ship%y = ship%y + ship%vy
        nsteps = nsteps + 1
      end do
    end function warp_y
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
  integer, parameter :: max_steps = 100000000
  type(star_t), allocatable :: stars(:)
  type(ship_t) :: ship
  integer(1), allocatable :: steps(:)
  integer :: nsteps, task_id, i
  call init_space_m()
  allocate(steps(-1:max_steps), source = 0_1)
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
