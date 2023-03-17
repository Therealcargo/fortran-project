program mandelbrot
  implicit none

  integer, parameter :: nx = 800, ny = 800
  integer :: i, j, k
  real :: xmin = -2.0, xmax = 1.0, ymin = -1.5, ymax = 1.5
  real :: dx, dy, x, y, zx, zy, cx, cy, magz
  integer, parameter :: max_iter = 2000
  integer :: iter(nx, ny)
  real :: r(nx, ny), g(nx, ny), b(nx, ny)
  character(len=50) :: filename
  integer :: status

  ! Calculate step sizes in x and y directions
  dx = (xmax - xmin) / (nx - 1)
  dy = (ymax - ymin) / (ny - 1)

  ! Loop over each pixel in the image
  do j = 1, ny
    do i = 1, nx
      x = xmin + (i-1) * dx
      y = ymin + (j-1) * dy

      ! Initialize the complex number c
      cx = x
      cy = y

      ! Iterate the Mandelbrot formula
      zx = 0.0
      zy = 0.0
      magz = 0.0
      k = 0
      do while (magz < 2.0 .and. k < max_iter)
        zx = zx**2 - zy**2 + cx
        zy = 2.0 * zx * zy + cy
        magz = sqrt(zx**2 + zy**2)
        k = k + 1
      end do

      ! Save the number of iterations to an array
      iter(i,j) = k

      ! Map the number of iterations to RGB values
      r(i,j) = 0.5 * (1.0 + sin(0.1*k))
      g(i,j) = 0.5 * (1.0 + sin(0.2*k))
      b(i,j) = 0.5 * (1.0 + sin(0.3*k))
    end do
  end do

  ! Save the image to a file
  write(filename, "(a)") "mandelbrot.png"
  call png_write_rgb(filename, r, g, b, nx, ny, status)
  if (status /= 0) then
    write(*,*) "Error saving image"
  else
    write(*,*) "Image saved to file: ", filename
  end if

contains

  subroutine png_write_rgb(filename, r, g, b, nx, ny, status)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(in) :: nx, ny
    integer, intent(out) :: status
    real, intent(in) :: r(nx, ny), g(nx, ny), b(nx, ny)
    integer, dimension
