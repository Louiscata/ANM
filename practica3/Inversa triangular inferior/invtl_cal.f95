subroutine invtl_cal (a, b, u, n)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (clreal), intent (in) :: a(n, n)
	real (clreal), intent (in) :: b(n)
	real (clreal), intent (out) :: u(n)
	
	!Metodo 1 (NO VA)
	
	u(1) = b(1) / a(1, 1)
	
	do i = 2, n
		u(i) = b(i)
		do j = 1, i - 1
			u(i) = u(i) - a(i, j) * u(j)
		enddo
		u(i) = u(i) / a(i, i)
	enddo
	
	!Otro metodo (orientado a columnas)
	
	!u = b
	!do j = 1, n
	!	u(j) = u(j) / a(j, j)
	!	u(j + 1: n) = u(j + 1: n) - a(j + 1: n, j) * u(j)
	!end do

end subroutine
