subroutine inv_cal (a, b, u, n, deter)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (clreal), intent (in) :: a(n, n)
	real (clreal), intent (in) :: b(n)
	real (clreal), intent (out) :: u(n)
	real (clreal), intent (out) :: deter
	real (clreal) :: aa(n, n)
	
	aa = a
	
	call inv_gauss (n, aa, b, deter)
	
	!Un método
	
	u(n) = b(n) / aa(n, n)
	do i = n - 1, 1, -1
		u(i) = b(i)
		do j = i + 1, n
			u(i) = u(i) - aa(i, j) * u(j)
		enddo
		u(i) = u(i) / aa(i, i)
	enddo
	
	!Otro metodo (orientado a columnas)
	
	!u = b
	!do j = n, 1, -1
	!	u(j) = u(j) / aa(j, j)
	!	u(1: j - 1) = u(1: j - 1) - aa(1: j - 1, j) * u(j)
	!end do

end subroutine
