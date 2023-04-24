subroutine sistu_cal (a, b, u, n)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (clreal), intent (in) :: a(n, n)
	real (clreal), intent (in) :: b(n)
	real (clreal), intent (out) :: u(n)
	
	u(n) = b(n) / a(n, n)
	
	do i = n - 1, 1, -1
		u(i) = b(i)
		do j = i + 1, n
			u(i) = u(i) - a(i, j) * u(j)
		enddo
		u(i) = u(i) / a(i, i)
	enddo
	
	print*
	print*, 'Solucion metodo 1:', u
	
	!Otro metodo (orientado a columnas)
	
	u = b
	do j = n, 1, -1
		u(j) = u(j) / a(j, j)
		u(1: j - 1) = u(1: j - 1) - a(1: j - 1, j) * u(j)
	end do
	
	print*
	print*, 'Solucion metodo 2:', u

end subroutine
