subroutine chol_sistu (n, a, b, u)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (kind = clreal) :: aux
	real (kind = clreal), intent (in) :: a(n, n)
	real (kind = clreal), intent (in) :: b(n)
	real (kind = clreal), intent (out) :: u(n)
	
	u(n) = b(n) / a(n, n)
	do i = n - 1, 1, -1
		u(i) = b(i)
		do j = i + 1, n
			u(i) = u(i) - a(j, i) * u(j)
		enddo
		u(i) = u(i) / a(i, i)
	enddo
	
	print*
	print*, 'Solucion:', u

end subroutine
