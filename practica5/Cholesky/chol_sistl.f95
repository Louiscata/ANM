subroutine chol_sistl (n, a, b, u)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: a(n, n)
	real (kind = clreal), intent (in) :: b(n)
	real (kind = clreal), intent (out) :: u(n)
	real (kind = clreal) :: aux
	
	u(1) = b(1) / a(1, 1)
	do i = 2, n
		u(i) = b(i)
		do j = 1, i - 1
			u(i) = u(i) - a(i, j) * u(j)
		enddo
		u(i) = u(i) / a(i, i)
	enddo
	
	print*
	print*,'Vector y:',u

end subroutine
