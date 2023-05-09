subroutine choltri_sistl (n, a, b, u)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: a(n, n)
	real (kind = clreal), intent (in) :: b(n)
	real (kind = clreal), intent (out) :: u(n)
	real (kind = clreal) :: aux
	
	b(1) = b(1) / dp(1)
	do i = 2, n
		b(i) = (b(i) - ds(i - 1) * b(i - 1)) / dp(i)
	enddo

end subroutine
