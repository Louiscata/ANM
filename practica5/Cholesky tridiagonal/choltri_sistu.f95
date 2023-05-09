subroutine choltri_sistu (n, a, b, u)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (kind = clreal) :: aux
	real (kind = clreal), intent (in) :: a(n, n)
	real (kind = clreal), intent (in) :: b(n)
	real (kind = clreal), intent (out) :: u(n)
	
	b(n) = b(n) / dp(n)
	do i = 1, n
		b(i) = (b(i) - ds(i) * b(i + 1)) / dp(i)
	enddo

end subroutine
