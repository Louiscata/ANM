subroutine choltri_sistu (n, dp, ds, b)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: dp(n), ds(n - 1)
	real (kind = clreal), intent (inout) :: b(n)
	
	b(n) = b(n) / dp(n)
	do i = n - 1, 1, -1
		b(i) = (b(i) - ds(i) * b(i + 1)) / dp(i)
	enddo

end subroutine
