subroutine choltri_sistl (n, dp, ds, b)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: dp(n), ds(n - 1)
	real (kind = clreal), intent (inout) :: b(n)
	
	b(1) = b(1) / dp(1)
	do i = 2, n
		b(i) = (b(i) - ds(i - 1) * b(i - 1)) / dp(i)
	enddo

end subroutine
