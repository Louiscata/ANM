subroutine choltri_cal (n, dp, ds, deter)
	
	use mod_clreal
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (inout) :: dp(n), ds(n - 1)
	real (kind = clreal), intent (out) :: deter
	
	deter = 1._clreal
	
	do i = 1, n
		deter = deter * dp(i)
	enddo
	
	dp(1) = sqrt(dp(1))
	
	do i = 2, n
		ds(i - 1) = ds(i - 1) / dp(i - 1)
		dp(i) = sqrt(dp(i) - ds(i - 1) ** 2)
	end do
	
end subroutine
