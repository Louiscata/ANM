subroutine tridiag_sistl (n, z, b, w)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: z(n - 1), b(n)
	real (kind = clreal), intent (out) :: w(n)
	
	w(1) = b(1)
	
	do i = 2, n
		w(i) = b(i) - z(i - 1) * w(i - 1)
	enddo

end subroutine
