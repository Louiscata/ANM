subroutine chol_residuo (n, m, a, b, u, r)

	!Esta subrutina calcula el residuo

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: m, n
	real (kind = clreal), intent (in) :: a(m, n), b(m), u(n)
	real (kind = clreal), intent (out) :: r(m)
	
	do i = 1, m
		r(i) = dot_product(a(i, :), u) - b(i)
	end do
	
	print*
	print*,'Residuo: ',r

end subroutine
