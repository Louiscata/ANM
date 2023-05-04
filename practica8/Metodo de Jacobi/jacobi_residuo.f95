subroutine jacobi_residuo (m, n, a, b, u, r)

	!Esta subrutina calcula el residuo

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: m, n
	real (clreal), intent (in) :: a(m, n), b(m), u(n)
	real (clreal), intent (out) :: r(m)
	
	do i = 1, m
		r(i) = dot_product(a(i, :), u) - b(i)
	end do
	
	print*
	print*,'Residuo: ',r

end subroutine
