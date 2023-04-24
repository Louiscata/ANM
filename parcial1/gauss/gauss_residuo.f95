subroutine gauss_residuo (m, n, a, b, u, r)

	!Esta subrutina calcula el residuo

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: m, n
	real (clreal), intent (in) :: a(m, n), b(m), u(n)
	real (clreal), intent (out) :: r(m)
	real (clreal) :: max
	
	do i = 1, m
		r(i) = dot_product(a(i, :), u) - b(i)
	end do
	
	max = r(1)
	do i = 2, n
		if (r(i) > max) then
			max = r(i)
		endif
	enddo
	
	print*
	print*,'Residuo: ',r
	
	print*
	print*,'Norma infinito de r:',max

end subroutine
