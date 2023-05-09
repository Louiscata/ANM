subroutine choltri_residuo (n, ad, au, al, u, b, r)

	!Esta subrutina calcula el residuo

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: ad(n), au(n - 1), al(n - 1), u(n), b(n)
	real (kind = clreal), intent (out) :: r(n)
	
	r(1) = ad(1) * u(1) + au(1) * u(2) - b(1)
	
	do i = 2, n - 1
		r(i) = al(i - 1) * u(i - 1) + ad(i) * u(i) + au(i) * u(i + 1) - b(i)
	enddo
	
	r(n) = al(n - 1) * u(n - 1) + ad(n) * u(n) - b(n)
	
	print*
	print*,'Residuo: ',r

end subroutine
