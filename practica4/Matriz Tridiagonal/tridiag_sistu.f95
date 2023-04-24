subroutine tridiag_sistu (n, x, y, w, u)

	!Esta subrutina calcula la solucion del sistema (asumiendo que es triangular superior)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: x(n), y(n - 1), w(n)
	real (kind = clreal), intent (out) :: u(n)
	
	u(n) = w(n) / x(n)
	
	do i = n - 1, 1, -1
		u(i) = (w(i) - y(i) * u(i + 1)) / x(i)
	enddo
	
	print*
	print*,'Solucion: ',u

end subroutine
