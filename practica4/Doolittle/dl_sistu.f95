subroutine dl_sistu (n, a, b, u)

	!Esta subrutina calcula la solucion del sistema (asumiendo que es triangular superior)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (kind = clreal) :: aux
	real (kind = clreal), intent (in) :: a(n, n)
	real (kind = clreal), intent (in) :: b(n)
	real (kind = clreal), intent (out) :: u(n)
	
	!Un método
	
	!u(n) = b(n) / a(n, n)
	!do i = n - 1, 1, -1
	!	u(i) = b(i)
	!	do j = i + 1, n
	!		u(i) = u(i) - a(i, j) * u(j)
	!	enddo
	!	u(i) = u(i) / a(i, i)
	!enddo
	
	!Otro metodo (orientado a columnas)
	
	u = b
	do j = n, 1, -1
		u(j) = u(j) / a(j, j)
		u(1: j - 1) = u(1: j - 1) - a(1: j - 1, j) * u(j)
	end do
	
	print*
	print*, 'Solucion:', u

end subroutine
