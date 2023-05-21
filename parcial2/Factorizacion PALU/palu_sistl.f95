subroutine palu_sistl (n, a, b, u, ip)

	use mod_clreal
	
	implicit none
	
	integer :: i, j, ipi
	real (kind = clreal) :: aux
	
	integer, intent (in) :: n, ip(n)
	real (kind = clreal), intent (in) :: a(n, n)
	real (kind = clreal), intent (in) :: b(n)
	real (kind = clreal), intent (out) :: u(n)
	
	u(1) = b(ip(1))
	do i = 2, n
		aux = 0._clreal
		ipi = ip(i)
		do j = 1, i - 1
			aux = aux + a(ipi, j) * u(j)
		enddo
		u(i) = b(ipi) - aux
	enddo
	
	!Otro metodo (orientado a columnas) (no esta actualizado para usar ip, probablemente no vaya)
	
	!u = b
	!do i = 1, n
	!	u (i + 1 : n) = u(i + 1 : n) - a(i + 1 : n, i) * u(i)
	!end do

end subroutine
