subroutine pivpar_sistu (n, a, b, ip, u)

	!Resolucion matriz escalonada con pivote parcial

	use mod_clreal
	
	implicit none
	
	integer :: i, j, ipi
	integer, intent(in) :: n, ip(n)
	real (kind = clreal), intent(in) :: a(n, n), b(n)
	real (kind = clreal), intent(out) :: u(n)
	real (kind = clreal) :: aux
	
	do i = n, 1, -1
		aux = 0._clreal
		ipi = ip(i)
		do j = i + 1, n
			aux = aux + a(ipi, j) * u(j)
		end do
		u(i) = (b(ipi) - aux) / a(ipi, i)
	end do
	
	print*
	print*, 'Solucion:', u

end subroutine
