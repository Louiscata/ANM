subroutine dl_sistl (n, a, b, u)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: a(n, n)
	real (kind = clreal), intent (in) :: b(n)
	real (kind = clreal), intent (out) :: u(n)
	
	!u(1) = b(1)
	!do i = 2, n
	!	u(i) = b(i)
	!	do j = 1, i - 1
	!		u(i) = u(i) - a(i, j) * u(j)
	!	enddo
	!enddo
	
	!Otro metodo (orientado a columnas)
	
	u = b
	do i = 1, n
		u (i + 1 : n) = u(i + 1 : n) - a(i + 1 : n, i) * u(i)
	end do

end subroutine
