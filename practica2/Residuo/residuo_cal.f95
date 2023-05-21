subroutine residuo_sub 	(m, n, a, b, u, r)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: m, n
	real (clreal), intent (in) :: a(m, n), b(m), u(n)
	real (clreal), intent (out) :: r(m)
	real :: start, finish
	
	call cpu_time (start)
	do i = 1, m
		r(i) = dot_product(a(i, :), u) - b(i)
	end do
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start

end subroutine
