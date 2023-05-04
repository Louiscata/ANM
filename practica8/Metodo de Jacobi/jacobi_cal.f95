subroutine jacobi_cal (n, a, b, u, eps, nitmax)
	
	use mod_clreal

	integer i, iter
	real (clreal), dimension(n) :: uold
	real (clreal) :: error
	
	integer, intent (in) :: n, nitmax
	real (clreal), intent (in) :: a(n, n)
	real (clreal), intent (in) :: b(n)
	real (clreal), intent (inout) :: u(n)
	real (clreal), intent (in) :: eps
	
	do i = 1, n
		if (abs(a(i, i)) < 1.e-12) then
			print*,'adfasf'
		endif
	enddo
	
	do iter = 1, nitmax
		uold = u
		do i = 1, n
			u(i) = (b(i) - sum(a(i, 1: i - 1) * uold(1: i - 1)) - sum(a(i, i + 1: n) * uold(i + 1: n))) / a(i, i)
		end do
		
		!u = b
		
		!do j = 1, n
		!	u(1: j - 1) = u(1: j - 1) - a(1: j - 1, j) * uold(j)
		!	u(j + 1: n) = u(j + 1: n) - a(j _ 1: n, j) * uold(j)
		!end do
		
		!do j = 1, n
		!	u(j) = u(j) / a(j, j)
		!end do
		
		!error = maxval(abs(u - uold))
		!error = sum(abs(u - uold))
		error = sqrt(dot_product(abs(u - uold), abs(u - uold)))
		
		if(error < eps) then
			print*
			print*,'Error:'
			return
		endif
	enddo
	
	print* 
	print*,'Efectuadas',nitmax,'iteraciones sin que se cumpla el test de parada'
	
end subroutine
