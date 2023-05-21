subroutine jacobi_lecdat	(n, a, b, u, eps, nitmax)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (inout) :: n
	integer, intent (out) :: nitmax
	real (clreal), intent (out) :: a(n, n)
	real (clreal), intent (out) :: b(n), u(n)
	real (clreal), intent (out) :: eps
	character(len=10)::formato='(100e12.4)'
	
	do i = 1, n
		read*, a(i, :)
	end do
	
	print*
	print*, 'Matriz a:'
	do i = 1, n
		if (abs(a(i, i)) < 1.e-12) then
			print*, 'Error: matriz singular'
			n = 0
			stop
		end if
	end do
	
	do i = 1, n
		print*, a(i, :)
	end do
	
	read*, b
	
	print*
	print*, 'Vector b:', b
	
	read*, u
	
	print*
	print*, 'Iterante inicial:', u
	
	read*,eps
	
	print*
	print*, 'Epsilon:', eps

	read*,nitmax
	
	print*
	print*, 'Numero maximo de iteraciones:', nitmax

end subroutine
