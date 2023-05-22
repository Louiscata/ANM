subroutine gauss_lecdat	(n, a, b)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (inout) :: n
	real (clreal), intent (out) :: a(n, n)
	real (clreal), intent (out) :: b(n)
	
	do i = 1, n
		read*, a(i, :)
	end do
	
	print*
	print*, 'Matriz a:'
	do i = 1, n
		print*, a(i, :)
	end do
	
	read*, b
	
	print*
	print*, 'Vector b:', b

end subroutine