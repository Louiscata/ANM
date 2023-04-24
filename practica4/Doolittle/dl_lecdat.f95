subroutine dl_lecdat	(n, a, b)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (inout) :: n
	real (kind = clreal), intent (out) :: a(n, n)
	real (kind = clreal), intent (out) :: b(n)
	
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

end subroutine
