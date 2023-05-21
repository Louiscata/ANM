subroutine palu_lecdat	(n, a, b)

	use mod_clreal
	
	implicit none
	
	integer :: i, j
	integer, intent (inout) :: n
	real (kind = clreal), intent (out) :: a(n, n)
	real (kind = clreal), intent (out) :: b(n)
	
	do i = 1, n
		do j = 1, n
			if (i <= j) then
				a(i, j) = j - i + 1._clreal
			else
				a(i, j) = j - i
			endif
		enddo
	enddo
	
	print*
	print*, 'Matriz a:'
	do i = 1, n
		print*, a(i, :)
	end do
	
	do i = 1, n
		b(i) = n
	enddo
	
	print*
	print*, 'Vector b:', b

end subroutine
