subroutine sistu_lecvec	(b, n)

	use mod_clreal
	
	implicit none
	
	integer, intent (in) :: n
	real (clreal), intent (out) :: b(n)

	read*, b
	
	print*
	print*, 'Vector b:', b

end subroutine
