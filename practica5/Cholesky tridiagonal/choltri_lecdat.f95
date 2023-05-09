subroutine choltri_lecdat	(n, dp, ds, b)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (inout) :: n
	real (kind = clreal), intent (out) :: dp(n), ds(n - 1), b(n)
	
	read*, dp
	
	print*
	print*, 'Diagonal principal:', dp
	
	read*, ds
	
	print*
	print*, 'Diagonal secundaria:', ds
	
	read*, b
	
	print*
	print*, 'Vector b:', b

end subroutine
