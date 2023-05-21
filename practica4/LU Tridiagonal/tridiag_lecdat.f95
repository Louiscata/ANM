subroutine tridiag_lecdat	(n, ad, au, al, b)

	use mod_clreal
	
	implicit none
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (out) :: ad(n), au(n - 1), al(n - 1), b(n)
	
	read*, au
	
	print*
	print*, 'Vector au:', au
	
	read*, ad
	
	print*, 'Vector ad:', ad

	read*, al
	
	print*, 'Vector al:', al

	read*, b
	
	print*
	print*, 'Vector b:', b


end subroutine
