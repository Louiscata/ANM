subroutine chol_cal (n, a, deter)
	
	use mod_clreal
	
	integer :: i, j
	integer, intent (in) :: n
	real (kind = clreal), intent (inout) :: a(n, n)
	real (kind = clreal), intent (out) :: deter
	
	deter = 1.
	
	do j = 1, n
		!elemento diagonal de B
		a(j,j) = a(j,j) - sum(a(j,1:j-1) * a(j,1:j-1))
		if (a(j,j) < 1.e-12) then
			print*,'Radicando ',j,' <= 1.e-12, en la matriz B'
			print*,'La matriz del sistema no es definida positiva!'
			stop
		end if
		
		a(j,j) = sqrt(a(j,j))
		!bucle de filas
		do i = j + 1, n
			a(i,j) = a(i,j) - sum(a(i,1:j-1) * a(j,1:j-1))
			a(i,j) = a(i,j) / a(j,j)
		end do
		
		!actualizacion del determinante
		deter = deter * a(j,j)
	end do
	
	print*
	print*,'Matriz B'
	do i = 1, n
		print*, a(i, :)
	end do
	
	print*
	print*,'Determinante:',deter
	
end subroutine
