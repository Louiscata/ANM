subroutine tridiag_cal (n, ad, au, al, x, y, z)
	
	use mod_clreal
	
	integer :: i
	integer, intent (in) :: n
	real (kind = clreal), intent (in) :: ad(n), au(n - 1), al(n - 1)
	real (kind = clreal), intent (out) :: x(n), y(n - 1), z(n - 1)
	real (kind = clreal) :: deter
	
	deter = 1. !Pendiente hacer el determinante
	
	do i = 1, n - 1
		y(i) = au(i);
	enddo
	
	x(1) = ad(1)
	
	do i = 1, n - 1
	
		if (abs (x(i)) < 1.e-12) then
			print*
			print*,'Atencion, elemento diagonal',x(i),'en la fila:',i
			if (i < n) then
				print*,'No existe la factorización A = LU'
				stop
			else
				print*,'La matriz U es singular'
				stop
			endif
		endif
		
		z(i) = al(i) / x(i)
		x(i + 1) = ad(i + 1) - z(i) * y(i)
		deter = deter * x(i)
	enddo
	
	
	if (abs (x(n)) < 1.e-12) then
		print*
		print*,'Atencion, elemento diagonal',x(n),'en la fila:',n
		if (i < n) then
			print*,'No existe la factorización A = LU'
			stop
		else
			print*,'La matriz U es singular'
			stop
		endif
	endif
		
	
	deter = deter * x(n)
	
	print*
	print*,'Determinante:',deter
	
	print*
	print*,'La superdiagonal de U es:', y
	print*,'La diagonal de U es:', x
	print*,'La subdiagonal de L es:', z
	
end subroutine
