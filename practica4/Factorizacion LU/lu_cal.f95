subroutine lu_cal (n, a, deter)

	!Esta subrutina hace ceros en la matriz para que quede triangular
	
	use mod_clreal
	
	integer :: i, j, k
	real (kind = clreal) :: piv
	integer, intent (in) :: n
	real (kind = clreal), intent (inout) :: a(n, n)
	real (kind = clreal), intent (out) :: deter
	
	deter = 1.
	
	do k = 1, n - 1
		piv = a(k, k)
		
		if (abs (piv) < 1.e-12) then
			print*,'Pivote nulo en la etapa:',k
			stop
		end if
		
		deter = deter * piv
		
		!Metodo 1 para la eliminacion
		!do i = k + 1, n
		!	a(i, k) = a(i, k) / piv
		!	do j = k + 1, n
		!		a(i, j) = a(i, j) - a(i, k) * a(k, j)
		!	end do
		!end do
		
		!Metodo 2 para la eliminacion, mas rapido
		a (k + 1 : n, k) = a(k + 1 : n, k) / piv
		do j = k + 1, n
			a(k + 1 : n, j) = a(k + 1 : n, j) - a(k + 1 : n, k) * a(k, j)
		end do
	end do

	if (abs (a(n, n)) < 1.e-12) then
		print*,'Pivote nulo en la etapa:',n
		stop
	end if

	deter = deter * a(n,n)
	
	print*
	print*,'Determinante:',deter
	
end subroutine
