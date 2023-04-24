subroutine inv_gauss (n, a, b, deter)

	!Esta subrutina hace ceros en la matriz para que quede triangular
	
	use mod_clreal
	
	integer, intent (in) :: n
	real (clreal), intent (inout) :: a(n, n)
	real (clreal), intent (inout) :: b(n)
	real (clreal), intent (out) :: deter
	real (clreal) :: piv, factor
	
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
		!	factor = a(i, k) / piv
		!	do j = k + 1, n
		!		a(i, j) = a(i, j) - factor * a(k, j)
		!	end do
		!		b(i) = b(i) - factor * b(k)
		!end do
		
		!Metodo 2 para la eliminacion, mas rapido
		a (k + 1 : n, k) = a(k + 1 : n, k) / piv
		do j = k + 1, n
			a(k + 1 : n, j) = a(k + 1 : n, j) - a(k + 1 : n, k) * a(k, j)
		end do
		b(k + 1 : n) = b(k + 1 : n) - a(k + 1 : n, k) * b(k)
	end do

	if (abs (a(n, n)) < 1.e-12) then
		print*,'Pivote nulo en la etapa:',n
		stop
	end if

	deter = deter * a(n, n)
	
end subroutine
