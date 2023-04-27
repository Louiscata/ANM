subroutine house_cal (n, a, b, deter)

	!Esta subrutina hace ceros en la matriz para que quede triangular
	
	use mod_clreal
	
	integer :: i, j, k
	integer, intent (in) :: n
	real (clreal), intent (inout) :: a(n, n)
	real (clreal), intent (inout) :: b(n)
	real (clreal), intent (out) :: deter
	real (clreal) :: s2, alfa, beta, p, q
	
	deter = 1.
	
	do k = 1, n - 1
		s2 = sum(a(k : n, k) * a(k : n, k))
		
		!Comprobacion de la eliminacion
		if (abs (s2) < 1.e-12) then
			print*
			print*,'Vector nulo en la etapa:', k
			print*,'La matriz es singular.'
			stop
		else if (sum(a(k + 1 : n, k) * a(k + 1 : n, k)) < 1.e-12) then
			print*,'No se precisa eliminacion en la etapa:', k
			!Actualizacion del determinante
			deter = deter * a(k, k)
			cycle
		else
			!Calculo de los elementos de la matriz de Householder
			if (a(k, k) >= 1.e-12) then
				alfa = sqrt(s2)
			else
				alfa = -sqrt(s2)
			end if
			beta = 1._clreal / (alfa * (alfa + a(k, k)))
			a(k, k) = a(k, k) + alfa
		end if
		
		!Modificacion de las columnas
		do j = k + 1, n
			q = sum(a(k : n, k) * a(k : n, j))
			p = beta * q
			a(k : n, j) = a(k : n, j) - p * a(k : n, k)
		end do
		
		!Modificacion del termino independiente
		q = sum(a(k : n, k) * b(k : n))
		p = beta * q
		b(k : n) = b(k : n) - p * a(k : n, k)
		
		!Se guarda -alfa en a(k, k) para completar la matriz triangular superior
		a(k, k) = -alfa
		
		!Actualizacion del determinante
		deter = -deter * a(k, k)
	end do
	
	!Comprobacion de que el n-esimo elemento no es nulo

	if (abs (a(n, n)) < 1.e-12) then
		print*,'Elemento diagonal',n,'en la matriz R nulo. La matriz es singular.'
		stop
	end if

	!Fin del calculo del determinante
	deter = deter * a(n,n)
	
end subroutine
