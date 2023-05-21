subroutine palu_cal (n, a, ip, deter)

	!Esta subrutina hace ceros en la matriz para que quede triangular, con pivote parcial
	
	use mod_clreal
	
	integer :: i, j, k, ipi, ipk, ipiv, cont
	integer, intent (in) :: n
	integer, intent (out) :: ip(n)
	real (kind = clreal), intent (inout) :: a(n, n)
	real (kind = clreal), intent (out) :: deter
	real (kind = clreal) :: piv
	
	!inicializacion del determinante
	deter = 1.
	
	!inicializacion de la permutacion de filas (como si fuese un for())
	ip = (/(i, i = 1, n)/)
	
	!inicializacion do contador de cambios de filas
	cont = 0
	
	!etapa k-esima de la eliminacion
	do k = 1, n - 1
	
		!busqueda del pivote y de la fila en la que se encuentra
		piv = a(ip(k), k)
		ipiv = k
		do i = k + 1, n
			if(abs (piv) < abs (a(ip(i), k))) then
				piv = a(ip(i), k)
				ipiv = i
			end if
		end do
		
		!comprobacion de que el k-esimo pivote no es nulo
		if(abs (piv) < 1.e-12) then
			print*
			print*,'Pivote nulo en la etapa:',k
			print*,'La matriz del sistema es singular'
			stop
		end if
		
		!actualizacion del determinante
		deter = deter * piv
		
		!puesta al dia de la permutacion y del contador de cambios de filas, si el pivote no esta en la fila k
		if (ipiv /= k) then
			ipk = ip(ipiv)
			ip(ipiv) = ip(k)
			ip(k) = ipk
			cont = cont + 1
		else
			ipk = ip(k)
		end if
		
		!eliminacion
		do i = k + 1, n
			ipi = ip(i)
			a(ipi, k) = a(ipi, k) / piv
			do j = k + 1, n
				a(ipi, j) = a(ipi, j) - a(ipi, k) * a(ipk, j)
			end do
		end do
		
		print*
		print*,'Etapa',k,'de la eliminacion'
		print*,'Permutacion de filas:',ip
	end do
	
	!comprobacion de que el ultimo pivote no es nulo
	piv = a(ip(n), n)
	if(abs (piv) < 1.e-12) then
		print*
		print*,'Pivote nulo en la etapa:',n
		print*,'La matriz del sistema es singular'
		stop
	end if
	
	!fin del calculo del determinante
	deter = deter * piv * (-1) ** cont
	
	print*
	print*,'Determinante:',deter
	
	print*
	print*,'Cambios de filas totales:',cont
	
	print*
	print*,'Permutación de filas',ip
	
	print*
	print*,'Matrices L y U:'
	do i = 1, n
		print*, a(i, :)
	end do
	
end subroutine
