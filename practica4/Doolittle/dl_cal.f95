subroutine dl_cal (n, a, deter)

	!Esta subrutina hace ceros en la matriz para que quede triangular
	
	use mod_clreal
	
	integer :: i, j, k
	real (kind = clreal) :: piv, aux
	integer, intent (in) :: n
	real (kind = clreal), intent (inout) :: a(n, n)
	real (kind = clreal), intent (out) :: deter
	
	deter = 1.
	
	do i = 1, n

		do j = i, n
			aux = 0.
			
			do k = 1, i - 1
				aux = aux + a(i, k) * a(k, j)
			end do
			
			a(i, j) = a(i, j) - aux
		end do
			
		if(abs (a(i, i)) < 1.e-12) then
			print*,'O bien no existe factorizacion posible o bien A es singular. Fallo en la fila ', i
			stop
		end if
		    
		deter = deter * a(i, i)

		do j = i + 1, n
			aux = 0.
			do k = 1, i - 1
				aux = aux + a(j, k) * a(k, i)
			enddo
			a(j, i) = a(j, i) - aux
			a(j, i) = a(j, i) / a(i, i)
		enddo
	end do
	
	print*
	print*,'Determinante:',deter
	
end subroutine
