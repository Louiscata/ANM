program sumat_ppal

	!Suma de dos matrices
	!f95 -o sumat_ppal.out sumat_ppal.f95
	!./sumat_ppal.out <sumat_dat> sumat_sal

	implicit none
	
	integer :: m, n, i
	real, allocatable, dimension (:, :) :: a, b, c
	
	!print*, 'Tamano de la matriz:'
	read*, m
	read*, n
	
	allocate (a(m, n))
	allocate (b(m, n))
	allocate (c(m, n))
	
	print*, 'Numero de filas:', m
	print*, 'Numero de columnas:', n
	
	print*
	print*, 'Matriz a:'
	do i = 1, m
		read*, a(i, :)
		print*, a(i, :)
	end do
	
	print*
	print*, 'Matriz b:'
	do i = 1, m
		read*, b(i, :)
		print*, b(i, :)
	end do
	
	c = a + b
	print*
	print*, 'Resultado:'
	do i = 1, m
		print*, c(i, :)
	end do
	
	deallocate (a, b, c)
	
end program sumat_ppal
