program matriz_ppal

	!Lectura y escritura de una matriz
	!f95 -o matriz_ppal.out matriz_ppal.f95
	!./matriz_ppal.out <matriz_dat> matriz_sal

	implicit none
	
	integer :: m, n, i
	real, allocatable, dimension (:, :) :: a
	
	!print*, 'Tamano de la matriz:'
	read*, m
	read*, n
	
	allocate (a(m, n))
	
	print*, 'Numero de filas:', m
	print*, 'Numero de columnas:', n
	
	print*, 'Matriz a:'
	do i = 1, m
		read*, a(i, :)
		print*, a(i, :)
	end do
	
	!esto escribe todos los datos seguidos, como si fuese un vector, pues en la memoria se guardan los elementos todos seguidos por columnas
	!print*, a
	
	deallocate (a)
	
end program matriz_ppal
