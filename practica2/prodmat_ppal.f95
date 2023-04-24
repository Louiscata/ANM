program prodmat_ppal

	!Producto de dos matrices
	!f95 -o prodmat_ppal.out prodmat_ppal.f95
	!./prodmat_ppal.out <prodmat_dat> prodmat_sal

	implicit none
	
	integer :: m, n, p, i, j, k
	real, allocatable, dimension (:, :) :: a, b, c
	real, allocatable :: aux(:)
	
	read*, m
	read*, n
	read*, p
	
	allocate (a(m, n))
	allocate (b(n, p))
	allocate (c(m, p))
	allocate (aux(m))
	
	print*, 'Numero de filas de la primera matriz:', m
	print*, 'Numero de columnas de la primera matriz/filas de la segunda:', n
	print*, 'Numero de columnas de la segunda matriz:', p
	
	print*
	print*, 'Matriz a:'
	do i = 1, m
		read*, a(i, :)
		print*, a(i, :)
	end do
	
	print*
	print*, 'Matriz b:'
	do i = 1, n
		read*, b(i, :)
		print*, b(i, :)
	end do
	
	!Con matmul
	c = matmul (a, b)
	print*
	print*, 'Resultado con matmul():'
	do i = 1, m
		print*, c(i, :)
	end do
	
	!Con bucle usando las columnas de la primera matriz
	do j = 1, p
		aux = 0.
			do k = 1, n
				aux = aux + a(:, k) * b(k, j)
			end do
		c(:, j) = aux
	end do
	print*
	print*, 'Resultado con bucles sobre las columnas de A:'
	do i = 1, m
		print*, c(i, :)
	end do
	
	deallocate (a, b, c)
	
end program prodmat_ppal
