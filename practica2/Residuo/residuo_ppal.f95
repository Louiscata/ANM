program prodmat_ppal

	!Calculo del vector residuo: r = Au - b
	!f95 -o residuo_ppal.out residuo_ppal.f95 residuo_cal.f95
	!./residuo_ppal.out <residuo_dat> residuo_sal
	
	use mod_clreal

	implicit none
	
	integer :: m, n, i	
	real (clreal), allocatable, dimension (:, :) :: a
	real (clreal), allocatable, dimension (:) :: u, b, r
	
	read*, m
	read*, n

	allocate (a(m, n), u(n), b(m), r(m))
	
	print*, 'Numero de filas de la matriz:', m
	print*, 'Numero de columnas de la matriz:', n
	
	print*
	print*, 'Matriz a:'
	do i = 1, m
		read*, a(i, :)
		print*, a(i, :)
	end do
	
	print*
	read*, u
	print*, 'Vector u:', u
	read*, b
	print*, 'Vector b:', b
	
	call residuo_sub(m, n, a, b, u, r)
	
	print*
	print*, 'Resultado:', r
	
	deallocate (a, u, b, r)
	
end program prodmat_ppal
