program householder

	!Programa que soluciona una matriz por el metodo de Gauss
	!f95 -o house_ppal.out house_ppal.f95 house_lecdat.f95 house_cal.f95 house_sistu.f95 house_residuo.f95
	!./house_ppal.out <house_dat.txt> house_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n, i, j
	real (clreal) :: deter
	real (clreal), allocatable :: a(:,:), aa(:,:), b(:), bb(:), u(:), r(:)
	real :: start, finish
	
	call cpu_time (start)
	
	read*, n
	print*, 'Tamanho de la matriz:',n
	
	allocate (a(n,n), aa(n,n), b(n), bb(n), u(n), r(n))
	
	call house_lecdat (n, a, b)
	if (n == 0) then
		stop
	end if
	
	aa = a
	bb = b
	
	call house_cal (n, a, b, deter)
	
	call house_sistu (n, a, b, u)
	
	call house_residuo (n, n, aa, bb, u, r)
	
	print*
	print*,'Determinante:',deter
	
	print*
	print*,'La matriz triangular superior R es:'
	do i = 1, n
		do j = 1, n
			if (i > j) then
				a(i, j) = 0
			end if
		end do
		print*, a(i, :)
	end do
	
	print*
	print*,'Termino independiente transformado:',b
	
	print*
	print*, 'Solucion:', u
	
	print*
	print*,'Residuo: ',r
	
	deallocate (a, aa, b, bb, u, r)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program householder
