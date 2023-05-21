program palu

	!Programa que soluciona una matriz por el metodo de la factorizacion PALU
	!f95 -o palu_ppal.out palu_ppal.f95 palu_lecdat.f95 palu_cal.f95 palu_sistl.f95 palu_sistu.f95 palu_residuo.f95
	!./palu_ppal.out > palu_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n	
	real (kind = clreal) :: deter
	integer, allocatable :: ip(:)
	real (kind = clreal), allocatable :: a(:,:), aa(:,:), b(:), x(:), y(:), r(:)
	real :: start, finish
	
	call cpu_time (start)
	
	n = 6
	print*, 'Tamanho de la matriz:',n
	
	allocate (a(n,n), aa(n,n), b(n), x(n), y(n), r(n), ip(n))
	
	call palu_lecdat (n, a, b)
	
	aa = a
	
	call palu_cal (n, a, ip, deter)
	
	call palu_sistl (n, a, b, y, ip)
	
	print*
	print*,'Vector w:',y
	
	call palu_sistu (n, a, y, x, ip)
	
	call palu_residuo (n, n, aa, b, x, r)
	
	deallocate (a, aa, b, x, y, r, ip)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program palu
