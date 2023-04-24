program pivpar

	!Programa que soluciona una matriz por el metodo de Gauss
	!f95 -o pivpar_ppal.out pivpar_ppal.f95 pivpar_lecdat.f95 pivpar_cal.f95 pivpar_sistu.f95 pivpar_residuo.f95
	!./pivpar_ppal.out <pivpar_dat.txt> pivpar_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n	
	real (kind = clreal) :: deter
	integer, allocatable :: ip(:)
	real (kind = clreal), allocatable :: a(:,:), aa(:,:), b(:), bb(:), u(:), r(:)
	real :: start, finish
	
	call cpu_time (start)
	
	read*, n
	print*, 'Tamanho de la matriz:',n
	
	allocate (a(n,n), aa(n,n), b(n), bb(n), u(n), r(n), ip(n))
	
	call pivpar_lecdat (n, a, b)
	if (n == 0) then
		stop
	end if
	
	aa = a
	bb = b
	
	call pivpar_cal (n, a, b, ip, deter)
	
	call pivpar_sistu (n, a, b, ip, u)
	
	call pivpar_residuo (n, n, aa, bb, u, r)
	
	deallocate (a, aa, b, bb, u, r, ip)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program pivpar
