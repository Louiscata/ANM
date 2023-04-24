program dl

	!Programa que soluciona una matriz por el metodo de Gauss LU
	!f95 -o dl_ppal.out dl_ppal.f95 dl_lecdat.f95 dl_cal.f95 dl_sistl.f95 dl_sistu.f95 dl_residuo.f95
	!./dl_ppal.out <dl_dat.txt> dl_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n	
	real (kind = clreal) :: deter
	real (kind = clreal), allocatable :: a(:,:), aa(:,:), b(:), x(:), y(:), r(:)
	real :: start, finish
	
	call cpu_time (start)
	
	read*, n
	print*, 'Tamanho de la matriz:',n
	
	allocate (a(n,n), aa(n,n), b(n), x(n), y(n), r(n))
	
	call dl_lecdat (n, a, b)
	if (n == 0) then
		stop
	end if
	
	aa = a
	
	call dl_cal (n, a, deter)
	
	call dl_sistl (n, a, b, y)
	
	call dl_sistu (n, a, y, x)
	
	call dl_residuo (n, n, aa, b, x, r)
	
	deallocate (a, aa, b, x, y, r)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program dl
