program lu

	!Programa que soluciona una matriz por el metodo de Gauss LU
	!f95 -o lu_ppal.out lu_ppal.f95 lu_lecdat.f95 lu_cal.f95 lu_sistl.f95 lu_sistu.f95 lu_residuo.f95
	!./lu_ppal.out <lu_dat.txt> lu_sal.txt
	
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
	
	call lu_lecdat (n, a, b)
	if (n == 0) then
		stop
	end if
	
	aa = a
	
	call lu_cal (n, a, deter)
	
	call lu_sistl (n, a, b, y)
	
	call lu_sistu (n, a, y, x)
	
	call lu_residuo (n, n, aa, b, x, r)
	
	deallocate (a, aa, b, x, y, r)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program lu
