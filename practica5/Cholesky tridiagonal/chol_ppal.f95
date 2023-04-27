program chol

	!Programa que soluciona una matriz por el metodo de Gauss LU
	!f95 -o chol_ppal.out chol_ppal.f95 chol_lecdat.f95 chol_cal.f95 chol_sistl.f95 chol_sistu.f95 chol_residuo.f95
	!./chol_ppal.out <chol_dat.txt> chol_sal.txt
	
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
	
	call chol_lecdat (n, a, b)
	if (n == 0) then
		stop
	end if
	
	aa = a
	
	call chol_cal (n, a, deter)
	
	call chol_sistl (n, a, b, y)
	
	call chol_sistu (n, a, y, x)
	
	call chol_residuo (n, n, aa, b, x, r)
	
	deallocate (a, aa, b, x, y, r)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program chol
