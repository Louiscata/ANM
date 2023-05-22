program sistu

	!Programa que soluciona una matriz por el metodo de Gauss
	!f95 -o gauss_ppal.out gauss_ppal.f95 gauss_lecdat.f95 gauss_cal.f95 gauss_sistu.f95 gauss_residuo.f95
	!./gauss_ppal.out <gauss_dat.txt> gauss_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n, i	
	real (clreal) :: deter
	real (clreal), allocatable :: a(:,:), aa(:,:), b(:), bb(:), u(:), r(:)
	real :: start, finish
	
	call cpu_time (start)
	
	read*, n
	print*, 'Tamanho de la matriz:',n
	
	allocate (a(n,n), aa(n,n), b(n), bb(n), u(n), r(n))
	
	call gauss_lecdat (n, a, b)
	if (n == 0) then
		stop
	end if
	
	aa = a
	bb = b
	
	call gauss_cal (n, a, b, deter)
	
	print*
	print*,'Determinante:',deter
	
	print*
	print*,'Matriz reducida (ojo con los factores de escalado, no estoy seguro del todo de que se guarden bien):'
	do i = 1, n
		print*, a(i, :)
	end do
	
	print*
	print*,'Terminos independientes',b
	
	call gauss_sistu (n, a, b, u)
	
	call gauss_residuo (n, n, aa, bb, u, r)
	
	deallocate (a, aa, b, bb, u, r)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program sistu
