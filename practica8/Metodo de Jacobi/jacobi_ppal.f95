program jacobi

	!f95 -o jacobi_ppal.out jacobi_ppal.f95 jacobi_lecdat.f95 jacobi_cal.f95 jacobi_residuo.f95
	!./jacobi_ppal.out <jacobi_dat.txt> jacobi_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n, i	
	real (clreal) :: deter
	real (clreal), allocatable :: a(:,:), b(:), u(:), r(:)
	real :: start, finish
	
	call cpu_time (start)
	
	read*, n
	print*, 'Tamanho de la matriz:',n
	
	allocate (a(n,n), b(n), u(n), r(n))
	
	!Lectura de datos
	call jacobi_lecdat (n, a, b)
	
	if (n == 0) then
		stop
	end if
	
	!Calculo de la solucion
	call jacobi_cal (n, a, b, deter)
	
	print*
	print*,'Ultimo iterante calculado',u
	
	!Residuo
	call jacobi_residuo (n, n, a, b, u, r)
	
	deallocate (a, b, u, r)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program jacobi
