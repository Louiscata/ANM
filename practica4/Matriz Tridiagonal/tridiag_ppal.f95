program tridiag

	!Programa que soluciona un s.e.l. con una matriz tridiagonal
	!f95 -o tridiag_ppal.out tridiag_ppal.f95 tridiag_lecdat.f95 tridiag_cal.f95 tridiag_sistl.f95 tridiag_sistu.f95 tridiag_residuo.f95
	!./tridiag_ppal.out <tridiag_dat.txt> tridiag_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n	
	real (kind = clreal) :: deter
	real (kind = clreal), allocatable :: ad(:), au(:), al(:), b(:), x(:), y(:), z(:), w(:), u(:), r(:)
	real :: start, finish
	
	call cpu_time (start)
	
	read*, n
	print*, 'Tamanho de la matriz:',n
	
	allocate (ad(n), au(n - 1), al(n - 1), b(n), x(n), y(n - 1), z(n - 1), w(n), u(n), r(n))
	
	call tridiag_lecdat (n, ad, au, al, b)
	if (n == 0) then
		stop
	end if
	
	call tridiag_cal (n, ad, au, al, x, y, z)
	
	call tridiag_sistl (n, z, b, w)
	
	call tridiag_sistu (n, x, y, w, u)
	
	call tridiag_residuo (n, ad, au, al, u, b, r)
	
	deallocate (ad, au, al, b, x, y, z, w, u, r)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program tridiag
