program choltri

	!f95 -o choltri_ppal.out choltri_ppal.f95 choltri_lecdat.f95 choltri_cal.f95 choltri_sistl.f95 choltri_sistu.f95 choltri_residuo.f95
	!./choltri_ppal.out <choltri_dat.txt> choltri_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n	
	real (kind = clreal) :: deter
	real (kind = clreal), allocatable :: dp(:), ds(:), b(:), r(:), ddp(:), dds(:), bb(:)
	real :: start, finish
	
	call cpu_time (start)
	
	read*, n
	print*, 'Tamanho de la matriz:',n
	
	allocate (dp(n), ds(n - 1), b(n), r(n), ddp(n), dds(n - 1), bb(n))
	
	call choltri_lecdat (n, dp, ds, b)
	if (n == 0) then
		stop
	end if
	
	ddp = dp
	dds = ds
	bb = b
	
	call choltri_cal (n, dp, ds, deter)
	
	call choltri_sistl (n, dp, ds, b)
	
	call choltri_sistu (n, dp, ds, b)
	
	print*
	print*,'Determinante:',deter
	
	print*
	print*, 'Solucion:', b
	
	call choltri_residuo (n, ddp, dds, dds, b, bb, r)
	
	deallocate (dp, ds, b, r, ddp, dds, bb)
	
	call cpu_time (finish)
	
	print*
	print*, 'Tiempo total de calculo: ',finish - start
	
end program choltri
