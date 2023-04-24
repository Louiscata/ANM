program invtl
	
	!Programa que lee una matriz triangular superior y un vector y calcula la solucion del sistema de ecuaciones
	!f95 -o invtl_ppal.out invtl_ppal.f95 invtl_lecmat.f95 invtl_cal.f95 
	!./invtl_ppal.out <invtl_dat.txt> invtl_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n, i, j
	real (clreal), allocatable, dimension (:, :) :: a, Id, ia, p
	
	read*, n
	
	allocate (a(n, n), Id(n, n), ia(n, n), p(n, n))
	
	print*
	print*,'Matriz identidad:'
	
	do i = 1, n
		do j = 1, n
			if (i == j) then
				Id(i, j) = 1
			else
				Id(i, j) = 0
			endif
		enddo
		print*, Id(i, :)
	enddo
	
	call invtl_lecmat (a, n)
	if (n == 0) then
		stop
	end if
	
	do i = 1, n
		call invtl_cal (a, Id(:, i), ia(:, i), n)
	enddo
	
	print*
	print*,'Matriz inversa:'
	do i = 1, n
		print*, ia(i, :)
	enddo
	
	p = matmul(a, ia)
	print*
	print*,'Resultado de A por su inversa:'
	do i = 1, n
		print*, p(i, :)
	enddo
	
	deallocate (a, Id, ia, p)
	
end program invtl
