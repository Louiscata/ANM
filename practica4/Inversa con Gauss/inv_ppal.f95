program inv

	!Programa que lee una matriz triangular superior y un vector y calcula la solucion del sistema de ecuaciones
	!f95 -o inv_ppal.out inv_ppal.f95 inv_lecmat.f95 inv_cal.f95 inv_gauss.f95
	!./inv_ppal.out <inv_dat.txt> inv_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n, i, j
	real (clreal), allocatable, dimension (:, :) :: a, Id, ia, p
	real (clreal) :: deter
	
	read*, n
	
	allocate (a(n, n), Id(n, n), ia(n, n), p(n, n))
	
	print*
	print*,'Matriz identidad:'
	
	do i = 1, n
		do j = 1, n
			if (i == j) then
				Id(i, j) = 1.
			else
				Id(i, j) = 0.
			endif
		enddo
		print*, Id(i, :)
	enddo
	
	call inv_lecmat (a, n)
	if (n == 0) then
		stop
	end if
	
	do i = 1, n
		call inv_cal (a, Id(:, i), ia(:, i), n, deter)
	enddo
	
	print*
	print*,'Determinante de A:',deter
	
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
	
end program inv
