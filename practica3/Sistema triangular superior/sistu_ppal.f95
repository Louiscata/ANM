program sistu

	!Programa que lee una matriz triangular superior y un vector y calcula la solucion del sistema de ecuaciones
	!f95 -o sistu_ppal.out sistu_ppal.f95 sistu_lecmat.f95 sistu_lecvec.f95 sistu_cal.f95 
	!./sistu_ppal.out <sistu_dat.txt> sistu_sal.txt
	
	use mod_clreal

	implicit none
	
	integer :: n	
	real (clreal), allocatable, dimension (:, :) :: a
	real (clreal), allocatable, dimension (:) :: b, u
	
	read*, n
	
	allocate (a(n, n), b(n), u(n))
	
	call sistu_lecmat (a, n)
	if (n == 0) then
		stop
	end if
	call sistu_lecvec (b, n)
	
	call sistu_cal (a, b, u, n)
	
	deallocate (a, b, u)
	
end program sistu
