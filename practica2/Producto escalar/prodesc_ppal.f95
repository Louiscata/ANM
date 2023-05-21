program prodesc_ppal

	!Producto escalar de dos vectores de tres maneras: con un bucle, con sum() y con dot_product()
	!f95 -o prodesc_ppal.out prodesc_ppal.f95
	!./prodesc_ppal.out <prodesc_dat> prodesc_sal

	implicit none
	
	integer :: n, i
	real, allocatable, dimension (:) :: u, v
	real :: prodesc
	
	!print*, 'Tamano de los vectores:'
	read*, n
	
	allocate (u(n), v(n))
	
	!print*, 'u:'
	read*, u
	!print*, 'v:'
	read*, v
	
	print*,'u:',u
	print*,'v:',v
	
	prodesc = 0.
	do i = 1, n
		prodesc = prodesc + u(i) * v(i)
	end do
	print*, 'Resultado con bucle:', prodesc
	
	prodesc = 0.
	prodesc = sum (u * v)
	print*, 'Resultado con sum():', prodesc
	
	prodesc = 0.
	prodesc = dot_product(u, v)
	print*, 'Resultado con dot_product():', prodesc
	
	deallocate (u, v)
	
end program prodesc_ppal
