program prodmat_ppal

	!Producto de una matriz tridiagonal por un vector
	!f95 -o tridiagonal_ppal.out tridiagonal_ppal.f95
	!./tridiagonal_ppal.out <tridiagonal_dat> tridiagonal_sal
	
	use mod_clreal

	implicit none
	
	integer :: n, i
	real (clreal), allocatable, dimension (:) :: ad, al, au, v, w
	!ad = diagonal, al = subdiagonal, au = superdiagonal

	read*, n
	
	allocate (ad(n), al(n - 1), au(n - 1), v(n), w(n))
	
	print*, 'Orden de la matriz:', n
	
	read*, au
	read*, ad
	read*, al
	read*, v
	
	print*
	print*, 'Vector v:', v
	print*, 'Superdiagonal:', au
	print*, 'Diagonal:', ad
	print*, 'Subdiagonal:', al
	
	w(1) = ad(1) * v(1) + au(1) * v(2)
	w(n) = ad(n) * v(n) + al(n - 1) * v(n - 1)
	
	!Con bucle
	do i = 2, n - 1
		w(i) = al(i - 1) * v(i - 1) + ad(i) * v(i) + au(i) * v(i + 1)
	end do	
	
	print*
	print*, 'Resultado:', w
	
	deallocate (ad, al, au, v, w)
	
end program prodmat_ppal
