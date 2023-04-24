program matdve_ppal

	!Producto de una matriz tridiagonal por un vector
	!f95 -o matdve_ppal.out matdve_ppal.f95
	
	use mod_clreal

	implicit none
	
	integer :: n, i, j
	real (clreal), allocatable, dimension (:) :: ad, al, au, v, w, r
	real (clreal), allocatable, dimension (:,:) :: a
	!ad = diagonal, al = subdiagonal, au = superdiagonal

	!read*, n
	n = 50
	
	allocate (a(n, n), ad(n), al(n - 1), au(n - 1), v(n), w(n), r(n))
	
	print*, 'Orden de la matriz:', n
	
	!read*, au
	!read*, ad
	!read*, al
	!read*, v
	
	do i = 1, n - 1
		ad(i) = 4._clreal
		au(i) = 2._clreal
		al(i) = -2._clreal
		if (mod(i, 2) == 0) then
			v(i) = 2._clreal + i
		else
			v(i) = 3._clreal + i
		endif
	enddo
	ad(n) = 4
	v(n) = 52
	
	do i = 1, n
		do j = 1, n
			if ((i - j) == 1) then
				a(i, j) = au(i)
			elseif (i == j) then
				a(i, j) = ad(i)
			elseif ((i - j) == -1) then
				a(i, j) = al(i)
			else
				a(i, j) = 0.
			endif
		enddo
		print*
	enddo
	
	w(1) = ad(1) * v(1) + au(1) * v(2)
	w(n) = ad(n) * v(n) + al(n - 1) * v(n - 1)
	
	!Con bucle
	do i = 2, n - 1
		w(i) = al(i - 1) * v(i - 1) + ad(i) * v(i) + au(i) * v(i + 1)
	end do
	
	print*
	print*,'Matriz A:'
	do i = 1, n
		print*, a(i, :)
	enddo
	
	print*
	do i = 1, n
		print*, 'Componente',i,'de v:', v(i)
	enddo
	
	print*
	do i = 1, n
		print*, 'Componente',i,'de w:', w(i)
	enddo
	
	r = w - v
	
	print*
	do i = 1, n
		print*, 'Componente',i,'de r:', r(i)
	enddo
	
	deallocate (ad, al, au, v, w, r)
	
end program matdve_ppal
