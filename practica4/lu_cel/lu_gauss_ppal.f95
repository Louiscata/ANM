program lu_gauss_ppal
use mod_clreal
implicit none 
integer:: n, i , j, k
real(kind=clreal), allocatable,dimension(:)::a(:,:),b(:),x(:),aa(:,:),y(:),r(:)
real(kind=clreal):: deter
character(len=4)::formato1='(i4)'
character(len=10)::formato2='(100e12.4)'

print*,'Num Filas y Columnas de A:'
read*,n

allocate(a(n,n),b(n), aa(n,n),x(n),y(n),r(n))

call datsis(n,a,b)
aa=a
call lu_gauss(n, a,deter)
print*, 'El determinante de la matriz es ', deter

call sistl1(n,a,b,y) 
print*, 'La matriz triangular inferior L es'
do i=1,n
    print* , a(i,1:i-1),1.
    end do
call sistu_sub(n,a,y,x)
print*,'x=',x

print*, 'La matriz triangular superior U es'
do i=1,n
    print* , (0., j=1,i-1),a(i,i:n)
    end do
call subresiduo(n,n,aa,b,x,r)

print*, ' EL residuo u es:  ',r
print*,'deter=',deter
deallocate(a,aa,b,x,y,r)
end program lu_gauss_ppal
