subroutine sistu_sub(n,a,b,u)
use mod_clreal
implicit none

integer, intent(in)::n
real(kind=clreal), intent(in)::a(n,n),b(n)
real(kind=clreal), intent(out)::u(n)
integer::k,i
real::aux

u(n)=b(n)/a(n,n)
do i=n-1,1,-1
    aux=0.
    u(i)=b(i)
    do k=i+1,n
        aux=aux+a(i,k)*u(k)
    enddo
    u(i)=(b(i)-aux)/a(i,i)
enddo

end subroutine sistu_sub
!revisar todos los jes anteriores que nopuedes poner read a
