subroutine sistl1(n,a,b,u)
use mod_clreal
implicit none

integer, intent(in)::n
real(kind=clreal), intent(in)::a(n,n),b(n)
real(kind=clreal), intent(out)::u(n)
integer::k,i
real(kind=clreal)::aux

!u(1)=b(1)/a(1,1)
!do i=2,n
 !   aux=0.
  !  do k=1,i-1
   !     aux=aux+a(i,k)*u(k)
    !enddo
   ! u(i)=(b(i)-aux)/a(i,i)
!enddo

u=b
do i=1,n
    u(i+1:n)=u(i+1:n)-a(i+1:n,i)*u(i)
end do

end subroutine sistl1
!revisar todos los jes anteriores que nopuedes poner read a
