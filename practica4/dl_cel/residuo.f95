subroutine subresiduo(m,n,a,b,u,r)
use mod_clreal
implicit none
integer, intent(in):: n,m
integer::i,j
real(kind=clreal)::aux
real(kind=clreal),intent(in) ::a(m,n),b(m),u(n)
real(kind=clreal),intent(out)::r(m)

do i=1,m
    aux=0.
    do j=1,n
        aux=aux+a(i,j)*u(j)
    end do
    r(i)=aux-b(i)
end do
end subroutine subresiduo
