subroutine dl_gauss(n, a, deter)
use mod_clreal
implicit none
real(kind=clreal),intent(inout) ::a(n,n)
integer, intent(in):: n
integer::j,k,i
real(kind=clreal), intent(out)::deter 
real(kind=clreal)::piv ,aux
!inicializacion del determinante
deter=1.

do i=1,n

    do j=i,n
    aux=0.
        do k=1,i-1
                aux=aux+a(i,k)*a(k,j)
        end do
        a(i,j)=a(i,j)-aux
    end do
    if(abs(a(i,i))<1.e-12)then
        print*,'No existe factorizacion o A singular. fallo en la fila ', i
        stop
    end if
    
    deter=deter*a(i,i)

    do j=i+1,n
        aux=0.
        do k=1,i-1
            aux=aux+a(j,k)*a(k,i)
        enddo
        a(j,i)=a(j,i)-aux
        a(j,i)=a(j,i)/a(i,i)
    enddo
end do

end subroutine dl_gauss
