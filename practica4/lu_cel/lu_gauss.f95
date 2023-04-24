subroutine lu_gauss(n, a, deter)
use mod_clreal
implicit none
real(kind=clreal),intent(inout) ::a(n,n)
integer, intent(in):: n
integer::j,k,i
real(kind=clreal), intent(out)::deter 
real(kind=clreal)::piv 
!inicializacion del determinante
deter=1.
!etapa k-esima de la eliminacion
do k=1,n-1
    piv=a(k,k)
    if(abs(a(n,n))<1.e-12) then
        print*,'Pivote nulo en la etapa: ',n
        stop
    end if
    !actualizacion del determinante
    deter=deter*piv
    !eliminacion
    !do i=k+1,n
    !a(i,k)=a(i,k)/piv
    !    do j=k+1,n
    !        a(i,j)=a(i,j)-a(i,k)*a(k,j)
    !    end do
   ! end do
   
   !VECTORIZACION
   a(k+1:n,k)=a(k+1:n,k)/piv
   do j=k+1,n
    a(k+1:n,j)=a(k+1:n,j)-a(k+1:n,k)*a(k,j)
    end do
   
end do

!comprobacion de que el
!ultimo pivote no es nulo
    if(abs(a(n,n))<1.e-12) then
        print*,'Pivote nulo en la etapa: ',n
stop
end if
!fin del calculo del determinante
deter=deter*a(n,n)

end subroutine lu_gauss
