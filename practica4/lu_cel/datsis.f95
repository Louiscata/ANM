subroutine datsis(n,a,b)
use mod_clreal
implicit none
integer, intent(in):: n
integer:: i,j
real(kind=clreal),intent(inout) ::a(n,n),b(n)  !comprueba si seria inout o out

print*,'Valores de A: '
print*,'Valores para una matriz A: '
do i=1,n
    read*,a(i,:) 
    print*,a(i,:)
end do 

print*, 'Valores de B: '
read*,b

end subroutine datsis
