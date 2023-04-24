module mod_clreal
!modulo que define la clase clreal

implicit none
integer, parameter::clreal=selected_real_kind(p=15,r=307)
!al menos 15 cifras significativas exactas  -307 a +307
!y un rango de exp de al menos 10 a 10
!f95 -c mod_clreal.f95
!y lo anhades a cualquier programa con use modclreal
end module mod_clreal
