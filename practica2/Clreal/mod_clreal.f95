module mod_clreal

	!Este modulo define la clase real
	
	!Compilacion: f95 -c mod_clreal.f95
	!Para usarlo, hay que compilar para crear el .o y el .mod. Se usa en un programa de la siguiente manera:
	! 
	!use mod_clreal
	! 
	!real(clreal) :: a
	
	implicit none
	
	integer, parameter :: clreal = selected_real_kind (p = 15, r = 307)
	
	!precision de al menos 15 cifras decimales significativas y un rango de exponente de al menos 10^-307 a 10^307
	
end module mod_clreal
