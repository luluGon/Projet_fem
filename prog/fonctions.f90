 module fonctions
 use iso_fortran_env
 
 implicit none
 integer, parameter :: rp=REAL64
 
 contains
 	
 	
 	subroutine trans_Fi_P1(i,Xi,X_chap)
 	implicit none 
 	real(rp), intent(in) :: X_chap
 	integer, intent(in) :: i
 	
 	
 	end subroutine trans_F_P1
 	
 	
 	real(rp)  function f(x,y)
        	implicit none
        	real(rp), intent(in) :: x,y
        	f=1.0_rp
        	return
	end function f
		
end module fonctions
