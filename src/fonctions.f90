 module fonctions
 use const_var
 
 
 contains
 	
 	
 	subroutine trans_Fi_P1(i,Xi,X_chap)
 	implicit none 
 	real(rp), dimension(2), intent(in) :: X_chap
 	real(rp), dimension(2), intent(in) :: Xi
 	integer, intent(in) :: i
 	
 	
 	end subroutine trans_Fi_P1
 	
 	
 	real(rp)  function f(x,y)
        	implicit none
        	real(rp), intent(in) :: x,y
        	f=1.0_rp
        	return
	end function f
		
end module fonctions
