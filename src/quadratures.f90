module quadratures

use const_var
use fonctions


        contains
        ! fonctions de quadrature utilisant les 3 sommets du triangle
        real(rp) function quad_P1(tab_fonction)
                implicit none
                real(rp), dimension(n_nodes_in_ele), intent(in) :: tab_fonction
                real(rp) :: somme
                integer :: k    
                somme =0.0_rp
                
                do k=1,n_nodes_in_ele
                        somme=somme + tab_fonction(k)
                end do
                quad_P1=somme/(2.0_rp*n_nodes_in_ele)
                return 
                
        end function quad_P1
        
        ! on définie une autre quadrature pour P2, cette fois ci sur 6 points
        real(rp) function quad_P2( fonction )
        	
        	implicit none
        	real(rp), external :: fonction
        	
        	real(rp) :: somme
        	integer :: i
        	
        	
        	somme=0.0_rp
        	
        	do i=1,6
			somme = somme + poids_quad_P2(i)*fonction(x_quad_P2(i),y_quad_P2(i)) 
			
        	end do
        	quad_P2=somme
        	
        	return
        	
        end function quad_P2
        
        
		
end module quadratures
