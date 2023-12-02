module quadratures
use iso_fortran_env
use fonctions

        contains
        real(rp) function quad_P1(Fi)
                implicit none
                real(rp), dimension(1:6), intent (in) :: Fi
                real(rp) :: somme
                integer :: k    
                
                do k=1,6,2
                        somme=somme + f(Fi(k),Fi(k+1))
                end do
                quad_P1=somme/6
                return 
                
        end function quad_P1
        
		
end module quadratures
