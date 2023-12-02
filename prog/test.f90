program test
use fonctions

implicit none


real(rp), dimension(1:6) ::F1= [0.0,0.0, 0.0,1.0,1.0,0.0]

real(rp) :: t

t=quad_P1(F1)

print*, t

		
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
        
end program test
