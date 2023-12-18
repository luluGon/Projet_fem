module erreur

use const_var
use fonctions

contains 

! calcul l'erreur en norme L2 de U-Uex 
real(rp) function erreurL2()
	implicit none
	integer :: i, j
	real(rp) :: somme, somme_interne
	
	somme=0.0_rp
	
	
	do i=1,n_ele
		! on approxime les intégrale sur les triangle par la somme des valeurs sur les noeuds diviser par 2*le nombre de noeuds
		do j=1,n_nodes_in_ele
			somme_interne=0.0_rp
			somme_interne = somme_interne + U(tab_ele(j,i)) - Uex(K(2*j-1,i),K(2*j,i))
		end do
		somme = somme +somme_interne*somme_interne/(2.0_rp*n_nodes_in_ele)
	end do
	erreurL2= sqrt(somme)
	
	return
	
end function erreurL2
end module erreur
