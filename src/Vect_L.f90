module Vect_L

use const_var
use fonctions
use quadratures

contains

! On crée une subroutine qui va calculer f o Fi phi |detJ_Fi| sur les noeuds de K chapeau
	subroutine f_phi (i,mu, tab_f_phi)
		implicit none
		integer, intent(in) :: i,mu
		real(rp), dimension(n_nodes_in_ele), intent(out) :: tab_f_phi
		real(rp), dimension(2)            :: Fi
		integer :: k
		real(rp) :: x_chap,y_chap
		
	
		do k=1,n_nodes_in_ele
		
			x_chap = K_chap(2*k-1)
			y_chap = K_chap(2*k)
			
			call trans_Fi_P1(i, K_chap(2*k-1:2*k),Fi(1:2))
			
			tab_f_phi(k) = f(Fi(1),Fi(2))*phi(mu,x_chap,y_chap)*tab_Jac_Fi(i)
			
		end do
		
		return 
	end subroutine f_phi
	
! subroutine créant le vecteur L	
subroutine creation_L()
	implicit none
	integer :: i_ele,i_node, i_node_global,compteur,i		! i_node l'indice local à chaque triangle
	real(rp), dimension(n_nodes) :: L_prov 				! L provisoire 
	real(rp), dimension(n_nodes_in_ele) :: tab_f_phi		! tableau qui prendra les valeur de ce qu'il ya sous l'intégrale

	allocate (L(dim_L))
	
	L_prov(:)=0.0_rp
	compteur=0
	
	
	do i_ele=1,n_ele
	
		do i_node=1,n_nodes_in_ele
			i_node_global=tab_ele(i_node,i_ele)	! On associe numérotation local à la numérotation local
			
			if (p(i_node_global) == 0) then 
				
				call f_phi(i_ele,i_node, tab_f_phi)
				L_prov(i_node_global) = L_prov(i_node_global) + quad_P1(tab_f_phi) ! On ajoute l'intégrale à l'indice du noeud dans L
				
			end if
		end do
	end do
	
	! on va maintenant écrire dans notre matrice L, en enlevant les noeuds du bords
	
	do i=1,n_nodes
		
		if (p(i) == 0) then
			compteur=compteur+1
			L(compteur)=L_prov(i)
			!write(6,*) L(compteur)
		end if
	end do
	
	
	
end subroutine creation_L

end module Vect_L
