module mat_A

use const_var
use fonctions
use quadratures

contains

! calcul les valeur de grad phi_i.grad phi_j apres changement de variable sur les noeud de K_chap
subroutine grad_grad(mu_i,mu_j,l,tab_grad_grad)

	implicit none
	integer, intent(in) :: mu_i,mu_j,l
	real(rp), dimension(n_nodes_in_ele),intent(out) :: tab_grad_grad
	real(rp), dimension(2) :: grad_phi_i, grad_phi_j
	integer ::  i_nodes_in_ele
	real(rp) :: x_chap,y_chap
	
	
	do i_nodes_in_ele=1,n_nodes_in_ele 
		
		x_chap=K_chap((i_nodes_in_ele)*2 -1)
		y_chap=K_chap((i_nodes_in_ele)*2 )
		
		call grad_phi(mu_i,grad_phi_i,x_chap,y_chap)
		call grad_phi(mu_j,grad_phi_j,x_chap,y_chap)
		
		tab_grad_grad(i_nodes_in_ele) = DOT_PRODUCT(MATMUL(JF_inv_T(1:2,1:2,l), grad_phi_i), &
		MATMUL(JF_inv_T(1:2,1:2,l), grad_phi_j))*tab_Jac_Fi(l)
		
	end do

end subroutine grad_grad

! crée la matrice A
subroutine creation_A()
	implicit none
	integer i_ele,i_node,j_node, i, j ,k ,compteur				! i, j indices des noeux globaux
	real(rp), dimension(n_nodes, n_nodes) :: A_prov, A_prov2
	real(rp), dimension(n_nodes_in_ele):: tab_gradi_gradj
	real(rp), dimension(2) :: grad_phi_i, grad_phi_j
	
	allocate(A(dim_L,dim_L))
	
	A_prov(:,:)=0.0_rp
	
	
	do i_ele=1,n_ele
	
		do i_node=1,n_nodes_in_ele
			do j_node=1,n_nodes_in_ele
			
				i=tab_ele(i_node,i_ele)
				j=tab_ele(j_node,i_ele)
				
				if (p(i)==0 .or. p(j)==0 ) then
					
					call grad_grad(i_node,j_node,i_ele,tab_gradi_gradj)
					A_prov(i,j) = A_prov(i,j) +quad_P1(tab_gradi_gradj)
	
				end if
				
			end do	
		end do
		
	end do
	

	compteur = 0
	do i=1,n_nodes
		if (p(i)==0) then
			compteur = compteur +1
			A_prov2(compteur,:) = A_prov(i,:)
			
		end if
	end do
	
	
	compteur = 0
	do j=1,n_nodes
		if (p(j)==0) then
			compteur = compteur +1
			A(:,compteur) = A_prov2(1:dim_L,j)
		end if
	end do

	
!	do i=1,dim_L
!		do j=1,dim_L
!			write(6,'(F8.3)', advance='no') A(i,j)
!		end do
!		write(6,*) ''
!	end do


end subroutine creation_A


	
end module mat_A
