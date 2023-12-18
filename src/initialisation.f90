module initialisation 
use const_var

contains

!creation de notre K chapeau en fonction du degre de P
subroutine init_K_chap()
	implicit none
	
	allocate(K_chap(1:dim_espace*n_nodes_in_ele))
	
	select case (choix_P)
		case (1)
			K_chap(1:dim_espace*n_nodes_in_ele)=[ 0.0_rp,0.0_rp, 1.0_rp,0.0_rp, 0.0_rp,1.0_rp ]
			
		case (2)
			K_chap(1:dim_espace*n_nodes_in_ele)=[0.0_rp,0.0_rp, 1.0_rp,0.0_rp, 0.0_rp,1.0_rp, &
			0.5_rp,0.5_rp, 0.0_rp,0.5_rp, 0.5_rp,0.0_rp ]
		case default 
			write(6,*) "Pas encore définie pour d'autres degré de P"
			STOP
	end select
	return 
	
end subroutine init_K_chap

! creation du vecteur K contenant les coordonnées des noeuds de chaques éléments  
subroutine creation_K()

	implicit none
	integer :: i,j
	
	allocate (K(1:dim_espace*n_nodes_in_ele,n_ele))
	do i=1,n_ele
		! on affecte les dim_espace*n_nodes_in_ele valeurs de K indice i, et ce par groupes de coordonnées spatiale 
		do j=1, n_nodes_in_ele
		
			K(1+dim_espace*(j-1):dim_espace*j,i)=tab_nodes(1:dim_espace,tab_ele(j,i))
		
			
		end do
		
	end do
	
	return
end subroutine

end module initialisation
