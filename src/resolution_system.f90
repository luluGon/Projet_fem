module resolution_system

USE const_var

contains

! resolution du system AU=L par la subroutine lapack dgesv utilisant une decomposition LU
subroutine res_standard()

	implicit none
	real(rp),	dimension(dim_L)	:: U_prov
	integer, 	dimension(dim_L)	:: IPIV
	integer							:: INFO, i, compteur
	
	allocate(U(n_nodes))
	
	compteur = 0
	U_prov(:) = L(:)
	
	! On resoud d'abord sur les noeuds intérieurs
	call DGESV( dim_L,  1,  A,  dim_L,  IPIV,  U_prov,  dim_L, INFO )
	
	! On ajoute maintenant les valeurs sur le bord
	do i=1, n_nodes
		if ( p(i) == 1 ) then 
			U(i) = 0.0_rp
		else
			compteur = compteur + 1
			U(i) = U_prov(compteur)
		end if
	end do
	
	
	return
	
	end subroutine res_standard
	
end module resolution_system
