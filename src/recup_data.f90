module recup_data

use const_var

contains

! recupération des éléments finis du fichier triangles/nom_fichier.1.ele et ajout de ceux-ci dans le tableau tab_ele

subroutine recup_ele()

	implicit none
	character(len=50) :: nom_fichier2
	
	integer :: i, len1			
	
	
	nom_fichier2='triangle/'//nom_fichier
	len1 = index(nom_fichier2, ' ') -1
	nom_fichier2(len1+1:len1+4) = '.ele'
	
	open (60, file= nom_fichier2(1:len1+6),status='old', action='read' )
	
	read(60,*) n_ele, n_nodes_in_ele 
	
	allocate (tab_ele(0:n_nodes_in_ele, n_ele))
	
	do i=1,n_ele
		read(60,*) tab_ele(0:n_nodes_in_ele,i)
		
	end do
	
	close (60)
	return

end subroutine recup_ele

! recupération des noeuds du fichier triangles/nom_fichier.1.nodes et ajout de ceux-ci dans le tableau tab_nodes
subroutine recup_nodes()

	implicit none
	character(len=50) :: nom_fichier2
	
	integer :: i, len1,num		
	
	
	nom_fichier2='triangle/'//nom_fichier
	len1 = index(nom_fichier2, ' ') -1
	nom_fichier2(len1+1:len1+5) = '.node'
	dim_L=0
	open (60, file= nom_fichier2(1:len1+8),status='old', action='read' )
	
	read(60,*) n_nodes, dim_espace
	
	allocate (tab_nodes(dim_espace, n_nodes),p(n_nodes))
	
	do i=1,n_nodes
		read(60,*) num, tab_nodes(1:dim_espace,i), p(i)
		if (p(i)==0) then
			dim_L=dim_L +1
		end if
	end do
	
	close (60)
	return

end subroutine recup_nodes


	
end module recup_data
