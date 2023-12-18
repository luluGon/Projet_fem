program main

use const_var
use lecture_init
use recup_data
use initialisation
use fonctions
use Vect_L
use mat_A
use resolution_system
use ecriture_vtk
use erreur

implicit none
real(rp) :: er

!!!!!!!!!!!!!!!!!!!!!!!
! LECTURE DES DONNÉES !	
!!!!!!!!!!!!!!!!!!!!!!!

! appel du fichier de configuration
call lect_ini()

! recupérations des noeuds et des éléments
call recup_ele()
call recup_nodes()



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! APPEL DES SUBROUTINES UTILES À LA RESOLUTION DU PROBLÈME !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! création de K chapeau, le triangle de référence
call init_K_chap()
! création du tableau de tous les triangle du maillage
call creation_K()
! création du tableau de toutes les déterminants en valeur absolue des jacobiennes des transformations affines associés aux triangles du maillage 
call Jac_Fi()
! creation du tableau contenant les transposé des matrices inverse des Jacobiennes des transformations affines associés aux triangles du maillage
call Jaco_F_inv_T()



!!!!!!!!!!!!!!!!!!!!!!!!!!
! RÉSOLUTION DU PROBLÈME !
!!!!!!!!!!!!!!!!!!!!!!!!!!

! Creation du vecteur L
call creation_L()
! Creation de la matrice A standard ( pas creuse )
call creation_A()

! resolution AU=L (matrice A non creuse )
call res_standard()


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ÉCRITURE DES RÉSULTATS DANS DES FICHIERS VTK !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! création du fichier vtk associé à la solution U
call creation_vtk()
! création du fichier vtk asscoié à la solution exacte
call creation_vtk_ex()

!!!!!!!!!!!!!!!!!!!!!!!!!
! AFFICHAGE DE L'ERREUR !
!!!!!!!!!!!!!!!!!!!!!!!!!
er=erreurL2()
select case (choix_P)
case (1)
	Write(6,'(A,I5,A)') 'Pour la méthode P1, avec ',n_nodes,' noeuds.'
	write(6,'(A,E10.6)') "L'erreur en norme L_2 est de : ", er
case (2)
	Write(6,'(A,I5,A)') 'Pour la méthode P2, avec ',n_nodes,' noeuds.'
	write(6,'(A,E13.6)') "L'erreur en norme L_2 est de : ", er
case default
	STOP
end select 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! LIBÉRATION DE LA MÉMOIRE !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

deallocate (tab_ele,tab_nodes,tab_Jac_Fi,p,K,K_chap,L,A,U)
		


        
end program main
