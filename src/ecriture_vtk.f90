! Module pour créer le fichier vtk
module ecriture_vtk

USE const_var
USE fonctions


contains 

! crée un fichier vtk pour pouvoir afficher la solution exacte calculé sur les noeuds
subroutine creation_vtk_ex()
	
IMPLICIT NONE
  INTEGER          :: int, i, lens, lens2, lens3,is,jt
  REAL(8)          :: r

  CHARACTER(len=30):: nomfic, nomfic2
  CHARACTER(LEN=75) :: vtk="data/"
  
  nomfic=nom_fichier
 
  lens   = INDEX(nomfic,' ')-1   
  vtk(6:lens+5)             	= nomfic(1:lens)
  vtk(lens+6:lens+12)   			= '_ex.vtk'
 
 


 OPEN(UNIT=61,FILE=vtk(1:lens+12) )
 WRITE(61,'(A)')'# vtk DataFile Version 3.0'
    WRITE(61,'(A)')'# Solution du M1 TR'
    WRITE(61,'(A)')'ASCII'
    WRITE(61,'(A)')'DATASET UNSTRUCTURED_GRID'
    WRITE(61,'(A,I7,A)')'POINTS', n_nodes,'  float'
    
 	DO is = 1,n_nodes
        WRITE(61,'(E13.7,2x,E13.7,2x,E13.7)') tab_nodes(1,is), tab_nodes(2,is), 0.0_8
	END DO
	
    WRITE(61,'(A,1x,I7,1x,I6)')'CELLS',n_ele, 4*n_ele
	DO jt = 1,n_ele
       WRITE(61,'(I1,1x,I7,1x,I7,1x,I7)') 3, tab_ele(1,jt)-1, tab_ele(2,jt)-1, tab_ele(3,jt)-1
    END DO
    
    WRITE(61,'(A,1x,I7)')'CELL_TYPES', n_ele
    DO jt = 1,n_ele
       WRITE(61,'(I1)') 5
    ENDDO
    
    WRITE(61,'(A,1x,I7)')'POINT_DATA',n_nodes
    WRITE(61,'(A)')'SCALARS hauteur float'
    WRITE(61,'(A)')'LOOKUP_TABLE DEFAULT'

    DO is = 1, n_nodes
       WRITE(61,'(ES20.7)') Uex(tab_nodes(1,is),tab_nodes(2,is))
    END DO
    
    CLOSE(61)
    
end subroutine creation_vtk_ex

! crée un fichier vtk pour pouvoir afficher U 
subroutine creation_vtk()
	
IMPLICIT NONE
  INTEGER          :: int, i, lens, lens2, lens3,is,jt
  REAL(8)          :: r

  CHARACTER(len=30):: nomfic, nomfic2
  CHARACTER(LEN=75) :: vtk="data/"
  
  nomfic=nom_fichier
 
  lens   = INDEX(nomfic,' ')-1   
  vtk(6:lens+5)             	= nomfic(1:lens)
  vtk(lens+6:lens+9)   			= '.vtk'
 
 


 OPEN(UNIT=61,FILE=vtk(1:lens+9) )
 WRITE(61,'(A)')'# vtk DataFile Version 3.0'
    WRITE(61,'(A)')'# Solution du M1 TR'
    WRITE(61,'(A)')'ASCII'
    WRITE(61,'(A)')'DATASET UNSTRUCTURED_GRID'
    WRITE(61,'(A,I7,A)')'POINTS', n_nodes,'  float'
    
 	DO is = 1,n_nodes
        WRITE(61,'(E13.7,2x,E13.7,2x,E13.7)') tab_nodes(1,is), tab_nodes(2,is), 0.0_8
	END DO
    
    select case (choix_P)
    	case (1)
    	WRITE(61,'(A,1x,I7,1x,I6)')'CELLS',n_ele, 4*n_ele
		DO jt = 1,n_ele
      			WRITE(61,'(I1,1x,I7,1x,I7,1x,I7)') 3, tab_ele(1,jt)-1, tab_ele(2,jt)-1, tab_ele(3,jt)-1
    		END DO
    	
    	case (2)
    	WRITE(61,'(A,1x,I7,1x,I6)')'CELLS',n_ele, 7*n_ele
		DO jt = 1,n_ele
       			WRITE(61,'(I1,1x,I7,1x,I7,1x,I7)', advance='no') 3, tab_ele(1,jt)-1, tab_ele(2,jt)-1,tab_ele(3,jt)-1
       			WRITE(61,'(1x,I7,1x,I7,1x,I7)') tab_ele(4,jt)-1, tab_ele(5,jt)-1, tab_ele(6,jt)-1
   		END DO
   	case default
   		STOP
    end select
    
    WRITE(61,'(A,1x,I7)')'CELL_TYPES', n_ele
    DO jt = 1,n_ele
       WRITE(61,'(I1)') 5
    ENDDO
    
    WRITE(61,'(A,1x,I7)')'POINT_DATA',n_nodes
    WRITE(61,'(A)')'SCALARS hauteur float'
    WRITE(61,'(A)')'LOOKUP_TABLE DEFAULT'

    DO is = 1, n_nodes
       WRITE(61,'(ES20.7)') U(is)
    END DO
    
    CLOSE(61)
    
end subroutine creation_vtk



end module ecriture_vtk
