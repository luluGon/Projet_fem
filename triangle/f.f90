program f

INTEGER, DIMENSION(:,:),ALLOCATABLE :: p, t
REAL(8), DIMENSION(:,:),ALLOCATABLE :: coord
integer ::i
integer ::dim

call op('carre',p,t,coord)
contains 

subroutine op(nomfic,p,t,coord)
	
IMPLICIT NONE
  INTEGER          :: n_node, n_ele, n_edge, Dim, n1, n2, n3, n4, n_per_triangle
  INTEGER          :: int, i, lens, lens2, lens3
  REAL(8)          :: r
  INTEGER, DIMENSION(:,:),ALLOCATABLE :: p, t
  REAL(8), DIMENSION(:,:),ALLOCATABLE :: coord
  CHARACTER(len=30):: nomfic, nomficnode, nomficele, str
  
  !--------------------------------------------------------!
  !    LECTURE DES FICHIERS CARRE.1.NODE ET CARRE.1.ELE    !
  !--------------------------------------------------------!
 
  lens   = INDEX(nomfic,' ')-1   
  str=''
  lens2  = INDEX(str,' ') 

  nomficnode = str
  nomficele = str
  nomficnode(lens2:lens2+lens) = nomfic
  nomficnode(lens2+lens:lens2+lens+7)= '.1.node'
  nomficele(lens2:lens2+lens) = nomfic
  nomficele(lens2+lens:lens2+lens+6) = '.1.ele'

  OPEN(unit=50,file=nomficnode)
  READ(50,*)n_node,Dim,n1,n2
  ALLOCATE(coord(n_node,Dim),p(n_node,n1+n2))
  DO i=1,n_node
     READ(50,*)int,coord(i,:),p(i,:)
  ENDDO

  CLOSE(50)

  OPEN(unit=50,file=nomficele)
  READ(50,*)n_ele,n_per_triangle,n3
  ALLOCATE(t(n_per_triangle+n3,n_ele))
  DO i=1,n_ele
     READ(50,*)int,t(:,i)
  ENDDO

  CLOSE(50)

  
end subroutine op  



end program f
