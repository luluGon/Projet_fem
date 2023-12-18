!module créant les paramètre, déclarant les variables gloables et les types
module const_var

USE ISO_FORTRAN_ENV

! Parametres
INTEGER, 	PARAMETER :: rp=REAL64
REAL(RP), 	PARAMETER :: pi=acos(-1.0_rp)

REAL(RP),	PARAMETER,	DIMENSION(6) :: x_quad_P2=[0.4459484909,0.1081030181,&
0.4459484909,0.0915762135,0.8168475729,0.0915762135]
REAL(RP),	PARAMETER,	DIMENSION(6) :: y_quad_P2=[0.4459484909,0.4459484909,&
0.1081030181,0.0915762135,0.0915762135,0.8168475729]
REAL(RP),	PARAMETER,	DIMENSION(6) :: poids_quad_P2= [0.1116907948,0.1116907948,&
0.1116907948,0.0549758718,0.0549758718,0.0549758718]


! variables globales
REAL(RP), 	DIMENSION(:,:,:), 	ALLOCATABLE :: JF_inv_T
REAL(RP), 	DIMENSION(:,:), 	ALLOCATABLE :: K, tab_nodes, A
INTEGER, 	DIMENSION(:,:), 	ALLOCATABLE :: tab_ele,	t
INTEGER, 	DIMENSION(:), 		ALLOCATABLE :: p
REAL(RP), 	DIMENSION(:), 		ALLOCATABLE :: K_chap, tab_Jac_Fi, L, U	

INTEGER :: n_nodes, n_ele, n_nodes_in_ele, dim_espace, choix_P, dim_L, choix_f

character(len=30) :: nom_fichier

! types
type :: Mat_creuse
	REAL(rp), DIMENSION(:),ALLOCATABLE :: valeur_mat
	INTEGER,  DIMENSION(:),ALLOCATABLE :: i_ligne
	INTEGER,  DIMENSION(:),ALLOCATABLE :: j_ligne
end type

type(Mat_creuse) :: A_creuse

end module const_var
