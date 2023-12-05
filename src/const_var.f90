module const_var

USE ISO_FORTRAN_ENV

! Parametres
INTEGER, PARAMETER :: rp=REAL64
REAL(RP), PARAMETER :: pi=acos(-1.0_rp)


! variables
REAL(RP), DIMENSION(:,:), ALLOCATABLE :: tab_nodes
INTEGER, DIMENSION(:,:), ALLOCATABLE :: tab_ele	
INTEGER, DIMENSION(:), ALLOCATABLE :: p
INTEGER :: n_nodes, n_ele, n_nodes_in_ele, dim_espace


end module const_var
