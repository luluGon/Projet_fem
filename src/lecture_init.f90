module lecture_init

use const_var

contains

subroutine lect_ini()

	implicit none
	
	open(40, file='data/init.dat', status='old', action='read')
	
	read(40, *) choix_P
	read(40, *) nom_fichier
	read(40, *) choix_f
	
	close(40)
end subroutine

end module lecture_init
