program main

use const_var
use fonctions
use recup_data

implicit none

character(len=25) :: nom_fichier='carre'

call recup_ele(nom_fichier)
call recup_nodes(nom_fichier)

write(6,*) tab_ele(1:4,34)
write(6,*) tab_nodes(1:2,10)
write(6,*) p(10)


deallocate (tab_ele)
		


        
end program main
