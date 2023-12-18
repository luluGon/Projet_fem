 module fonctions
 use const_var
 
 
 contains
 	
 	! F qui passe de x,y chap à x,y
 	subroutine trans_Fi_P1(i, X_entre,X_sortie)
 	implicit none 
 	real(rp), dimension(2), intent(in)  :: X_entre
 	real(rp), dimension(2), intent(out) :: X_sortie
 	integer, intent(in) :: i
 	
 	
 	X_sortie(1)= K(1,i) +(K(3,i)-K(1,i))*X_entre(1) +(K(5,i)-K(1,i))*X_entre(2)	!x
 	X_sortie(2)= K(2,i) +(K(4,i)-K(2,i))*X_entre(1) +(K(6,i)-K(2,i))*X_entre(2)	!y
 	
 	return
 	end subroutine trans_Fi_P1

	! fonction calculant la valeur exacte de U selon x,y elle permet la comparaison avec 
 	real(rp) function Uex(x,y)
 			
 			implicit none
 			real(rp), intent(in) :: x,y
 			select case (choix_f)
 			
 			case (1)
 				Uex = sin(2.0_rp*pi*x)*sin(2.0_rp*pi*y)
 			case (2)
 				Uex = x*(x-1.0_rp)*y*(y-1.0_rp)
 			case default
 				write(6,*) " pas d'autres fonctions prevu"
 				STOP
 			end select
 			return
 			
 	end function Uex
 	
 	
 	! fonction f
 	real(rp)  function f(x,y)
        	implicit none
        	real(rp), intent(in) :: x,y
        	select case (choix_f)
        	
        	case (1)
        		f= 8.0_rp*pi*pi*sin(2.0_rp*pi*x)*sin(2.0_rp*pi*y) 	! pour u = sin(2pix)sin(2piy)
        	case (2)
        		f=-2.0_rp*(x*(x-1.0_rp) +y*(y-1.0_rp)) 			! pour u = x(x-1)y(y-1)
        	case default 
        		STOP
        	end select
        	return
        	
	end function f
	
	!les différents phi chapeau 
	real(rp) function phi(mu,x,y)
			implicit none
			real(rp), intent(in) :: x,y
			integer,  intent(in) :: mu
			
			select case (choix_P)
				case (1)
				! selon mu = 1,2,3
				select case (mu)
			
					case(1)
						phi=1.0_rp-x-y
					case(2)
						phi=x
					case(3)
						phi=y
					case default 
						write(6,*) "Il n'y a que 3 fonctions par élément pour P1"
						STOP
				end select
				case (2)
				! selon mu allant de 1 à 6
				select case (mu)
			
					case(1)
						phi=2.0_rp*(1.0_rp-x-y)*(0.5_rp-x-y)
					case(2)
						phi=2.0_rp*x*(x-0.5_rp)
					case(3)
						phi=2.0_rp*y*(y-0.5_rp)
					case(6)
						phi=4.0_rp*x*(1.0_rp-x-y)
					case(4)
						phi=4.0_rp*x*y
					case(5)
						phi=4.0_rp*y*(1.0_rp-x-y)
					case default 
						write(6,*) "Il n'y a que 6 fonctions par élément pour P2"
						STOP
				end select
				
				case default 
					write(6,*) ' choix_P= 1 ou 2 '
			end select
			
			return
	end function phi
	
	! les different gradient de phi chapeau 
	subroutine grad_phi(mu, grad, x,y)
		
		implicit none
		integer,  intent(in) :: mu
		real(rp), intent(in) :: x,y
		real(rp), dimension(2), intent(out) :: grad
		
		select case (choix_P)
		
		case (1)
			! selon mu=1,2,3
			select case (mu)
				case (1)
					grad(1)=-1.0_rp
					grad(2)=-1.0_rp
			
				case (2)
					grad(1)=1.0_rp
					grad(2)=0.0_rp
				
				case (3)
					grad(1)=0.0_rp
					grad(2)=1.0_rp
				case default 
					write(6,*) "Il n'y a que 3 fonctions par élément pour P1"
					STOP
			end select	
		
		case (2)
			select case (mu)
				! selon mu de 1 à 6
				case (1)
					grad(1)=4.0_rp*x +y -1.5_rp
					grad(2)=4.0_rp*y +x -1.5_rp
			
				case (2)
					grad(1)=4.0_rp*x -1.0_rp
					grad(2)=0.0_rp
				
				case (3)
					grad(1)=0.0_rp
					grad(2)=4.0_rp*y -1.0_rp
				case (6)
					grad(1)= -8.0_rp*x +1.0_rp -y
					grad(2)= -4.0_rp*x
				case (4) 
					grad(1)= 4.0_rp*y
					grad(2)= 4.0_rp*x
				case (5)
					grad(1)= -4.0_rp*y
					grad(2)= -8.0_rp*y +1.0_rp -x
				case default 
					write(6,*) "Il n'y a que 6 fonctions par élément pour P2"
					STOP
			end select
		
		case default 
					write(6,*) ' choix_P= 1 ou 2 '
					STOP
		end select
		
		return
		
	end subroutine grad_phi
	
		
	!crée un tableau avec les valeur absolue des déterminants de Jacobiennes des transformations
	subroutine Jac_Fi()
		implicit none
		integer :: i
		allocate(tab_Jac_Fi(n_ele))
		
		do i=1, n_ele
			tab_Jac_Fi(i) = abs((K(3,i)-K(1,i))*(K(6,i)-K(2,i))-(K(4,i)-K(2,i))*(K(5,i)-K(1,i)))
			
		end do
		
		return 
		
		end subroutine Jac_Fi
	
	! crée un tableau de matrice des Jacobienne inverse transposé poour chaque Fi
	! avec Jacobienne F =	a		b
	!			c		d
	subroutine Jaco_F_inv_T()
		implicit none
		integer :: l
		real(rp) :: det
		allocate(JF_inv_T( 2,2,n_ele))
		
		
		do l=1, n_ele
			det=(K(3,l)-K(1,l))*(K(6,l)-K(2,l))-(K(4,l)-K(2,l))*(K(5,l)-K(1,l))		!  ad -bc
			
			JF_inv_T(1,1,l) =  (K(6,l)-K(2,l))  /det	!  d/det
			JF_inv_T(2,1,l)	= -(K(5,l)-K(1,l))  /det	! -b/det
			JF_inv_T(1,2,l) = -(K(4,l)-K(2,l))  /det	! -c/det
			JF_inv_T(2,2,l) =  (K(3,l)-K(1,l))  /det	!  a/det
			
		end do
		
		return
		
	end subroutine Jaco_F_inv_T
	
		
end module fonctions 
