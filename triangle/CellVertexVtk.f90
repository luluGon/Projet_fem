  SUBROUTINE CellVertexVtk(DATA, Mesh, Var, PbName, flag_topo)

    TYPE(Donnees)   , INTENT(in)     :: DATA
    TYPE(MeshDef)   , INTENT(in)     :: Mesh
    TYPE(Variables) , INTENT(in)     :: Var
    CHARACTER(LEN=70),INTENT(in)     :: PbName
    integer          ,intent(in)     :: flag_topo

    REAL(PR)          :: u_x, u_y
    CHARACTER(LEN=75) :: vtk=" "
    INTEGER           :: is, jt
    INTEGER           :: lensd3, lPbName


    IF (Data%Impre > 5 ) WRITE (6,FMT = *) " --------------------------------------------"
    IF (Data%Impre > 5 ) WRITE (6,FMT = *) " ---------      passage dans vtk   ----------"
    IF (Data%Impre > 5 ) WRITE (6,FMT = *) " --------------------------------------------"

    ! ECRITURE sous FICHIER vtk !

    lPbName                    = INDEX(PbName,' ') - 1
    vtk(1:lPbName)             = PbName(1:lPbName)
    vtk(lPbName+1:lPbName+5)   = '.vtk '
    lensd3                     = lPbName+4
    OPEN(UNIT=61,FILE=vtk(1:lensd3) )

    WRITE(61,'(A)')'# vtk DataFile Version 3.0'
    WRITE(61,'(A)')'# Solution du M1 TR'
    WRITE(61,'(A)')'ASCII'
    WRITE(61,'(A)')'DATASET UNSTRUCTURED_GRID'
    WRITE(61,'(A,I7,A)')'POINTS', Mesh%Npoint,'  float'

    DO is = 1,Mesh%Npoint
       if (flag_topo == 1) then
          WRITE(61,'(E13.7,2x,E13.7,2x,E13.7)') Mesh%coor(1,is), Mesh%coor(2,is),  DATA%Z(is)
       else
          WRITE(61,'(E13.7,2x,E13.7,2x,E13.7)') Mesh%coor(1,is), Mesh%coor(2,is),  Var%Ua(1,is) + DATA%Z(is)
       end if
    END DO
    WRITE(61,'(A,1x,I7,1x,I6)')'CELLS',Mesh%Nelemt, 4*Mesh%Nelemt

    DO jt = 1,Mesh%Nelemt
       WRITE(61,'(I1,1x,I7,1x,I7,1x,I7)') 3, Mesh%Nu(1,jt)-1, Mesh%Nu(2,jt)-1, Mesh%Nu(3,jt)-1
    END DO

    WRITE(61,'(A,1x,I7)')'CELL_TYPES', Mesh%Nelemt
    DO jt = 1,Mesh%Nelemt
       WRITE(61,'(I1)') 5
    ENDDO

    WRITE(61,'(A,1x,I7)')'POINT_DATA',Mesh%Npoint
    WRITE(61,'(A)')'SCALARS hauteur float'
    WRITE(61,'(A)')'LOOKUP_TABLE DEFAULT'

    DO is = 1,Mesh%Npoint
       WRITE(61,'(ES20.7)') max(1.E-08,Var%Ua(1,is))
    END DO

    WRITE(61,'(A)')'SCALARS topographie float'
    WRITE(61,'(A)')'LOOKUP_TABLE DEFAULT'

    DO is = 1,Mesh%Npoint
       WRITE(61,'(ES20.7)') max(1.E-08,DATA%Z(is))
    END DO

    WRITE(61,'(A)')'VECTORS vitesse float'

    DO is = 1,Mesh%Npoint
       u_x = vitesse(Var%Ua(1,is) , Var%Ua(2,is))
       u_y = vitesse(Var%Ua(1,is) , Var%Ua(3,is))
       WRITE(61,'(E13.7,2x,E13.7,2x,E13.7)') max(1.E-08,u_x), max(1.E-08,u_y), 0.
    END DO

    CLOSE(61)

  END SUBROUTINE CellVertexVtk

