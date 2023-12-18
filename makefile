# Variables
FC := gfortran
SRCDIR := src
OBJDIR := obj
MODDIR := mod
SRC := $(SRCDIR)/const_var.f90 $(SRCDIR)/lecture_init.f90 $(SRCDIR)/recup_data.f90 $(SRCDIR)/initialisation.f90 $(SRCDIR)/fonctions.f90 $(SRCDIR)/quadratures.f90 $(SRCDIR)/Vect_L.f90 $(SRCDIR)/mat_A.f90 $(SRCDIR)/resolution_system.f90 $(SRCDIR)/ecriture_vtk.f90 $(SRCDIR)/erreur.f90 $(SRCDIR)/main.f90
OBJ := $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SRC)) 
EXE := exe

# Flags de compilation
FCFLAGS :=  -O0
MODFLAGS := -J$(MODDIR)
LFLAGS := -llapack

# Règle de construction du programme
all: $(EXE)

# Règle pour générer l'exécutable
$(EXE): $(OBJ)
	$(FC) $(FCFLAGS)  $^ -o $@ $(LFLAGS)

# Règle générique pour la compilation des fichiers source en fichiers objets
$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	@mkdir -p $(@D)
	$(FC) $(FCFLAGS) $(MODFLAGS) -c $< -o $@ 

$(shell mkdir -p $(MODDIR))

# Nettoyer les fichiers objets et l'exécutable
clean:
	rm -rf $(OBJDIR)/*.o $(MODDIR)/*.mod $(EXE)

	
run: $(EXE)
	./$(EXE)	
	
.PHONY: all clean run
	
