# Variables
FC := gfortran
SRCDIR := src
OBJDIR := obj
MODDIR := mod
SRC := $(SRCDIR)/const_var.f90 $(SRCDIR)/recup_data.f90 $(SRCDIR)/fonctions.f90 $(SRCDIR)/main.f90
OBJ := $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SRC)) 
EXE := exe

# Flags de compilation
FCFLAGS :=  -O3
MODFLAGS := -J$(MODDIR)

# Règle de construction du programme
all: $(EXE)

# Règle pour générer l'exécutable
$(EXE): $(OBJ)
	$(FC) $(FCFLAGS)  $^ -o $@

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

plotUo: 
	bash script_bash/plotUo.sh
	
	
.PHONY: all clean run plotUo
	
	
