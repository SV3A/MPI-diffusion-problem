# High Performance Computing F19 (41391)
# Svend E. Andersen (s133037)

# Define compilation flags
DEBUG ?= 0
ifeq ($(DEBUG), 1)
	FFLAGS = -g -C
else
	FFLAGS = -O3 -fast
endif

# Define program name, directories and objects:
TARGET = ./bin
SRCDIR = ./src
OBJDIR = ./objs
MODDIR = ./mods
OBJ = mod_globdata.o mod_alloc_field.o mod_timetools.o mod_io.o \
			mod_iteration.o main.o sub_init.o 

VPATH = $(SRCDIR):$(OBJDIR)

# Libraries and their paths
LIBS =
LIBPATH =

# Define compiler and suffixes
COMPILE = mpif90
LINK = mpif90

.SUFFIXES: .f90
%.o:%.mod

# Compile rule
.f90.o:
	@mkdir -p $(OBJDIR) $(MODDIR)
	$(COMPILE) $(FFLAGS) -c -moddir=$(MODDIR) -o $(OBJDIR)/$@ $<

# Link rule:
$(TARGET): $(OBJ)
	$(LINK) $(FFLAGS) -I$(MODDIR) -o $(TARGET) $(addprefix $(OBJDIR)/, $(OBJ))\
		$(LIBPATH) $(LIBS)
	chmod go+rx $(TARGET)

# Dependencies:
main.o:               mod_globdata.o mod_io.o mod_timetools.o mod_iteration.o\
	sub_init.o 
sub_init.o:           mod_globdata.o mod_alloc_field.o mod_io.o
mod_iteration.o:      mod_globdata.o mod_alloc_field.o
mod_alloc_field.o:    mod_globdata.o
mod_io.o:             mod_globdata.o
mod_timetools.o:      mod_globdata.o

.PHONY: clean cleanapp

# Clean-up rule:
clean:
	rm -rf $(OBJDIR) $(MODDIR)
cleanapp:
	rm *.ps *.dat
