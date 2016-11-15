SHELL       = /bin/sh

exist_modules := $(wildcard ./conf/modules)
ifeq ($(strip $(exist_modules)),)
    $(error "conf/modules file is missing. Run ./configure")
endif

##
## global configuration
##
AR          = ar
ARFLAGS     = -rvu

OPTS        = -O3
#TRAPS       = -g -traceback -ftrapuv -check all -fp-stack-check

#
# Assemble Options
#
FFLAGS      = $(OPTS) $(TRAPS)

F90         = ifort $(FFLAGS)

#
# NEMSIO
#
NEMSIOINC   = -I$(NEMSIO_INC)
NEMSIOLIBS  = $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(W3EMC_LIBd)

##
## local configuration
##

F90_FLAGS  = $(NEMSIOINC)

##
## suffix lists and implicit rules
##
.SUFFIXES: .f90

.f90.o:
	$(F90) -c $< $(F90_FLAGS)

default:
	make all


################################################################################


DNEMSIO.X = dnemsio

TARGETS = $(DNEMSIO.X)

$(DNEMSIO.X): dnemsio.o
	$(F90) -o $@ dnemsio.o $(NEMSIOLIBS)

all: $(TARGETS)

clean:
	rm -f *.o *.mod *.lst lm map
	rm -f $(TARGETS)
	rm -f ./conf/modules
