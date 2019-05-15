#     File patched for g77 compilation
#     Scott Pearse / email: scott.pearse@gmail.com / web: http://www.linkedin.com/in/scottpearse
#
#
# makefile for PLTSYN
#
EXEDIR=../bin/
PLTLIB=../pltlib
#
# compile flags
#
FFLAGS =  -O
CFLAGS =  -O
#
#rules for compiling .f files:
#
.f.o:
	$(COMPILE.f) $(FFLAGS) $< $(INCS)
.c.o:
	$(COMPILE.c) $(CFLAGS) $< $(INCS)
#---------------------------------------------------------------------------
#
# Source objects
#
PLTSYN_OBJS= main.o pltsec.o misc.o blkdat.o $(PLTLIB)/pltsub.o $(PLTLIB)/pltlib.o
XPLTSYN_OBJS= main.o pltsec.o misc.o blkdat.o $(PLTLIB)/pltsub.o $(PLTLIB)/xpltlib.o $(PLTLIB)/xbuplot.o
#
# Executable name
#
PLTSYN_EXEC=$(EXEDIR)pltsyn 
XPLTSYN_EXEC=$(EXEDIR)xpltsyn 
#
pltsyn: $(PLTSYN_EXEC)
$(PLTSYN_EXEC): ${PLTSYN_OBJS}
#	unilink ${PLTSYN_OBJS}
	f77 -o main ${PLTSYN_OBJS}
	mv main ${PLTSYN_EXEC}
#
xpltsyn: $(XPLTSYN_EXEC)
$(XPLTSYN_EXEC): ${XPLTSYN_OBJS}
	f77 -o main ${XPLTSYN_OBJS} -L/usr/lib/X11 -lX11
	mv main ${XPLTSYN_EXEC}
#
# dependencies
#
main.o     :  pltsyn.par pltsyn.com
pltsec.o   :  pltsyn.par pltsyn.com
misc.o     :  pltsyn.par
blkdat.o   :  pltsyn.par pltsyn.com

$(PLTLIB)/pltsub.o: FORCE
	cd $(@D); $(MAKE) $(@F)
FORCE:

$(PLTLIB)/pltlib.o: FORCE
	cd $(@D); $(MAKE) $(@F)
FORCE:

$(PLTLIB)/xpltlib.o: FORCE
	cd $(@D); $(MAKE) $(@F)
FORCE:

$(PLTLIB)/xbuplot.o: FORCE
	cd $(@D); $(MAKE) $(@F)
FORCE:
