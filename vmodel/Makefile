#     File patched for g77 compilation
#     Scott Pearse / email: scott.pearse@gmail.com / web: http://www.linkedin.com/in/scottpearse
#
#
# makefile for VMODEL
#
EXEDIR=../bin/
PLTLIB=../pltlib/
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
VMODEL_OBJS= main.o plt.o misc.o blkdat.o $(PLTLIB)/pltsub.o $(PLTLIB)/pltlib.o
XVMODEL_OBJS= main.o plt.o misc.o blkdat.o $(PLTLIB)/pltsub.o $(PLTLIB)/xpltlib.o $(PLTLIB)/xbuplot.o
#
# Executable name
#
VMODEL_EXEC=$(EXEDIR)vmodel
XVMODEL_EXEC=$(EXEDIR)xvmodel
#
vmodel: $(VMODEL_EXEC)
$(VMODEL_EXEC): ${VMODEL_OBJS}
#	unilink ${VMODEL_OBJS}
	f77 -o main ${VMODEL_OBJS}
	mv main ${VMODEL_EXEC}
#
xvmodel: $(XVMODEL_EXEC)
$(XVMODEL_EXEC): ${XVMODEL_OBJS}
	f77 -o main ${XVMODEL_OBJS} -L/usr/lib/X11 -lX11
	mv main ${XVMODEL_EXEC}
#
# dependencies
#
main.o   :  vmodel.par vmodel.com
plt.o    :  vmodel.par vmodel.com
misc.o   :  vmodel.par vmodel.com
blkdat.o :  vmodel.par vmodel.com
 
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

