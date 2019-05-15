#     File patched for g77 compilation
#     Scott Pearse / email: scott.pearse@gmail.com / web: http://www.linkedin.com/in/scottpearse
#
#
# makefile for RAYPLOT
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
RAYPLOT_OBJS= rayplot.o $(PLTLIB)/pltsub.o $(PLTLIB)/pltlib.o
XRAYPLOT_OBJS= rayplot.o $(PLTLIB)/pltsub.o $(PLTLIB)/xpltlib.o $(PLTLIB)/xbuplot.o
#
# Executable name
#
RAYPLOT_EXEC=$(EXEDIR)rayplot 
XRAYPLOT_EXEC=$(EXEDIR)xrayplot 
#
rayplot: $(RAYPLOT_EXEC)
$(RAYPLOT_EXEC): ${RAYPLOT_OBJS}
	f77 -o rayplot ${RAYPLOT_OBJS}
	mv rayplot ${RAYPLOT_EXEC}
#
xrayplot: $(XRAYPLOT_EXEC)
$(XRAYPLOT_EXEC): ${XRAYPLOT_OBJS}
	f77 -o rayplot ${XRAYPLOT_OBJS} -L/usr/lib/X11 -lX11
	mv rayplot ${XRAYPLOT_EXEC}

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

