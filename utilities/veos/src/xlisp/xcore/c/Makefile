#
#	Xlisp compilation control
#


#
#	Xlisp compilation control
#

#
#	Xlisp compilation control
#

include ../../Makefile.basic

XLISP_LIB = $(XLISP_LIB_DIR)/libxlisp.a
xlisplib: $(XLISP_LIB)


HEADER=
INCLUDES= 

XLISPLIBOBJS= \
  $(XLISP_LIB)(unixstuff.o) \
  $(XLISP_LIB)(xlbfun.o) \
  $(XLISP_LIB)(xlcont.o) \
  $(XLISP_LIB)(xldbug.o) \
  $(XLISP_LIB)(xldmem.o) \
  $(XLISP_LIB)(xleval.o) \
  $(XLISP_LIB)(xlfio.o) \
  $(XLISP_LIB)(xlftab.o) \
  $(XLISP_LIB)(xlglob.o) \
  $(XLISP_LIB)(xlimage.o) \
  $(XLISP_LIB)(xlinit.o) \
  $(XLISP_LIB)(xlio.o) \
  $(XLISP_LIB)(xljump.o) \
  $(XLISP_LIB)(xllist.o) \
  $(XLISP_LIB)(xlmath.o) \
  $(XLISP_LIB)(xlobj.o) \
  $(XLISP_LIB)(xlpp.o) \
  $(XLISP_LIB)(xlprin.o) \
  $(XLISP_LIB)(xlread.o) \
  $(XLISP_LIB)(xlstr.o) \
  $(XLISP_LIB)(xlstruct.o) \
  $(XLISP_LIB)(xlsubr.o) \
  $(XLISP_LIB)(xlsym.o) \
  $(XLISP_LIB)(xlsys.o) \
  $(XLISP_LIB)(xmain.o)

$(INCLUDES): ../../xmodules.h xlisp.h xldmem.h


${XLISPLIBOBJS}:
	${CC} -c ${CFLAGS} ${INCLUDES} $<
	${LD} -r $*.o
	/bin/mv a.out $*.o
	${AR} ${XLISPARFLAGS} $@ $*.o
	/bin/rm $*.o


$(XLISP_LIB): ${XLISPLIBOBJS} 
	${XLISPRANLIB} $@
	cp $@ $(VEOS_LIB_DIR)/.
	${XLISPRANLIB} $(VEOS_LIB_DIR)/libxlisp.a

