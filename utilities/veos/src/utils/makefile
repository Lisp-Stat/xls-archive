# ***************************************************************************************
#  Copyright (C) 1992  Human Interface Technology Lab, Seattle				*
# ***************************************************************************************


#--------------------------------------------------------------------------------------

HOME = /u1/lisp/veos/

LISP_INCLUDE_DIR = ${HOME}src/xlisp/xcore/c/
PUB_INCLUDE_DIR = ${HOME}src/include/
VEOS_INCLUDE_DIRS = -I${PUB_INCLUDE_DIR} -I${LISP_INCLUDE_DIR}

LISP_LIB_DIR = /home/xlisp/lib/
PUB_LIB_DIR = ${HOME}lib/

PUB_EXEC_DIR = ${HOME}bin/

#--------------------------------------------------------------------------------------

include $(HOME)src/machine_specific.mk

#--------------------------------------------------------------------------------------

clean:
	- /bin/rm -f $(UTIL_SRC_DIR)*.o
	- /bin/rm -f $(PUB_LIB_DIR)libxvutils.a

#--------------------------------------------------------------------------------------

###
### an xlisp utils library
###

utils: ${PUB_LIB_DIR}libxvutils.a

${PUB_LIB_DIR}libxvutils.a: \
	    ${UTIL_SRC_DIR}xv_utils.o \
	    ${UTIL_SRC_DIR}error_check.o \
	    ${UTIL_SRC_DIR}xform_prims.o 
	${AR} $@ $?
	${UPDATE_LIB} $@

#--------------------------------------------------------------------------------------

###
### miscellaneous .o files
###

.c.o:
	${CC} -c -o $@ $<

.c:

#--------------------------------------------------------------------------------------


