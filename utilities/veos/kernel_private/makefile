# *****************************************************************************
#
# VEOS 2.0 Copyright (C) 1992 Geoffrey P. Coco,
# Human Interface Technology Laboratory
# 											
# This program is free software; you use it under the terms of the
# VEOS LICENSE which can be found the in root of the veos directory tree.
# 											
# *****************************************************************************


#------------------------------------------------------------------------------
#
#  VEOS makefile methodology:
#
#  $(HOME) is the root veos directory.
#  It contains all accociated source, libraries, executables and documents.
#
#  $(HOME)lib/ contains public libraries for the veos kernel, xlisp, etc..
#  $(HOME)bin/ contains public executables, like entity, testshell and xlisp.
#  $(HOME)src/ contains public source for drivers, veos kernel, fern, etc..
#
#  $(HOME)kernel_private/ is the directory reserved for the site
#  administrator of veos.  This is where all veos build commands should
#  be issued.  Build commands are performed by this makefile
#
#  Notice that kernel_private/ contains the original source tree for the
#  veos kernel and fern.  When building a stable version of veos, use the
#  'make public' option.  This command will also copy the private source
#  and libraries to public directories $(PUB_SRC_DIR).  Veos users should
#  always link with public versions of libraries and inspect public
#  versions of the code.
#
#------------------------------------------------------------------------------

HOME = /u1/lisp/xlisp/veos/
COMMAND_DIR = ${HOME}kernel_private/

LISP_INCLUDE_DIR = ${HOME}src/xlisp/xcore/c/
VEOS_INCLUDE_DIR = $(COMMAND_DIR)src/include/
VEOS_INCLUDE_DIRS = -I${VEOS_INCLUDE_DIR} -I${LISP_INCLUDE_DIR}
PUB_INCLUDE_DIR = ${HOME}src/include/

NANCY_SRC_DIR = $(COMMAND_DIR)src/nancy/
SHELL_SRC_DIR = $(COMMAND_DIR)src/shell/
INET_SOCK_SRC_DIR = $(COMMAND_DIR)src/talk/
FERN_SRC_DIR  = $(COMMAND_DIR)src/fern/
PUB_SRC_DIR = ${HOME}src/kernel_current/

LIB_DIR = $(COMMAND_DIR)lib/
PUB_LIB_DIR = ${HOME}lib/

PUB_EXEC_DIR = ${HOME}bin/
EXEC_DIR = $(COMMAND_DIR)bin/

KINCLUDES = ${VEOS_INCLUDE_DIR}kernel.h \
	    ${VEOS_INCLUDE_DIR}world.h  \

#------------------------------------------------------------------------------

include $(HOME)src/machine_specific.mk

# The machine specific file must define the following macros:
# 
#   CC =
#       The c compiler and associated options.
#       Things to include here are options for debugging,
#       optimization, include directories, veos machine specific
#       flags.  VEOS 2.0 currently knows about -D_SG_ for Silicon
#   	Graphics, -D_DEC_ for DECStations, and -D_SUN for Sun.
#   	Do NOT use -c in this macro.
# 
#   AR = 
#       The library archive command and flags.
#       Usually 'ar rcv' is sufficient
# 
#   UPDATE_LIB = 
#       What to do to a library after it has been archived.
#       Usually: ranlib or touch.
# 
#   ASSOC_LIBS =
#       These are extra libraries which get bound in a basic entity.
#       The libraries for xlisp, the veos kernel, and xlisp/veos
#       utitilities are automatically bound to the entity and do not
#       need to be mentioned here.
#
#------------------------------------------------------------------------------

clean:
	- /bin/rm -f $(SHELL_SRC_DIR)*.o
	- /bin/rm -f $(INET_SOCK_SRC_DIR)*.o
	- /bin/rm -f $(NANCY_SRC_DIR)*.o
	- /bin/rm -f $(LIB_DIR)libkernel_proto.a
	- /bin/rm -f $(LIB_DIR)libxvnative_glue_proto.a
	- /bin/rm -f $(FERN_SRC_DIR)*.o
	- /bin/rm -f $(LIB_DIR)libfern.a

#------------------------------------------------------------------------------

public: entity public_kernel public_fern

xlisplib:
	cd $(HOME)src/xlisp; make xlisplib

utils:
	cd $(HOME)src/utils; make utils
	
#------------------------------------------------------------------------------

###
### the kernel
###

kernel: ${LIB_DIR}libkernel_proto.a

${LIB_DIR}libkernel_proto.a: \
		${SHELL_SRC_DIR}shell.o \
		${INET_SOCK_SRC_DIR}socket.o \
		${INET_SOCK_SRC_DIR}shmem.o \
		${INET_SOCK_SRC_DIR}talk.o \
		${NANCY_SRC_DIR}nancy_match.o \
		${NANCY_SRC_DIR}nancy_fundamental.o
	${AR} $@ $?
	${UPDATE_LIB} $@

#------------------------------------------------------------------------------

###
### the native primitive library
###

native: ${LIB_DIR}libxvnative_glue_proto.a

${LIB_DIR}libxvnative_glue_proto.a: \
	    	${SHELL_SRC_DIR}xv_native.o \
	    	${SHELL_SRC_DIR}xv_glutils.o
	${AR} $@ $?
	${UPDATE_LIB} $@

#------------------------------------------------------------------------------
 
###
### the talk module
###

talk: ${INET_SOCK_SRC_DIR}socket.o ${INET_SOCK_SRC_DIR}talk.o

#------------------------------------------------------------------------------

###
### the nancy module
###

nancy: ${NANCY_SRC_DIR}nancy.o ${NANCY_SRC_DIR}nancy_fundamental.o

#------------------------------------------------------------------------------

###
### the shell module
###

shell: ${SHELL_SRC_DIR}shell.o
main: ${SHELL_SRC_DIR}main.o

#------------------------------------------------------------------------------

###
### the fern system library
###

fern: ${LIB_DIR}libfern_proto.a

${LIB_DIR}libfern_proto.a: ${FERN_SRC_DIR}fern.o
	${AR} $@ $?
	${UPDATE_LIB} $@

#------------------------------------------------------------------------------

###
### the test entity shell
###

testshell: ${EXEC_DIR}testshell
	cp ${EXEC_DIR}testshell ${PUB_EXEC_DIR}testshell

${EXEC_DIR}testshell: kernel native main fern
	${CC} ${SHELL_SRC_DIR}main.o \
	      -L${PUB_LIB_DIR} \
	      -L${LIB_DIR} \
	         -lxlisp \
	         -lxvnative_glue_proto \
	         -lkernel_proto \
	         -lfern_proto \
	         -lxvutils \
	      ${ASSOC_LIBS} \
	      -o $@

	
entity: testshell
	cp ${EXEC_DIR}testshell ${PUB_EXEC_DIR}entity

#------------------------------------------------------------------------------

###
### miscellaneous .o files
###

.c.o:
	${CC} -c -o $@ $<

.c: ${KINCLUDES}

zoot: zoot.o
	${CC} -o $@ $? -L$(LIB_DIR) -lkernel_proto

#------------------------------------------------------------------------------

###
### print the kernel
###

print_kernel:
	lpr ${PUB_SRC_DIR}talk/*.c
	lpr ${PUB_SRC_DIR}nancy/*.c
	lpr ${PUB_SRC_DIR}shell/*.c
	lpr ${PUB_SRC_DIR}include/*.h
	lpr $(COMMAND_DIR)makefile
	lpr ${HOME}docs/VEOS_The_Complete_Documentation
	lpr ${HOME}docs/VEOS_Copyright

print_fern:
	lpr ${PUB_SRC_DIR}fern/*.c
	lpr ${PUB_SRC_DIR}fern/*.lsp

#------------------------------------------------------------------------------

###
### the public kernel
###

public_kernel: ${PUB_LIB_DIR}libkernel.a ${PUB_LIB_DIR}libxvnative_glue.a
	cp ${NANCY_SRC_DIR}*.c ${PUB_SRC_DIR}nancy/.
	cp ${SHELL_SRC_DIR}*.c ${PUB_SRC_DIR}shell/.
	cp ${INET_SOCK_SRC_DIR}*.c ${PUB_SRC_DIR}talk/.
	cp ${VEOS_INCLUDE_DIR}*.h ${PUB_SRC_DIR}include/.
	cp ${VEOS_INCLUDE_DIR}world.h ${HOME}src/include/.

public_fern: ${PUB_LIB_DIR}libfern.a
	cp ${FERN_SRC_DIR}* ${PUB_SRC_DIR}fern/.

${PUB_LIB_DIR}libkernel.a: kernel
	cp ${LIB_DIR}libkernel_proto.a $@
	${UPDATE_LIB} $@

${PUB_LIB_DIR}libxvnative_glue.a: native
	cp ${LIB_DIR}libxvnative_glue_proto.a $@
	${UPDATE_LIB} $@

${PUB_LIB_DIR}libfern.a: fern
	cp ${LIB_DIR}libfern_proto.a $@
	${UPDATE_LIB} $@

#------------------------------------------------------------------------------


