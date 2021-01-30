#
#		"... and one Ring to rule them all."
#
# Master makefile to build xlisp-plus-selected-extension-modules.
#

all:	bare

xlisplib:
	cd xcore/c;        $(MAKE) -${MAKEFLAGS}                 xlisplib
    
bare: xlisplib
	cd xexec/c;        $(MAKE) -${MAKEFLAGS}                 bare

clean:
	cd xcore/c;       ${MAKE} -${MAKEFLAGS}                 clean








