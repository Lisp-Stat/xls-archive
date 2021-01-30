/****************************************************************************************
 *											*
 * file: main.c										*
 *											*
 * An example controlling of veos using xlisp interface.				*
 *											*
 * creation: December, 1991								*
 *											*
 *											*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/

/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/


#include "world.h"

extern xmain();


/****************************************************************************************
 * main
 * launchpad of any stand-alone program
 ****************************************************************************************/
main(argc, argv)
    int		argc;
    char	*argv[];
{
    /** call the xlisp controller, never returns **/
    xmain(argc, argv);
    }


/****************************************************************************************
 * xlinclude_hybrid_prims
 * lisp calls this function to load user-defined lisp primitives
 ****************************************************************************************/
xlinclude_hybrid_prims()
{
    /** load veos native lisp primitive entries **/
    Shell_LoadNativePrims();
    Fern_LoadPrims();
    XVUtils_LoadPrims();
    }



/****************************************************************************************
 * xlshutdown_hybrid
 * lisp calls this function before graceful exit
 ****************************************************************************************/
xlshutdown_hybrid()
{
    /** let the kernel unwind **/
    Kernel_Shutdown();
    }




