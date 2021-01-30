/****************************************************************************************
 *											*
 * file: xv_native_prims.h								*
 *											*
 * the xlisp include file for integration with VEOS native prims.			*
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



/****************************************************************************************/

#ifdef VEOS_NATIVE_LOAD

DEFINE_SUBR("VPUT", Native_Put  )
DEFINE_SUBR("VGET", Native_Get  )
DEFINE_SUBR("VCOPY", Native_Copy )
DEFINE_SUBR("VINIT", Native_Init )
DEFINE_SUBR("VCLOSE", Native_Close )
DEFINE_SUBR("VTASK", Native_Task )
DEFINE_SUBR("VTHROW", Native_Throw )
DEFINE_SUBR("VCATCH", Native_Catch )
DEFINE_SUBR("VNOSIGNALS", Native_NoSignals )
DEFINE_SUBR("VBUGS", Native_Bugs )
DEFINE_SUBR("VZOOT", Native_Zoot )
DEFINE_SUBR("VMINTIME", Native_MinTime )

#endif


#ifdef VEOS_NATIVE_DEFS

extern LVAL Native_Put();
extern LVAL Native_Get();
extern LVAL Native_Copy();
extern LVAL Native_Init();
extern LVAL Native_Close();
extern LVAL Native_Task();
extern LVAL Native_Throw();
extern LVAL Native_Catch();
extern LVAL Native_NoSignals();
extern LVAL Native_Bugs();
extern LVAL Native_Zoot();
extern LVAL Native_MinTime();

#endif

/****************************************************************************************/
