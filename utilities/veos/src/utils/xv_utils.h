/****************************************************************************************
 *											*
 * file: xv_utils.c									*
 *											*
 * Sundry lisp utils for the veos project						*
 *											*
 * creation: March 28, 1991								*
 *											*
 *											*
 * Includes utilities by:								*
 *											*
 *	Geoff Coco									*
 *	Dav Lion									*
 *	Andy McDonald									*
 *	Fran Taylor									*
 *											*
 ****************************************************************************************/


/****************************************************************************************
 * Copyright (C) 1992  Human Interface Technology Lab, Seattle				*
 ****************************************************************************************/


#ifdef UTIL_LOAD
DEFINE_SUBR("READ-TIME", read_time)
DEFINE_SUBR("SPRINTF", native_sprintf )
DEFINE_SUBR("PRINTF", native_printf )
DEFINE_SUBR("PRINTF1", native_printf1 )
DEFINE_SUBR("SSCANF", native_sscanf )
#endif

#ifdef UTIL_DEFS
extern LVAL read_time();
extern LVAL native_sprintf();
extern LVAL native_printf();
extern LVAL native_printf1();
extern LVAL native_sscanf();
#endif
