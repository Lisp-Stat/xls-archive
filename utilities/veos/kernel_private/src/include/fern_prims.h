/****************************************************************************************
 * file: fern_prims.h									*
 *											*
 * February 25, 1992: implementation of the Fractal Entity Relativity Node for veos.	*
 * 	       										*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/

/****************************************************************************************
 * Copyright (C) 1992  Human Interface Technology Lab, Seattle				*
 ****************************************************************************************/


/*--------------------------------------------------------------------------------*/

#ifdef FERN_LOAD

DEFINE_SUBR("FCON-GO", Fbase_fcon_go)
DEFINE_SUBR("FCON-LOCAL-UNGO", Fbase_fcon_local_ungo)
DEFINE_SUBR("FCON-TIME", Fbase_fcon_time)
DEFINE_SUBR("FBASE-INIT", Fbase_Init)
DEFINE_SUBR("FBASE-NEW-HTAB", Fbase_Hash_NewTab)
DEFINE_SUBR("FBASE-PUT-HASH", Fbase_Hash_AddUid)
DEFINE_SUBR("FBASE-GET-HASH", Fbase_Hash_RemoveUid)
DEFINE_SUBR("FBASE-HASH", Fbase_Hash_HashUid)
DEFINE_SUBR("FE-COPY.INT.SUBS", Fbase_CopyIntSubs)
DEFINE_SUBR("FBASE-INIT-COPY.INT.SUBS", Fbase_Init_CopyIntSubs)
DEFINE_SUBR("FE-COPY.BNDRY.VRT", Fbase_CopyBndryVrt)
DEFINE_SUBR("FBASE-INIT-COPY.BNDRY.VRT", Fbase_Init_CopyBndryVrt)

#endif


#ifdef FERN_DEFS

extern LVAL Fbase_fcon_go();
extern LVAL Fbase_fcon_local_ungo();
extern LVAL Fbase_fcon_time();
extern LVAL Fbase_Init();
extern LVAL Fbase_Hash_NewTab();
extern LVAL Fbase_Hash_AddUid();
extern LVAL Fbase_Hash_RemoveUid();
extern LVAL Fbase_Hash_HashUid();
extern LVAL Fbase_CopyIntSubs();
extern LVAL Fbase_CopyBndryVrt();
extern LVAL Fbase_Init_CopyIntSubs();
extern LVAL Fbase_Init_CopyBndryVrt();

#endif

/*--------------------------------------------------------------------------------*/
