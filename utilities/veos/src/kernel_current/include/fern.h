/****************************************************************************************
 * file: fern.h										*
 *											*
 * February 25, 1992: implementation of the Fractal Entity Relativity Node for veos.	*
 * 	       										*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/

/****************************************************************************************
 * Copyright (C) 1992  Human Interface Technology Lab, Seattle				*
 ****************************************************************************************/


/*--------------------------------------------------------------------------------*
				  Useful Externs
 *--------------------------------------------------------------------------------*/

extern LVAL xsendmsg0();
extern LVAL s_unbound;
extern LVAL true;
extern LVAL s_stderr;
extern LVAL s_quote;

extern void Native_NextMsg();

/*--------------------------------------------------------------------------------*
			      Fern Data Structures
 *--------------------------------------------------------------------------------*/

typedef struct stmpnode {
    
    str63		sHost;
    int			iPort;
    float		fData;
    struct stmpnode	*pNext;

    } TStampEntRec,
      *TPStampEntRec,
      **THStampEntRec;

typedef TPStampEntRec	TStampEntHash[13];

/*--------------------------------------------------------------------------------*
				    Defines
 *--------------------------------------------------------------------------------*/

#define FBASE_HASH_HOST(sHost) ((sHost[0] - 'a') / 2)
#define FBASE_HASH_HIT(pXVect, pFNode) \
(getfixnum(getelement(pXVect, 1)) == (pFNode)->iPort && \
 strcmp(getstring(getelement(pXVect, 0)), (pFNode)->sHost) == 0)

/*--------------------------------------------------------------------------------*/
