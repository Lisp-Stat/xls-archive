/****************************************************************************************
 *											*
 * file: xv_glutils.c									*
 *											*
 * Sundry utilities which serve as glue for xlisp veos primitives.			*
 *											*
 * creation: April 13, 1992								*
 *											*
 *											*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/


/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/



/****************************************************************************************
 * 			             preliminaries					*/

#include <math.h>
#include "xlisp.h"

/* VEOS definitions: */
#include "kernel.h"

#define NATIVE_CODE
#include "xv_native.h"
#undef NATIVE_CODE

/****************************************************************************************/

extern LVAL 	xsendmsg0();
extern LVAL	s_unbound;
extern LVAL	true;
extern LVAL 	xlfatal();

/****************************************************************************************/

boolean		native_bSubstBeenMarked;
boolean		native_bVoidBeenMarked;
boolean		native_bDestruct;

#define SUBST	native_bSubstBeenMarked
#define VOID	native_bVoidBeenMarked
#define MOD	native_bDestruct

/****************************************************************************************/

TVeosErr Native_PatVEltClerical();
extern LVAL ReverseList();

/****************************************************************************************/




/****************************************************************************************
			    Basic Xlisp <--> Nancy Conversion
 ****************************************************************************************/


/****************************************************************************************/
TVeosErr Native_XEltToVElt(pXElt, pVElt)
    LVAL 	pXElt;
    TPElt	pVElt;
{
    TVeosErr	iErr;

    iErr = VEOS_FAILURE;


    /** NIL is the empty grouple **/

    if (null(pXElt)) {
	iErr = Nancy_NewGrouple(&pVElt->u.pGr);
	pVElt->iType = GR_grouple;
	}


    /** case-wise conversion to nancy format **/

    else {
	switch (ntype(pXElt)) {

	case CONS:
	    /** a list becomes a grouple **/
	    iErr = Native_ListToGrouple(pXElt, &pVElt->u.pGr);
	    pVElt->iType = GR_grouple;
	    break;
	    
	case VECTOR:
	    /** a vector becomes a special grouple **/
	    iErr = Native_VectToGrouple(pXElt, &pVElt->u.pGr);
	    pVElt->iType = GR_vector;
	    break;

	case FIXNUM:
	    pVElt->iType = GR_int;
	    pVElt->u.iVal = getfixnum(pXElt);
	    break;
	    
	case FLONUM:
	    pVElt->iType = GR_float;
	    pVElt->u.fVal = (float) getflonum(pXElt);
	    break;
	    
	case STRING:
	    pVElt->iType = GR_string;
	    pVElt->u.pS = strdup((char *) getstring(pXElt));
	    break;
	    
	case SYMBOL:
	    pVElt->iType = GR_prim;
	    pVElt->u.pS = strdup(getstring(getpname(pXElt)));
	    break;
	    
	default:
	    iErr = NATIVE_BADVTYPE;
	    break;

	    }
	}

    return(iErr);

    } /* Native_XEltToVElt */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_ListToGrouple(pList, hGrouple)
    LVAL	pList;
    THGrouple	hGrouple;
{
    TVeosErr	iErr;
    LVAL	pXFinger;
    int		iElt;
    TPGrouple	pGrouple;
    TPElt	pVFinger;

    
    *hGrouple = nil;

    iErr = Nancy_NewGrouple(&pGrouple);
    iElt = 0;

    /** convert each lisp sub-element **/

    pXFinger = pList;
    while (!null(pXFinger) && iErr == VEOS_SUCCESS) {


	/** make room for another grouple element **/
	
	Nancy_NewElementsInGrouple(pGrouple, iElt, 1, GR_unspecified, 0);


	/** do actual element conversion **/

	iErr = Native_XEltToVElt(car(pXFinger), &pGrouple->pEltList[iElt]);


	/** advance element refs **/

	iElt ++;
	pXFinger = cdr(pXFinger);

	} /* while */


    if (iErr == VEOS_SUCCESS)
	*hGrouple = pGrouple;
    else
	Nancy_DisposeGrouple(pGrouple);


    return(iErr);

    } /* Native_ListToGrouple */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_VectToGrouple(pVect, hGrouple)
    LVAL	pVect;
    THGrouple	hGrouple;
{
    TVeosErr	iErr;
    int		iElts, iEltIndex;
    TPGrouple	pGrouple;
    TPElt	pVElt;

    *hGrouple = nil;

    iErr = Nancy_NewGrouple(&pGrouple);


    iElts = getsz(pVect);
    if (iElts > 0 && iErr == VEOS_SUCCESS) {

	/** make enough room for all impending elements **/

	iErr = Nancy_NewElementsInGrouple(pGrouple, 0, iElts, GR_unspecified, 0);



	/** convert each lisp sub-element **/

	iEltIndex = 0; pVElt = pGrouple->pEltList;
	while (iEltIndex < iElts && iErr == VEOS_SUCCESS) {

	    iErr = Native_XEltToVElt(getelement(pVect, iEltIndex), pVElt);

	    iEltIndex ++; pVElt ++;
	    }
	}

    if (iErr == VEOS_SUCCESS)
	*hGrouple = pGrouple;
    else
	Nancy_DisposeGrouple(pGrouple);


    return(iErr);

    } /* Native_VectToGrouple */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_VEltToXElt(pVElt, hXElt)
    TPElt	pVElt;
    LVAL 	*hXElt;
{
    TVeosErr	iErr;


    *hXElt = NIL;

    iErr = VEOS_SUCCESS;

    switch (pVElt->iType) {

    case GR_grouple:
	iErr = Native_GroupleToList(pVElt->u.pGr, hXElt);
	break;
	
    case GR_vector:
	iErr = Native_GroupleToVect(pVElt->u.pGr, hXElt);
	break;

    case GR_int:            
	*hXElt = cvfixnum(pVElt->u.iVal);
	break;
	
    case GR_float:
	*hXElt = cvflonum(pVElt->u.fVal);
	break;
	
    case GR_string:
	*hXElt = cvstring(pVElt->u.pS);
	break;
	
    case GR_prim:
	*hXElt = xlenter(pVElt->u.pS);
	break;

    case GR_unspecified:
	iErr = NATIVE_EMPTYELT;
	break;

    default:	
	iErr = NATIVE_BADXTYPE;
	break;

	}

    return(iErr);

    } /* Native_VEltToXElt */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_GroupleToList(pGrouple, hList)
    TPGrouple	pGrouple;
    LVAL	*hList;
{
    TVeosErr	iErr;
    LVAL	pNewXElt, pList;
    int		iElts, iElt;

    xlstkcheck(2);
    xlsave(pNewXElt);
    xlsave(pList);

    iErr = VEOS_SUCCESS;
    iElts = pGrouple->iElts;
    iElt = iElts - 1;

    while (iElt >= 0 && iErr == VEOS_SUCCESS) {
	
	iErr = Native_VEltToXElt(&pGrouple->pEltList[iElt], &pNewXElt);
	if (iErr == VEOS_SUCCESS)
	    pList = cons(pNewXElt, pList);

	iElt --;
	}

    *hList = pList;

    xlpopn(2);

    return(iErr);

    } /* Native_GroupleToList */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_GroupleToVect(pGrouple, hVect)
    TPGrouple	pGrouple;
    LVAL	*hVect;
{
    TVeosErr	iErr;
    LVAL	pNewXElt, pVect;
    int		iElts, iElt;

    xlstkcheck(2);
    xlsave(pVect);
    xlsave(pNewXElt);

    iErr = VEOS_SUCCESS;
    iElts = pGrouple->iElts;
    iElt = 0;

    pVect = newvector(iElts);

    while (iElt < iElts && iErr == VEOS_SUCCESS) {
	
	iErr = Native_VEltToXElt(&pGrouple->pEltList[iElt], &pNewXElt);
	if (iErr == VEOS_SUCCESS)
	    setelement(pVect, iElt, pNewXElt);

	iElt ++;
	}

    *hVect = pVect;

    xlpopn(2);

    return(iErr);

    } /* Native_GroupleToVect */
/****************************************************************************************/



/****************************************************************************************
			 Timestamped Xlisp <--> Nancy Conversion
 ****************************************************************************************/

/****************************************************************************************/
TVeosErr Native_NewVEltToXElt(pVElt, hXElt, time)
    TPElt	pVElt;
    LVAL 	*hXElt;
    TTimeStamp	time;
{
    TVeosErr	iErr;

    *hXElt = NIL;
    iErr = NATIVE_STALE;

    if (TIME_LESS_THAN(pVElt->tLastMod, time)) {

	/** old data, retrieve only contents of containers 
	 **/
	if (pVElt->iType == GR_grouple)
	    iErr = Native_NewGroupleToList(pVElt->u.pGr, hXElt, time);
	
	else if (pVElt->iType == GR_vector)
	    iErr = Native_NewGroupleToVect(pVElt->u.pGr, hXElt, time);
	}

    else {
	/** new data, retrieve completely **/

	switch (pVElt->iType) {
	    
	case GR_grouple:            
	    iErr = Native_GroupleToList(pVElt->u.pGr, hXElt);
	    break;

	case GR_vector:            
	    iErr = Native_GroupleToVect(pVElt->u.pGr, hXElt);
	    break;

	case GR_int:            
	    *hXElt = cvfixnum(pVElt->u.iVal);
	    iErr = VEOS_SUCCESS;
	    break;
	    
	case GR_float:
	    *hXElt = cvflonum(pVElt->u.fVal);
	    iErr = VEOS_SUCCESS;
	    break;
	    
	case GR_string:
	    *hXElt = cvstring(pVElt->u.pS);
	    iErr = VEOS_SUCCESS;
	    break;
	    
	case GR_prim:
	    *hXElt = xlenter(pVElt->u.pS);
	    iErr = VEOS_SUCCESS;
	    break;
	    
	case GR_unspecified:
	    iErr = NATIVE_EMPTYELT;
	    break;
	    
	default:	
	    iErr = NATIVE_BADXTYPE;
	    break;
	    
	    }
	}

    return(iErr);

    } /* Native_NewVEltToXElt */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_NewGroupleToList(pGrouple, hList, time)
    TPGrouple	pGrouple;
    LVAL	*hList;
    TTimeStamp	time;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    LVAL	pNewXElt, pList;
    int		iElts, iElt;
    TPElt	pVElt;
    boolean	bStale = TRUE;

    xlsave1(pNewXElt);
    xlsave1(pList);

    iElts = pGrouple->iElts;
    iElt = iElts - 1;

    while (iElt >= 0) {
	
	/** determine if caller has already seen this data **/
	
	iErr = Native_NewVEltToXElt(&pGrouple->pEltList[iElt], &pNewXElt, time);
	if (iErr == VEOS_SUCCESS) {
	    /** assume caller has locked this ptr **/

	    pList = cons(pNewXElt, pList);
	    bStale = FALSE;
	    }

	else if (iErr == NATIVE_STALE)
	    iErr = VEOS_SUCCESS;

	else
	    break;

	iElt --;
	}

    if (iErr == VEOS_SUCCESS) {
	if (bStale)
	    iErr = NATIVE_STALE;

	*hList = pList;
	}

    xlpopn(2);

    return(iErr);

    } /* Native_NewGroupleToList */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_NewGroupleToVect(pGrouple, hVect, time)
    TPGrouple	pGrouple;
    LVAL	*hVect;
    TTimeStamp	time;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    LVAL	pNewXElt, pVect;
    int		iElts, iElt;
    boolean	bStale = TRUE;

    xlsave1(pNewXElt);
    xlsave1(pVect);

    iElts = pGrouple->iElts;
    pVect = newvector(iElts);

    iElt = 0;

    while (iElt < iElts) {
	
	iErr = Native_NewVEltToXElt(&pGrouple->pEltList[iElt], &pNewXElt, time);
	if (iErr == VEOS_SUCCESS) {

	    /** assume caller has locked this ptr **/

	    setelement(pVect, iElt, pNewXElt);
	    bStale = FALSE;
	    }

	else if (iErr == NATIVE_STALE)
	    iErr = VEOS_SUCCESS;

	else
	    break;

	iElt ++;
	}

    if (iErr == VEOS_SUCCESS) {
	if (bStale)
	    iErr = NATIVE_STALE;
	
	*hVect = pVect;
	}

    xlpopn(2);

    return(iErr);

    } /* Native_NewGroupleToVect */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_XEltToNewVElt(pXElt, pVElt, time)
    LVAL 	pXElt;
    TPElt	pVElt;
    TTimeStamp	time;
{
    TVeosErr	iErr;

    iErr = VEOS_SUCCESS;


    /** NIL is the empty grouple **/

    if (null(pXElt)) {
	pVElt->iType = GR_grouple;
	iErr = Nancy_NewGrouple(&pVElt->u.pGr);
	}

    /** case-wise conversion to nancy format **/

    else {
	switch (ntype(pXElt)) {

	case CONS:
	    /** a list becomes a grouple **/
	    iErr = Native_ListToNewGrouple(pXElt, &pVElt->u.pGr, time);
	    pVElt->iType = GR_grouple;
	    break;
	    
	case VECTOR:
	    /** a vector becomes a special grouple **/
	    iErr = Native_VectToNewGrouple(pXElt, &pVElt->u.pGr, time);
	    pVElt->iType = GR_vector;
	    break;

	case FIXNUM:
	    pVElt->iType = GR_int;
	    pVElt->u.iVal = getfixnum(pXElt);
	    break;
	    
	case FLONUM:
	    pVElt->iType = GR_float;
	    pVElt->u.fVal = (float) getflonum(pXElt);
	    break;
	    
	case STRING:
	    pVElt->iType = GR_string;
	    pVElt->u.pS = strdup((char *) getstring(pXElt));
	    break;
	    
	case SYMBOL:
	    pVElt->iType = GR_prim;
	    pVElt->u.pS = strdup(getstring(getpname(pXElt)));
	    break;
	    
	default:
	    iErr = NATIVE_BADVTYPE;
	    break;

	    }
	}

    pVElt->tLastMod = time;

    return(iErr);

    } /* Native_XEltToNewVElt */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_ListToNewGrouple(pList, hGrouple, time)
    LVAL	pList;
    THGrouple	hGrouple;
    TTimeStamp	time;
{
    TVeosErr	iErr;
    LVAL	pXFinger;
    int		iElt;
    TPGrouple	pGrouple;
    TPElt	pVFinger;

    xlsave1(pXFinger);
    
    *hGrouple = nil;

    iErr = Nancy_NewGrouple(&pGrouple);
    iElt = 0;


    /** convert each lisp sub-element **/

    pXFinger = pList;
    while (!null(pXFinger) && iErr == VEOS_SUCCESS) {


	/** make room for another grouple element **/
	
	Nancy_NewElementsInGrouple(pGrouple, iElt, 1, GR_unspecified, 0);


	/** do actual element conversion **/

	iErr = Native_XEltToNewVElt(car(pXFinger), &pGrouple->pEltList[iElt], time);


	/** advance element refs **/

	iElt ++;
	pXFinger = cdr(pXFinger);

	} /* while */


    if (iErr == VEOS_SUCCESS)
	*hGrouple = pGrouple;
    else
	Nancy_DisposeGrouple(pGrouple);

    xlpop();

    return(iErr);

    } /* Native_ListToNewGrouple */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_VectToNewGrouple(pVect, hGrouple, time)
    LVAL	pVect;
    THGrouple	hGrouple;
    TTimeStamp	time;
{
    TVeosErr	iErr;
    int		iElts, iEltIndex;
    TPGrouple	pGrouple;


    *hGrouple = nil;

    iErr = Nancy_NewGrouple(&pGrouple);


    iElts = getsz(pVect);
    if (iElts > 0 && iErr == VEOS_SUCCESS) {

	/** make enough room for all impending elements **/

	iErr = Nancy_NewElementsInGrouple(pGrouple, 0, iElts, GR_unspecified, 0);



	/** convert each lisp sub-element **/

	iEltIndex = 0;
	while (iEltIndex < iElts && iErr == VEOS_SUCCESS) {

	    iErr = Native_XEltToNewVElt(getelement(pVect, iEltIndex),
					&pGrouple->pEltList[iEltIndex], time);
	    iEltIndex ++;
	    }
	}

    if (iErr == VEOS_SUCCESS)
	*hGrouple = pGrouple;
    else
	Nancy_DisposeGrouple(pGrouple);


    return(iErr);

    } /* Native_VectToNewGrouple */
/****************************************************************************************/


/****************************************************************************************
			   Pattern Xlisp <--> Nancy Conversion
 ****************************************************************************************/


/****************************************************************************************/
TVeosErr Native_GetPatternArg(hPattern, iMatchFlag)
    THGrouple	hPattern;
    int		iMatchFlag;
{	
    LVAL	pXElt;
    TVeosErr	iErr;


    SUBST = FALSE;
    VOID = FALSE;
    MOD = (iMatchFlag == NANCY_ReplaceMatch);


    /** get lisp pattern list **/

    pXElt = xlgalist();


    /** dispatch lisp->veos conversion **/

    iErr = Native_PatListToGrouple(pXElt, hPattern);

#ifndef OPTIMAL
    if (iErr == VEOS_SUCCESS) {
	if (iMatchFlag == NANCY_ReplaceMatch) {
	    if (!SUBST && !VOID)
		iErr = NATIVE_NOREPLACEMARK;
	    }
	else {
	    if (VOID)
		iErr = NATIVE_NOVOID;
	    else if (!SUBST)
		iErr = NATIVE_NOFETCHMARK;
	    }
	}
#endif

    return(iErr);

    } /* Native_GetPatternArg */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_PatXEltToVElt(pXElt, pVElt)
    LVAL 	pXElt;
    TPElt	pVElt;
{
    TVeosErr	iErr;

    iErr = VEOS_SUCCESS;


    /** NIL is the empty grouple **/

    if (null(pXElt)) {
	iErr = Nancy_NewGrouple(&pVElt->u.pGr);
	pVElt->iType = GR_grouple;
	}


    /** case-wise conversion to nancy format **/

    else {
	switch (ntype(pXElt)) {

	case CONS:
	    /** a list becomes a grouple **/
	    iErr = Native_PatListToGrouple(pXElt, &pVElt->u.pGr);
	    pVElt->iType = GR_grouple;
	    break;
	    
	case VECTOR:
	    /** a vector becomes a special grouple **/
	    iErr = Native_PatVectToGrouple(pXElt, &pVElt->u.pGr);
	    pVElt->iType = GR_vector;
	    break;

	case FIXNUM:
	    pVElt->iType = GR_int;
	    pVElt->u.iVal = getfixnum(pXElt);
	    break;
	    
	case FLONUM:
	    pVElt->iType = GR_float;
	    pVElt->u.fVal = (float) getflonum(pXElt);
	    break;
	    
	case STRING:
	    pVElt->iType = GR_string;
	    pVElt->u.pS = strdup((char *) getstring(pXElt));
	    break;
	    
	case SYMBOL:
	    iErr = Native_ConvertSymbol(pXElt, pVElt);
	    break;
	    
	default:
	    iErr = NATIVE_BADVTYPE;
	    break;
	    }
	}

    return(iErr);

    } /* Native_PatXEltToVElt */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_PatListToGrouple(pList, hGrouple)
    LVAL	pList;
    THGrouple	hGrouple;
{
    TVeosErr	iErr;
    LVAL	pXFinger;
    int		iElt;
    TPGrouple	pGrouple;
    TPElt	pVFinger;
    TPatStatRec	patPB;
    TElt	eltNew;

    
    /******************
     ** setup locals **
     ******************/

    *hGrouple = nil;
    iErr = Nancy_NewGrouple(&pGrouple);

    /** by default, a grouple is literally an ordered list of elements.
     **	in some cases, a pattern grouple can specifiy an order-blind element
     ** collection.  in other words, a content-dependent-pattern.
     **/
    patPB.bOrdered = TRUE;

    /** prepare to check for pattern format inconsistencies **/

    patPB.bExpContent = FALSE;
    patPB.bExpOrder = FALSE;
    patPB.bMarkedWithin = FALSE;
    patPB.bTouchedWithin = FALSE;
    
    patPB.bMarkNextElt = FALSE;
    patPB.bTouchNextElt = FALSE;
    patPB.bMustEnd = FALSE;
    patPB.bGetAnother = FALSE;


    /***********************************
     ** convert each lisp sub-element **
     ***********************************/

    pXFinger = pList;
    while (!null(pXFinger)) {

	eltNew = NIL_ELT;

	/** do actual element conversion **/
	
	iErr = Native_PatXEltToVElt(car(pXFinger), &eltNew);
	if (iErr != VEOS_SUCCESS)
	    break;
	
	iErr = Native_PatVEltClerical(&eltNew, &patPB);
	if (iErr != VEOS_SUCCESS)
	    break;
	
	if (patPB.bGetAnother) {
	    
	    /** this elt was actually a modifier elt for next one.
	     ** prepare for caller forgetting to pass next elt
	     **/
	    iErr = NATIVE_NOTEND;
	    }
	
	else {
	    /** place converted nancy element into dest grouple **/
	    
	    Nancy_NewElementsInGrouple(pGrouple, pGrouple->iElts,
				       1, GR_unspecified, 0);
	    pGrouple->pEltList[pGrouple->iElts - 1] = eltNew;
	    }	    
	
	
	/** advance element refs **/
	
	pXFinger = cdr(pXFinger);
	} /* while */

    if (iErr != VEOS_SUCCESS)
	Nancy_DisposeGrouple(pGrouple);

    else {
	if (!patPB.bOrdered)
	    SETFLAG(NANCY_ContentMask, pGrouple->iFlags);
	if (patPB.bMarkedWithin)
	    SETFLAG(NANCY_MarkWithinMask, pGrouple->iFlags);
	if (patPB.bTouchedWithin)
	    SETFLAG(NANCY_TouchWithinMask, pGrouple->iFlags);

	*hGrouple = pGrouple;
	}

    return(iErr);

    } /* Native_PatListToGrouple */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_PatVectToGrouple(pVect, hGrouple)
    LVAL	pVect;
    THGrouple	hGrouple;
{
    TVeosErr	iErr;
    LVAL	pXFinger;
    int		iXElts, iXEltIndex;
    TPGrouple	pGrouple;
    TPatStatRec	patPB;
    TElt	eltNew;

    /******************
     ** setup locals **
     ******************/

    *hGrouple = nil;
    iErr = Nancy_NewGrouple(&pGrouple);

    /** by default, a grouple is literally an ordered list of elements.
     **	in some cases, a pattern grouple can specifiy an order-blind element
     ** collection.  in other words, a content-dependent-pattern.
     **/
    patPB.bOrdered = TRUE;

    /** prepare to check for pattern format inconsistencies **/

    patPB.bExpContent = FALSE;
    patPB.bExpOrder = FALSE;
    patPB.bMarkedWithin = FALSE;
    patPB.bTouchedWithin = FALSE;
    
    patPB.bMarkNextElt = FALSE;
    patPB.bTouchNextElt = FALSE;
    patPB.bMustEnd = FALSE;
    patPB.bGetAnother = FALSE;

    iXElts = getsz(pVect);
    if (iXElts > 0 && iErr == VEOS_SUCCESS) {

	/***********************************
	 ** convert each lisp sub-element **
	 ***********************************/
	
	iXEltIndex = 0;
	while (iXEltIndex < iXElts) {
	    

	    /** cache current vector element **/
	    
	    pXFinger = getelement(pVect, iXEltIndex);
	    eltNew = NIL_ELT;
	    
	    /** do actual element conversion **/
	    
	    iErr = Native_PatXEltToVElt(pXFinger, &eltNew);
	    if (iErr != VEOS_SUCCESS)
		break;
	    
	    iErr = Native_PatVEltClerical(&eltNew, &patPB);
	    if (iErr != VEOS_SUCCESS)
		break;
	    
	    if (patPB.bGetAnother) {

		/** this elt was actually a modifier elt for next one.
		 ** prepare for caller forgetting to pass next elt
		 **/
		iErr = NATIVE_NOTEND;
		}

	    else {
		/** place converted nancy element into dest grouple **/
		
		Nancy_NewElementsInGrouple(pGrouple, pGrouple->iElts,
					   1, GR_unspecified, 0);
		pGrouple->pEltList[pGrouple->iElts - 1] = eltNew;
		}	    


	    /** advance element refs **/
		
	    iXEltIndex ++;

	    } /* while */
	}

    if (iErr != VEOS_SUCCESS)
	Nancy_DisposeGrouple(pGrouple);

    else {
	if (!patPB.bOrdered)
	    SETFLAG(NANCY_ContentMask, pGrouple->iFlags);
	if (patPB.bMarkedWithin)
	    SETFLAG(NANCY_MarkWithinMask, pGrouple->iFlags);
	if (patPB.bTouchedWithin)
	    SETFLAG(NANCY_TouchWithinMask, pGrouple->iFlags);

	*hGrouple = pGrouple;
	}

    return(iErr);

    } /* Native_PatVectToGrouple */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_PatVEltClerical(pVElt, pStats)
    TPElt		pVElt;
    TPPatStatRec	pStats;
{
    TVeosErr		iErr = VEOS_SUCCESS;

#ifndef OPTIMAL
    if (pStats->bMustEnd)
	iErr = NATIVE_NOTEND;

    else {
	/** catch possible undefined expressions **/
	
	switch (pVElt->iType) {
	    
	case GR_these:
	    if (pStats->bExpContent)
		iErr = NATIVE_CANTMIX;
	    break;
	    
	case GR_theseall:
	    if (pStats->bExpContent)
		iErr = NATIVE_CANTMIX;
	    break;
	    
	case GR_some:
	    iErr = NATIVE_NOSTARN;
	    break;
	    
	case GR_any:
	    if (pStats->bExpOrder)
		iErr = NATIVE_CANTMIX;
	    break;
	    
	case GR_here:
	    if (SUBST || VOID) 
		iErr = NATIVE_TOOMANYMARKS;
	    else if (pStats->bGetAnother)
		iErr = NATIVE_MODVOID;
	    break;
	    
	case GR_mark:
	    if (SUBST || VOID) 
		iErr = NATIVE_TOOMANYMARKS;
	    else if (pStats->bGetAnother)
		iErr = NATIVE_THISWHAT;
	    break;

	case GR_touch:
	    if (!MOD)
		iErr = NATIVE_NOTOUCH;
	    else if (pStats->bGetAnother)
		iErr = NATIVE_THISWHAT;
	    break;

	default:
	    break;
	    } /* switch */
	}
#endif    

    if (iErr == VEOS_SUCCESS) {

	/** mark the element for nancy matcher **/
	
	if (pStats->bMarkNextElt) {
	    SETFLAG(NANCY_EltMarkMask, pVElt->iFlags);
	    pStats->bMarkNextElt = FALSE;
	    pStats->bGetAnother = FALSE;
	    }

	if (pStats->bTouchNextElt) {
	    SETFLAG(NANCY_EltTouchMask, pVElt->iFlags);
	    pStats->bTouchNextElt = FALSE;
	    pStats->bGetAnother = FALSE;
	    }


	switch (pVElt->iType) {
	    
	case GR_these:
	    pStats->bExpOrder = TRUE;
	    break;
	    
	case GR_any:
	    pStats->bOrdered = FALSE;
	    pStats->bExpContent = TRUE;
	    pStats->bMustEnd = TRUE;
	    break;
	    
	case GR_theseall:
	    pStats->bExpOrder = TRUE;
	    pStats->bMustEnd = TRUE;
	    break;
	    
	case GR_here:
	    VOID = TRUE;
	    SETFLAG(NANCY_EltMarkMask, pVElt->iFlags);
	    pStats->bMarkedWithin = TRUE;
	    break;
	    
	case GR_mark:
	    SUBST = TRUE;
	    pStats->bMarkedWithin = TRUE;
	    pStats->bMarkNextElt = TRUE;		
	    pStats->bGetAnother = TRUE;		
	    break;
	    
	case GR_touch:
	    pStats->bTouchedWithin = TRUE;
	    pStats->bTouchNextElt = TRUE;		
	    pStats->bGetAnother = TRUE;		
	    break;
	    
	default:
	    break;
	    } /* switch */
	}

    return(iErr);

    } /* Native_PatVEltClerical */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_ConvertSymbol(pXElt, pVElt)
    LVAL	pXElt;
    TPElt	pVElt;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    char	*sSrc;
    boolean	bParsed = FALSE;


    sSrc = (char *) getstring(getpname(pXElt));

    switch(sSrc[0]) {

	
    case '^': 	/* '^' marks the void for insertion */
	if (sSrc[1] == '\0') {
	    pVElt->iType = GR_here;
	    bParsed = TRUE;
	    }
	break;

    case '>':   /* '>' is a mark for the next element */
	if (sSrc[1] == '\0') {
	    pVElt->iType = GR_mark;
	    bParsed = TRUE;
	    }
	break;

    case '~':   /* '~' touches the next element */
	if (sSrc[1] == '\0') {
	    pVElt->iType = GR_touch;
	    bParsed = TRUE;
	    }
	break;

    case '@': 	/* '@' is wildcard for ordered elements **/
    
	/** special form (@) means exactly one element **/
	if (sSrc[1] == '\0') {
	    pVElt->iType = GR_these;
	    pVElt->u.iVal = 1;
	    bParsed = TRUE;
	    }
	
	/** special form (@n) means exactly n elts **/
	else if (IsIntStr(&sSrc[1]) == VEOS_SUCCESS) {
	    if ((pVElt->u.iVal = atoi(&sSrc[1])) < 1)
		iErr = NATIVE_CRAZYWILD;
	    else
		pVElt->iType = GR_these;
	    bParsed = TRUE;
	    }
	
	/** special form (@@) means zero or more elts **/
	else if (sSrc[1] == '@' && sSrc[2] == '\0') {
	    pVElt->iType = GR_theseall;
	    bParsed = TRUE;
	    }
	break;


    case '*':   /* '*' is wildcard for unordered elements */
	
	/** special form (*) means exatly one element **/
	if (sSrc[1] == '\0') {
	    pVElt->iType = GR_some;
	    pVElt->u.iVal = 1;
	    bParsed = TRUE;
	    }
	
	/** special form (*n) means exactly n elts **/
	else if (IsIntStr(&sSrc[1]) == VEOS_SUCCESS) {
	    if ((pVElt->u.iVal = atoi(&sSrc[1])) < 1)
		iErr = NATIVE_CRAZYWILD;
	    else
		pVElt->iType = GR_some;
	    bParsed = TRUE;
	    }
	
	/** special form (**) means zero or more elts **/
	else if (sSrc[1] == '*' && sSrc[2] == '\0') {
	    pVElt->iType = GR_any;
	    bParsed = TRUE;
	    }
	break;

	} /* switch */


    /** save symbol's name as veos prim type **/
    
    if (!bParsed && iErr == VEOS_SUCCESS) {
	pVElt->iType = GR_prim;
	pVElt->u.pS = strdup(sSrc);
	}
    

    return(iErr);

    } /* Native_ConvertSymbol */
/****************************************************************************************/



/****************************************************************************************
			  Xlisp <--> Linearized Data Conversion
 ****************************************************************************************/


/****************************************************************************************/
TVeosErr Native_XEltToMsgRec(pXData, pMsgRec)
    LVAL 		pXData;
    TPMsgRec		pMsgRec;
{
    TVeosErr		iErr;

    pMsgRec->iLen = 0;
    pMsgRec->sMessage = TALK_BUFFER;


    /** perform data conversion to flat network-friendly form **/

    iErr = Native_XEltToMessage(pXData, pMsgRec->sMessage, &pMsgRec->iLen);

    if (iErr != VEOS_SUCCESS)
	Native_TrapErr(iErr, pXData);


    return(iErr);

    } /* Native_XEltToMsgRec */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_XEltToMessage(pXElt, pBuffer, pLen)
    LVAL 		pXElt;
    char		*pBuffer;
    int			*pLen;
{
    TVeosErr		iErr;
    int			iLen;
    TF2L		fTrans;

    iErr = VEOS_SUCCESS;

    /** message element is: element type, then data (except for NIL)
     ** assume pBuffer is aligned 
     **/

    if (null(pXElt)) {

	/** nil element is empty grouple **/
	*(int *) pBuffer = htonl(GR_grouple);	
	pBuffer += 4;

	/** empty grouple has zero elements **/
	*(int *) pBuffer = htonl(0);	

	iLen = 8;
	}
    else {

	switch (ntype(pXElt)) {
	    
	case CONS:
	    *(int *) pBuffer = htonl(GR_grouple);	
	    pBuffer += 4;
	    iLen = 4;
	    iErr = Native_ListToMessage(pXElt, pBuffer, &iLen);
	    break;

	case VECTOR:
	    *(int *) pBuffer = htonl(GR_vector);	
	    pBuffer += 4;
	    iLen = 4;
	    iErr = Native_VectToMessage(pXElt, pBuffer, &iLen);
	    break;

	case FIXNUM:
	    *(int *) pBuffer = htonl(GR_int);	
	    pBuffer += 4;
	    *(long *) pBuffer = htonl(getfixnum(pXElt));
	    iLen = 8;
	    break;
	    
	case FLONUM:
	    *(int *) pBuffer = htonl(GR_float);	
	    pBuffer += 4;
	    fTrans.u.f = getflonum(pXElt);
	    *(long *) pBuffer = htonl(fTrans.u.l);
	    iLen = 8;
	    break;
	    
	case STRING:
	    *(int *) pBuffer = htonl(GR_string);	
	    pBuffer += 4;
	    strcpy(pBuffer, getstring(pXElt));
	    iLen = 4 + MEMSIZE(strlen(getstring(pXElt)) + 1);
	    break;
	    
	case SYMBOL:
	    *(int *) pBuffer = htonl(GR_prim);	
	    pBuffer += 4;
	    strcpy(pBuffer, getstring(getpname(pXElt)));
	    iLen = 4 + MEMSIZE(strlen(getstring(getpname(pXElt))) + 1);
	    break;
	    
	default:
	    iErr = NATIVE_BADVTYPE;
	    iLen = 0;
	    break;

	    } /* switch */
	}

    *pLen += iLen;

    return(iErr);

    } /* Native_XEltToMessage */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_ListToMessage(pList, pBuffer, pLen)
    LVAL	pList;
    char	*pBuffer;
    int		*pLen;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    LVAL	pXFinger;
    int		iLen, iElts = 0;
    char	*pListHead;


    /** first code of protocol is number of elements, write later **/
    
    pListHead = pBuffer;
    pBuffer = pListHead + 4;
    *pLen += 4;

    
    /** convert each lisp sub-element **/

    pXFinger = pList;
    while (!null(pXFinger)) {

	/** invoke recursive translation **/

	iLen = 0;
	iErr = Native_XEltToMessage(car(pXFinger), pBuffer, &iLen);

	if (iErr != VEOS_SUCCESS) 
	    break;

	else {
	    iElts ++;

	    pBuffer += iLen;
	    *pLen += iLen;
	    }

	/** advance element ref **/
	
	pXFinger = cdr(pXFinger);
	
	} /* while */


    /** write number of elements **/

    *(int *) pListHead = htonl(iElts);

    return(iErr);

    } /* Native_ListToMessage */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Native_VectToMessage(pVect, pBuffer, pLen)
    LVAL	pVect;
    char	*pBuffer;
    int		*pLen;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    LVAL	pXFinger;
    int		iLen, iEltIndex, iElts;

    iElts = getsz(pVect);

    /** first code of protocol is number of elements **/
    *(int *) pBuffer = htonl(iElts);

    pBuffer += 4;
    *pLen += 4;

    
    /** convert each lisp sub-element **/

    iEltIndex = 0;
    while(iEltIndex < iElts) {


	/** invoke recursive translation **/

	iLen = 0;
	iErr = Native_XEltToMessage(getelement(pVect, iEltIndex), pBuffer, &iLen);

	if (iErr != VEOS_SUCCESS) 
	    break;

	else {
	    pBuffer += iLen;
	    *pLen += iLen;
	    }


	/** advance element ref **/

	iEltIndex ++;

	} /* while */

	
    return(iErr);

    } /* Native_VectToMessage */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Native_MessageToXElt(pBuffer, hXElt, pLen)
    char		*pBuffer;
    LVAL 		*hXElt;
    int			*pLen;
{
    TVeosErr		iErr = VEOS_SUCCESS;
    int			iLen, iType;
    TF2L		fTrans;

    *hXElt = NIL;

    iType = ntohl(*(int *) pBuffer);	/** assume pBuffer is aligned **/

    pBuffer += 4;
    *pLen += 4;

    switch (iType) {
	    
    case GR_grouple:
	iLen = 0;
	iErr = Native_MessageToList(pBuffer, hXElt, &iLen);
	break;

    case GR_vector:
	iLen = 0;
	iErr = Native_MessageToVect(pBuffer, hXElt, &iLen);
	break;

    case GR_int:
	*hXElt = cvfixnum((int) ntohl(*(long *) pBuffer));
	iLen = 4;
	break;
			 
    case GR_float:
	fTrans.u.l = ntohl(*(long *) pBuffer);
	*hXElt = cvflonum(fTrans.u.f);
	iLen = 4;
	break;

    case GR_string:
	*hXElt = cvstring(pBuffer);
	iLen = MEMSIZE(strlen(pBuffer) + 1);  
	break;

    case GR_prim:
	*hXElt = xlenter(pBuffer);
	iLen = MEMSIZE(strlen(pBuffer) + 1);  
	break;

    case GR_unspecified:
    default:
	iLen = 0;
	break;
	    
	} /* switch */

    *pLen += iLen;
	
    return(iErr);

    } /* Native_MessageToXElt */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_MessageToList(pBuffer, hList, pLen)
    char	*pBuffer;
    LVAL	*hList;
    int		*pLen;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    LVAL	pXFinger;
    int		iLen, iElts, iEltIndex;
    char	*pListHead;
    LVAL	pList, pXElt;

    xlstkcheck(2);
    xlsave(pList);
    xlsave(pXElt);

    /** extract # of elements from first part of grouple data **/

    iElts = ntohl(*(int *) pBuffer);

    pBuffer += 4;
    *pLen += 4;


    /** convert each element one at a time, 'talk msg format' -> list' **/

    iEltIndex = 0;
    while (iEltIndex < iElts) {

	iLen = 0;

	/** extract elt data, allocate specific elt mem, stuff it with data. **/

	iErr = Native_MessageToXElt(pBuffer, &pXElt, &iLen);

	if (iErr != VEOS_SUCCESS)
	    break;

	else {
	    pBuffer += iLen;
	    *pLen += iLen;

	    pList = cons(pXElt, pList);
	    }

	iEltIndex ++;
	}

    if (iErr == VEOS_SUCCESS) {

	*hList = ReverseList(pList);
	}

    xlpopn(2);

    return(iErr);

    } /* Native_MessageToList */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Native_MessageToVect(pBuffer, hVect, pLen)
    char	*pBuffer;
    LVAL	*hVect;
    int		*pLen;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    int		iLen, iElts, iEltIndex;
    LVAL	pVect, pXElt;

    xlstkcheck(2);
    xlsave(pVect);
    xlsave(pXElt);

    /** extract # of elements from first part of grouple data **/

    iElts = ntohl(*(int *) pBuffer);

    pBuffer += 4;
    *pLen += 4;


    /** create new lisp vector as container **/

    pVect = newvector(iElts);


    /** convert each element one at a time **/

    iEltIndex = 0;
    while (iEltIndex < iElts) {

	iLen = 0;

	/** extract elt data, allocate specific elt mem, stuff it with data. **/

	iErr = Native_MessageToXElt(pBuffer, &pXElt, &iLen);
	if (iErr != VEOS_SUCCESS) 
	    break;

	else {
	    pBuffer += iLen;
	    *pLen += iLen;

	    setelement(pVect, iEltIndex, pXElt);
	    }

	iEltIndex ++;
	}


    if (iErr == VEOS_SUCCESS)
	*hVect = pVect;

    xlpopn(2);

    return(iErr);

    } /* Native_MessageToVect */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_TrapErr(iErr, pXElt)
    TVeosErr	iErr;
    LVAL	pXElt;
{
    str63	sErr;

    switch(iErr) {

    case NATIVE_BADTYPE:
	xlbadtype(pXElt);
	break;
    case NATIVE_NOKERNEL:
	xlfail("veos kernel not initialized, use (vinit <port-num>)");
	break;
    case NATIVE_BADFREQ:
	xlerror("'!' expected", pXElt);
	break;
    case NATIVE_2KERNELS:
	xlfail("veos kernel already initialized");
	break;
    case NATIVE_BADVTYPE:
	xlerror("veos does not support that data type", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_BADXTYPE:
	xlerror("xlisp does not support that data type from veos",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_EMPTYELT:
	xlerror("empty data element from veos, probably a memory error",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NODATA:
	xlerror("no veos data to match... only the void remains", s_unbound);
	break;
    case NATIVE_THISWHAT:
	xlerror("pattern element modifier ('>' or '~') must be followed by a matchable element", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_TOOMANYMARKS:
	xlerror("patterns must contain exactly one '>' or '^'",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_CANTMIX:
	xlerror("can't mix '@' and '*'", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NOTEND:
	xlerror("indefinite wildcards (eg '@@' or '**') can only appear at end of grouple in pattern",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NOREPLACEMARK:
	xlerror("pattern must contain '>' or '^'", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NOFETCHMARK:
	xlerror("pattern must contain '>'", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NOVOID:
	xlerror("cannot get or copy from the void ('^')",  
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_BADPATSYMBOL:
	xlerror("symbol not recognized", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_CRAZYWILD:
	xlerror("nonsensical number of wildcard elements",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_MATCHFAIL:
	xlerror("match and/or replace did not succeed",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NOSTARN:
	xlerror("the '*n' feature is not supported",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_BADVOID:
	xlerror("ambiguous void marker (can't use '^' in pattern grouple containing '*')",
		pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NOHOST:
	xlerror("host not recognized", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_NOTOUCH:
	xlerror("can't touch (eg. '~') elements during nondestructive grouplespace access", pXElt == nil ? s_unbound : pXElt);
	break;
    case NATIVE_MODVOID:
	xlerror("can't use element modifiers ('>' or '~') with the void ('^')", pXElt == nil ? s_unbound : pXElt);
	break;
    case VEOS_SUCCESS:
	break;
    default:
	sprintf(sErr, "unexpected error %d", iErr);
	xlerror(sErr, pXElt == nil ? s_unbound : pXElt);
	break;
	}

    return(VEOS_SUCCESS);

    } /* Native_TrapErr */
/****************************************************************************************/



/****************************************************************************************/
boolean	IsUidElt(pXElt)
    LVAL	pXElt;
{
    return(vectorp(pXElt) &&
	   getsz(pXElt) == 2 &&
	   stringp(getelement(pXElt, 0)) &&
	   fixp(getelement(pXElt, 1)));

    } /* IsUidElt */
/****************************************************************************************/


/****************************************************************************************/
TVeosErr XVect2Uid(pXElt, pUid)
    LVAL	pXElt;
    TPUid	pUid;
{
    TVeosErr	iErr;

    /** assume sanity is checked **/

    iErr = Sock_ResolveHost(getstring(getelement(pXElt, 0)), &pUid->lHost);
    if (iErr == VEOS_SUCCESS)
	pUid->iPort = getfixnum(getelement(pXElt, 1));
    else
	iErr = NATIVE_NOHOST;

    return(iErr);

    } /* XVect2Uid */
/****************************************************************************************/


/****************************************************************************************/
TVeosErr Uid2XVect(pUid, hXElt)
    TPUid	pUid;
    LVAL	*hXElt;
{
    str255	sTemp;

    /** assume sanity is checked **/

    if (Sock_IP2StrHost(pUid->lHost, sTemp) == VEOS_SUCCESS ||
	Sock_IP2StrAddr(pUid->lHost, sTemp) == VEOS_SUCCESS) {

	/** assume caller locked *hXElt **/

	*hXElt = newvector(2);
	setelement(*hXElt, 0, cvstring(sTemp));
	setelement(*hXElt, 1, cvfixnum(pUid->iPort));
	}

    return(VEOS_SUCCESS);

    } /* Uid2XVect */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_XVectsToUids(pList, hDests)
    LVAL	pList;
    THUidNode	hDests;
{
    TVeosErr	iErr = VEOS_SUCCESS;
    TPUidNode	pDests, pNode;
    LVAL	pXFinger;

    /** convert lisp 'uid' vectors to nancy uids **/

    pDests = nil;
    pXFinger = pList;
    while (!null(pXFinger)) {

#ifndef OPTIMAL	
	if (!IsUidElt(car(pXFinger))) {
	    iErr = NATIVE_BADTYPE;
	    break;
	    }
#endif
	iErr = Shell_NewBlock(sizeof(TUidNode), &pNode, "uid-node");

	if (iErr != VEOS_SUCCESS)
	    break;

	else{
	    /** add new node to list **/
	    
	    pNode->pNext = pDests;
	    pDests = pNode;
	    
	    
	    /** convert addr to internal format **/
	    
	    iErr = XVect2Uid(car(pXFinger), &pNode->addr);
	    }

	pXFinger = cdr(pXFinger);

	} /* while */

    if (iErr == VEOS_SUCCESS)
	*hDests = pDests;
    else
	Native_DisposeUids(pDests);

    return(iErr);

    } /* Native_XVectsToUids */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_DisposeUids(pDests)
    TPUidNode	pDests;
{
    TPUidNode	pSave;

    while (pDests) {

	pSave = pDests->pNext;
	Shell_ReturnBlock(pDests, sizeof(TUidNode), "uid-node");
	pDests = pSave;
	}

    return(VEOS_SUCCESS);

    } /* Native_DisposeUids */
/****************************************************************************************/


/****************************************************************************************/
TVeosErr IsIntStr(sSrc)
    char     	*sSrc;
{
    TVeosErr	iErr;

    iErr = VEOS_FAILURE;
    if (sSrc) {

	for (iErr = VEOS_SUCCESS;
	     sSrc[0] != '\0' && iErr == VEOS_SUCCESS;
	     sSrc ++)

	    if (!isdigit(sSrc[0]))
		iErr = VEOS_FAILURE;
	}

    return(iErr);

    } /* IsIntStr */
/****************************************************************************************/


