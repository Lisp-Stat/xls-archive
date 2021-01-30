/****************************************************************************************
 *											*
 * file: nancy.c									*
 *											*
 * August 21, 1990: the world(s)' interface to grouples.			       	*
 *											*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/

/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/


/****************************************************************************************
 * 			         	includes galore					*/

#include "kernel.h"
#include <string.h>
#include <malloc.h>
#include <varargs.h>

/****************************************************************************************/


/****************************************************************************************
 * 			        forward function declarations				*/


/* nancy setup and preprocessing */

TVeosErr Nancy_Init();


/* fundamental grouple data structure utils */

TVeosErr Nancy_NewGrouple();						 
TVeosErr Nancy_DisposeGrouple();
TVeosErr Nancy_CopyGrouple();
TVeosErr Nancy_CreateElement();
TVeosErr Nancy_DisposeElement();
TVeosErr Nancy_CopyElement();
TVeosErr Nancy_NewElementsInGrouple();
TVeosErr Nancy_DeleteElementsInGrouple();


/* related public nancy utils */

TVeosErr Nancy_GroupleToStream();
TVeosErr Nancy_ElementToStream();
TVeosErr Nancy_GroupleToStreamWithLevel();
TVeosErr Nancy_ElementToStreamWithLevel();

TVeosErr Nancy_EmptyGrouple();
TVeosErr Nancy_InsertEltList();
TVeosErr Nancy_CopyEltList();
TVeosErr Nancy_ConcatGrouple();

TVeosErr Nancy_GetFileSize();
TVeosErr Nancy_FileToGrouple();
TVeosErr Nancy_TrapErr();


/* private nancy utils */

TVeosErr Nancy_ResizeEltList();
TVeosErr Nancy_SetupTypeSizes();

/****************************************************************************************/




/****************************************************************************************
 *				setup and preprocessing					*
 ****************************************************************************************/


/****************************************************************************************
 * Nancy_Init									*/

TVeosErr Nancy_Init()
{
    TVeosErr		iSuccess;

    iSuccess = VEOS_MEM_ERR;
    LINE_COUNT = 0;
    NANCY_MINTIME = 0;
    NANCY_TIME = 1;

    /** setup runtime hash table for element sizes **/

    iSuccess = Nancy_SetupFastMem();
    if (iSuccess == VEOS_SUCCESS) {
	
	/** StreamToElement assumes global buffer **/
	
	if (NEWPTR(NANCY_BUF, char *, VEOS_GROUPLE_BUF_SIZE)) {
	    
	    NIL_ELT.iType = GR_unspecified;
	    NIL_ELT.u.pU = nil;
	    NIL_ELT.tLastMod = 0x7FFFFFFF;
	    NIL_ELT.iFlags = 0;

	    iSuccess = Nancy_NewGrouple(&GR_INSPACE);
	    if (iSuccess == VEOS_SUCCESS) {

		iSuccess = Nancy_NewGrouple(&WORK_SPACE);
		}
	    }
	}
	
    return(iSuccess);

    } /* Nancy_Init */
/****************************************************************************************/



/****************************************************************************************
 *			 fundamental nancy data structure utils				*
 ****************************************************************************************/



/****************************************************************************************
 * Nancy_NewGrouple									*/

TVeosErr Nancy_NewGrouple(hDestGrouple)
    THGrouple		hDestGrouple;
{
    TVeosErr		iSuccess;
    TPGrouple		pNewGrouple;


    iSuccess = VEOS_FAILURE;			       	/* pessimism */


    if (hDestGrouple) {					/* sanity check */

	iSuccess = VEOS_MEM_ERR;			/* more pessimism */

	*hDestGrouple = (TPGrouple) nil; 



	/** allocate the grouple structure itself **/

	iSuccess = Shell_NewBlock(TYPE_SIZES[GR_grouple], &pNewGrouple,
				  "grouple");

	if (iSuccess == VEOS_SUCCESS) {
	    pNewGrouple->pEltList = nil;
	    pNewGrouple->iElts = 0;
	    pNewGrouple->iFlags = 0;

	    *hDestGrouple = pNewGrouple;
	    }
	}

    return(iSuccess);

    } /* Nancy_NewGrouple */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_DisposeGrouple									*/

TVeosErr Nancy_DisposeGrouple(pDeadGrouple)
    TPGrouple		pDeadGrouple;
{
    TVeosErr		iSuccess;
    int			iEltIndex;
    TPElt		pEltList;

    iSuccess = VEOS_SUCCESS;			/* what could go wrong? */

    if (pDeadGrouple) {				/* sanity check */


	/** clear all elements from grouple **/

	Nancy_DeleteElementsInGrouple(pDeadGrouple, 0, pDeadGrouple->iElts);
	    

	/** deallocate element list itself **/

	Nancy_ResizeEltList(pDeadGrouple, 0);


	/** deallocate the grouple structure itself **/

	Shell_ReturnBlock(pDeadGrouple, TYPE_SIZES[GR_grouple], "grouple");
	}


    return(iSuccess);

    } /* Nancy_DisposeGrouple */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_CopyGrouple									*/
 
TVeosErr Nancy_CopyGrouple(pSrcGrouple, pDestGrouple)
    TPGrouple		pSrcGrouple;
    TPGrouple		pDestGrouple;
{
    TVeosErr		iSuccess;

    iSuccess = VEOS_FAILURE;			      		/* pessimism */

    if (pSrcGrouple && pDestGrouple) {				/* sanity check */

	/** allocate element list enough for all copied elements **/

	iSuccess = Nancy_ResizeEltList(pDestGrouple, pSrcGrouple->iElts);
	if (iSuccess == VEOS_SUCCESS) {


	    iSuccess = Nancy_CopyEltList(pSrcGrouple->pEltList,
					 pDestGrouple->pEltList,
					 pSrcGrouple->iElts);
	    }
	}

    return(iSuccess);

    } /* Nancy_CopyGrouple */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_CreateElement									*/

TVeosErr Nancy_CreateElement(pDestElt, iType, iSize)
    TPElt		pDestElt;
    int			iType, iSize;
{
    TVeosErr		iSuccess;
    str15		sTypeName;

    iSuccess = VEOS_FAILURE;

    if (pDestElt) {		/* sane? */

	pDestElt->iType = iType;

	iSuccess = VEOS_MEM_ERR;

	switch (iType) {

	case GR_grouple:
	    iSuccess = Nancy_NewGrouple(&pDestElt->u.pGr);
	    break;

	case GR_vector:
	    iSuccess = Nancy_NewGrouple(&pDestElt->u.pGr);
	    pDestElt->iType = GR_vector;
	    break;

	case GR_string:
	case GR_prim:
	    if (iSize > 0) {
		if (NEWPTR(pDestElt->u.pS, char *, iSize))
		    iSuccess = VEOS_SUCCESS;
		}
	    else {
		pDestElt->u.pS = nil;
		iSuccess = VEOS_SUCCESS;
		}
	    break;

	case GR_float:	
	case GR_int:
	case GR_these:
	case GR_theseall:
	case GR_some:
	case GR_any:
	case GR_here:
	    /* nothing to allocate */
	    iSuccess = VEOS_SUCCESS;
	    break;

	case GR_unspecified:
	default:
	    pDestElt->u.pU = nil;
	    iSuccess = VEOS_SUCCESS;
	    break;

	    } /* switch */
	}

    return(iSuccess);

    } /* Nancy_CreateElement */
/****************************************************************************************/





/****************************************************************************************
 * Nancy_DisposeElement									*/

TVeosErr Nancy_DisposeElement(pDestElt)
    TPElt		pDestElt;
{
    TVeosErr		iSuccess;
    str15		sTypeName;

    iSuccess = VEOS_FAILURE;

    if (pDestElt) {
	
	/** recurs to sublist if necessary **/
	switch (pDestElt->iType) {
	    
	case GR_grouple:
	case GR_vector:
	    Nancy_DisposeGrouple(pDestElt->u.pGr);
	    break;
	    
	case GR_string:
	    DUMP(pDestElt->u.pS);
	    break;
	    
	case GR_float:	
	case GR_int:
	case GR_these:
	case GR_theseall:
	case GR_some:
	case GR_any:
	case GR_here:
	case GR_unspecified:
	default:
	    /* nothing allocated */
	    break;
	    
	    } /* switch */
	
	*pDestElt = NIL_ELT;
	
	iSuccess = VEOS_SUCCESS;
	}

    return(iSuccess);

    } /* Nancy_DisposeElement */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_CopyElement									*/

TVeosErr Nancy_CopyElement(pSrcElt, pDestElt)
    TPElt		pSrcElt, pDestElt;
{
    TVeosErr		iSuccess;

    iSuccess = VEOS_FAILURE;

    if (pSrcElt && pDestElt && pSrcElt->iType == pDestElt->iType) {	/* sane? */

	iSuccess = VEOS_SUCCESS;

	switch (pSrcElt->iType) {

	case GR_grouple:
	case GR_vector:
	    iSuccess = Nancy_CopyGrouple(pSrcElt->u.pGr,
					 pDestElt->u.pGr);
	    break;

	case GR_float:	
	case GR_int:
	case GR_these:
	case GR_some:
	    pDestElt->u.iVal = pSrcElt->u.iVal;
	    break;

	case GR_theseall:
	case GR_any:
	case GR_here:
	    /** no data to copy **/
	    break;

	case GR_string:
	case GR_prim:
	    if (pDestElt->u.pS)
		strcpy(pDestElt->u.pS, pSrcElt->u.pS);
	    else
		pDestElt->u.pS = strdup(pSrcElt->u.pS);
	    break;

	case GR_unspecified:
	    break;

	    } /* switch */

	pDestElt->tLastMod = pSrcElt->tLastMod;
	}

    return(iSuccess);

    } /* Nancy_CopyElement */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_NewElementsInGrouple								*/

TVeosErr Nancy_NewElementsInGrouple(pDestGrouple, iInsertElt, iElts, iType, iSize)
    TPGrouple		pDestGrouple;
    int			iInsertElt, iElts, iType, iSize;
{
    TVeosErr		iSuccess;
    TPElt		pEltList;
    int			iIndex, iOldElts, iLimit;

    iSuccess = VEOS_FAILURE;

    if (pDestGrouple) {

	iOldElts = pDestGrouple->iElts;		/* ResizeEltList() clobbers this field */

	iSuccess = Nancy_ResizeEltList(pDestGrouple,
				       iOldElts > iInsertElt ?
				       (iOldElts + iElts) : (iInsertElt + iElts));
	if (iSuccess == VEOS_SUCCESS) {



	    /** use stack var for speed **/

	    pEltList = pDestGrouple->pEltList;



	    /** all elements which occur after insertion point are shifted down **/

	    iIndex = iOldElts + iElts - 1;		    
	    iLimit = iInsertElt + iElts;

	    while (iIndex >= iLimit) {

		pEltList[iIndex] = pEltList[iIndex - iElts];

		iIndex --;
		}


	    /** initialize new elements that may have been created by list growth **/

	    iIndex = iOldElts;
	    iLimit = iInsertElt + iElts;

	    while (iIndex < iLimit) {

		pEltList[iIndex] = NIL_ELT;

		iIndex ++;
		}


	    /** attempt to create actual element data block, if requested **/

	    iIndex = iInsertElt;
	    iLimit = iInsertElt + iElts;
	    while (iIndex < iLimit && iSuccess == VEOS_SUCCESS) {

		iSuccess = Nancy_CreateElement(&pEltList[iIndex], iType, iSize);

		iIndex ++;
		}
	    }
	}

    return(iSuccess);

    } /* Nancy_NewElementsInGrouple */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_DeleteElementsInGrouple							*/

TVeosErr Nancy_DeleteElementsInGrouple(pGrouple, iStartElt, iElts)
    TPGrouple		pGrouple;
    int			iStartElt, iElts;
{
    TVeosErr		iSuccess;
    int			iIndex, iEndElt, iNewElts;
    TPElt		pEltList;

    iSuccess = VEOS_SUCCESS;
    iEndElt = iStartElt + iElts;

    if (pGrouple &&
	iElts > 0) {

	if (pGrouple->iElts >= iEndElt) {		/* sane? */
	    
	    
	    /** deallocate specific element data **/
	    
	    iIndex = iStartElt;
	    while (iIndex < iEndElt) {
		
		Nancy_DisposeElement(&pGrouple->pEltList[iIndex]);
		
		iIndex ++;
		}	
	    
	    
	    iSuccess = Nancy_DownShift(pGrouple, iStartElt, iElts);
	    }
	}

    return(iSuccess);

    } /* Nancy_DeleteElementsInGrouple */
/****************************************************************************************/


/****************************************************************************************
				     Data Conversion
 ****************************************************************************************/


/****************************************************************************************
 * Nancy_ElementToStream								*/

TVeosErr Nancy_ElementToStream(pElt, pStream)
    TPElt		pElt;
    FILE		*pStream;
{
    TVeosErr		iSuccess;
    FILE		*pSave;

    iSuccess = VEOS_FAILURE;

    if (pElt && pStream) {				/* sane? */

	pSave = GR_STREAM;
	GR_STREAM = pStream;

	iSuccess = Nancy_ElementToStreamAux(pElt, 0);

	GR_STREAM = pSave;
	}

    return(iSuccess);

    } /* Nancy_ElementToStream */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_GroupleToStream								*/

TVeosErr Nancy_GroupleToStream(pGrouple, pStream)
    TPGrouple		pGrouple;
    FILE		*pStream;
{
    TElt		elt;
    TVeosErr		iSuccess;

    iSuccess = VEOS_FAILURE;

    if (pGrouple && pStream) {				/* sane? */

	elt = NIL_ELT;
	elt.iType = GR_grouple;
	elt.u.pGr = pGrouple;
	
	iSuccess = Nancy_ElementToStream(&elt, pStream);
	}

    return(iSuccess);

    } /* Nancy_GroupleToStream */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_ElementToStreamWithLevel							*/

TVeosErr Nancy_ElementToStreamWithLevel(pElt, pStream, iLevel)
    TPElt		pElt;
    FILE		*pStream;
    int			iLevel;
{
    TVeosErr		iSuccess;
    FILE		*pSave;

    iSuccess = VEOS_FAILURE;

    if (pElt && pStream) {				/* sane? */

	pSave = GR_STREAM;
	GR_STREAM = pStream;

	iSuccess = Nancy_ElementToStreamAux(pElt, iLevel);

	GR_STREAM = pSave;
	}

    return(iSuccess);

    } /* Nancy_ElementToStreamWithLevel */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_GroupleToStreamWithLevel							*/

TVeosErr Nancy_GroupleToStreamWithLevel(pGrouple, pStream, iLevel)
    TPGrouple		pGrouple;
    FILE		*pStream;
    int			iLevel;
{
    TElt		elt;
    TVeosErr		iSuccess;

    iSuccess = VEOS_FAILURE;

    if (pGrouple && pStream) {				/* sane? */

	elt = NIL_ELT;
	elt.iType = GR_grouple;
	elt.u.pGr = pGrouple;

	iSuccess = Nancy_ElementToStreamWithLevel(&elt, pStream, iLevel);
	}

    return(iSuccess);

    } /* Nancy_GroupleToStreamWithLevel */
/****************************************************************************************/




/****************************************************************************************
			       Grouple -> Network Message
 ****************************************************************************************/


/****************************************************************************************
 * Nancy_EltToMessage									*/

TVeosErr Nancy_EltToMessage(pElt, pBuffer, pLen)
    TPElt		pElt;
    char		*pBuffer;
    int			*pLen;
{
    int			iLen, iType;

    if (pElt) {				/* sane? */

	iType = pElt->iType;

	/** first part of message element is element type **/
	/** assume pBuffer is aligned **/

	*(int *) pBuffer = htonl(iType);	

	pBuffer += 4;
	*pLen += 4;

	switch (iType) {

	case GR_grouple:
	case GR_vector:
	    iLen = 0;
	    Nancy_GroupleToMessage(pElt->u.pGr, pBuffer, &iLen);
	    break;

	case GR_int:
	case GR_float:
	    *(long *) pBuffer = htonl(pElt->u.iVal);
	    iLen = 4;
	    break;

	case GR_string:
	case GR_prim:
	    strcpy(pBuffer, pElt->u.pS);
	    iLen = MEMSIZE(strlen(pElt->u.pS) + 1);
	    break;
	    
	case GR_unspecified:
	default:
	    iLen = 0;
	    break;
	    
	    } /* switch */

	*pLen += iLen;
	}

    return(VEOS_SUCCESS);

    } /* Nancy_EltToMessage */
/****************************************************************************************/



/****************************************************************************************
 * Nancy_GroupleToMessage								*/

TVeosErr Nancy_GroupleToMessage(pGrouple, pBuffer, pLen)
    TPGrouple		pGrouple;
    char		*pBuffer;
    int			*pLen;
{
    int			iEltIndex, iElts, iLen;
    TPElt		pEltList;
    
    if (pGrouple) {				/* sane? */


	/** use stack vars for speed **/

	iElts = pGrouple->iElts;
	pEltList = pGrouple->pEltList;



	/** first code of protocol is number of elements **/

	*(int *) pBuffer = htonl(iElts);	/** assume pBuffer is aligned **/

	pBuffer += 4;
	*pLen += 4;


	for (iEltIndex = 0; iEltIndex < iElts; iEltIndex ++) {
	    
	    iLen = 0;

	    /** invoke recursive translation **/

	    Nancy_EltToMessage(&pEltList[iEltIndex], pBuffer, &iLen);

	    pBuffer += iLen;
	    *pLen += iLen;
	    }
	}

    return(VEOS_SUCCESS);

    } /* Nancy_GroupleToMessage */
/****************************************************************************************/




/****************************************************************************************
 *				related public utils					*
 ****************************************************************************************/


/****************************************************************************************
 * Nancy_EmptyGrouple									*/

TVeosErr Nancy_EmptyGrouple(pGrouple)
    TPGrouple		pGrouple;
{
    TVeosErr		iSuccess;

    iSuccess = VEOS_FAILURE;

    if (pGrouple && pGrouple->iElts > 0) {

	iSuccess = Nancy_DeleteElementsInGrouple(pGrouple, 0, pGrouple->iElts);
	}

    return(iSuccess);

    } /* Nancy_EmptyGrouple */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Nancy_InsertEltList(pSrcList, iSrcElts, pDestGrouple, iStartElt)
    TPElt		pSrcList;
    int			iSrcElts, iStartElt;
    TPGrouple		pDestGrouple;
{
    TVeosErr		iSuccess;
    int			iSrcIndex;
    TPElt		pDestList;


    iSuccess = VEOS_SUCCESS;

    if (pSrcList && pDestGrouple) {		/* sane? */
	
	iSuccess = Nancy_NewElementsInGrouple(pDestGrouple,
					      iStartElt,
					      iSrcElts,
					      GR_unspecified, 0);
	if (iSuccess == VEOS_SUCCESS) {


	    /** transfer each element from chosen starting locations **/
	    
	    pDestList = &pDestGrouple->pEltList[iStartElt];
	    iSrcIndex = 0;
	    while (iSrcIndex < iSrcElts) {
		
		pDestList[iSrcIndex] = pSrcList[iSrcIndex];
		

		/** set default vals for src elements **/
		/** in case the caller disposes the src elt list after the call **/
	    
		pSrcList[iSrcIndex++] = NIL_ELT;
		}
	    }
	}

    return(iSuccess);

    } /* Nancy_InsertEltList */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Nancy_CopyEltList(pSrcList, pDestList, iElts)
    TPElt		pSrcList, pDestList;
    int			iElts;
{
    int			iEltIndex;
    TVeosErr		iSuccess = VEOS_SUCCESS;


    if (pSrcList && pDestList) {		/* sane? */

	/** copy the grouple element list, one elt at a time **/
	
	iSuccess = VEOS_SUCCESS;
	iEltIndex = 0;
	while (iEltIndex < iElts && iSuccess == VEOS_SUCCESS) {

	    pDestList[iEltIndex] = pSrcList[iEltIndex];

	    if (pSrcList[iEltIndex].iType != GR_unspecified) {

		iSuccess = Nancy_CreateElement(&pDestList[iEltIndex],
					       pSrcList[iEltIndex].iType, 0);
		if (iSuccess == VEOS_SUCCESS)
		    
		    iSuccess = Nancy_CopyElement(&pSrcList[iEltIndex],
						 &pDestList[iEltIndex]);
		}
	    
	    iEltIndex ++;
	    }
	}

    return(iSuccess);
    
    } /* Nancy_CopyEltList */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_ConcatGrouple									*/
 
TVeosErr Nancy_ConcatGrouple(pSrcGrouple, pDestGrouple)
    TPGrouple		pSrcGrouple;
    TPGrouple		pDestGrouple;
{
    TVeosErr		iSuccess;
    int			iOldElts;

    iSuccess = VEOS_FAILURE;			      		/* pessimism */

    if (pSrcGrouple && pDestGrouple) {				/* sanity check */


	/** allocate element list enough for all copied elements **/

	iOldElts = pDestGrouple->iElts;
	iSuccess = Nancy_ResizeEltList(pDestGrouple,
				       iOldElts + pSrcGrouple->iElts);
	if (iSuccess == VEOS_SUCCESS) {


	    iSuccess = Nancy_CopyEltList(pSrcGrouple->pEltList,
					 &pDestGrouple->pEltList[iOldElts],
					 pSrcGrouple->iElts);
	    }
	}

    return(iSuccess);

    } /* Nancy_ConcatGrouple */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Nancy_EltIdentical(pLeftElt, pRightElt)
    TPElt		pRightElt, pLeftElt;
{
    TVeosErr		iSuccess;
    int			iType;
    boolean		bSame;
    char		*pGenericRight, *pGenericLeft, *pMax;
    

    iSuccess = VEOS_FAILURE;
    bSame = FALSE;

    if (pLeftElt == pRightElt)
	bSame = TRUE;

    else if (pLeftElt &&
	     pRightElt &&
	     pLeftElt->iType == pRightElt->iType) {

	iType = pLeftElt->iType;
	switch (iType) {
		
	case GR_float:
	    if (pLeftElt->u.fVal == pRightElt->u.fVal)
		bSame = TRUE;
	    break;
		
	case GR_int:
	    if (pLeftElt->u.iVal == pRightElt->u.iVal)
		bSame = TRUE;
	    break;
		
	case GR_string:
	case GR_prim:
	    if (strcmp(pLeftElt->u.pS, pRightElt->u.pS) == 0)
		bSame = TRUE;
	    break;
		
	case GR_unspecified:
	default:
	    bSame = TRUE;
	    break;
	    
	    } /* switch */
	}
	
    if (bSame)
	iSuccess = VEOS_SUCCESS;

    return(iSuccess);

    } /* Nancy_EltIdentical */
/****************************************************************************************/




/****************************************************************************************
 * Nancy_TrapErr									*/

TVeosErr Nancy_TrapErr(iErr)
    TVeosErr		iErr;
{
    switch(iErr) {
	
    case NANCY_EndOfGrouple:
	fprintf(stderr, "nancy %s: end of grouple reached\n", WHOAMI);
	break;	     
	
    case NANCY_MisplacedLeftBracket:
	fprintf(stderr, "nancy %s: misplaced '[', near line: %d\n", WHOAMI, LINE_COUNT);	
	break;
	
    case NANCY_MisplacedRightBracket:
	fprintf(stderr, "nancy %s: misplaced ']', near line: %d\n", WHOAMI, LINE_COUNT);	
	break;
	
    case NANCY_MissingRightBracket:
	fprintf(stderr, "nancy %s: missing ']', near line: %d\n", WHOAMI, LINE_COUNT);	
	break;
	
    case NANCY_BadType:
	fprintf(stderr, "nancy %s: bad element type, near line: %d\n", WHOAMI, LINE_COUNT);	
	break;
	
    case NANCY_NoTypeMatch:
	fprintf(stderr, "nancy %s: unknown data type, near line: %d\n", WHOAMI, LINE_COUNT);
	break;
	
    case VEOS_EOF:
	fprintf(stderr, "nancy %s: end of stream reached permaturely, near line: %d\n", WHOAMI, LINE_COUNT);
	break;
	
    case VEOS_MEM_ERR:
	fprintf(stderr, "nancy %s: memory error\n", WHOAMI);
	break;
	
    case VEOS_FAILURE:
	fprintf(stderr, "nancy %s: bad parameters\n", WHOAMI);
	break;
	
    case VEOS_SUCCESS:
	fprintf(stderr, "nancy %s: success\n", WHOAMI);
	break;
	
    case NANCY_NoMatch:
	fprintf(stderr, "nancy %s: no matches were found\n", WHOAMI);
	break;

    case NANCY_NotSupported:
	fprintf(stderr, "nancy %s: that operation not currently supported\n", WHOAMI);
	break;
	
    case NANCY_SrcTooShort:
	fprintf(stderr, "nancy %s: no match - source grouple shorter than pattern\n", WHOAMI);
	break;

    case NANCY_PatTooShort:
	fprintf(stderr, "nancy %s: no match - pattern shorter than source grouple\n", WHOAMI);
	break;

    default:
	fprintf(stderr, "nancy %s: unknown error: %d\n", WHOAMI, iErr);
	break;
	
	} /* switch */
    
    } /* Nancy_TrapErr */
/****************************************************************************************/



/****************************************************************************************
 * 				      private routines					*
 ****************************************************************************************/


/****************************************************************************************
 * Nancy_ResizeEltList									*/

TVeosErr Nancy_ResizeEltList(pDestGrouple, iNewElts)
    TPGrouple		pDestGrouple;
    int			iNewElts;
{		
    TVeosErr		iSuccess;
    TPElt		pEltList;
    int			iIsLen, iShouldLen;

    iSuccess = VEOS_SUCCESS;	

    if (pDestGrouple) {				/* sane? */


	/** if element ptr array is too long or too short, alter size **/

	iShouldLen = ELTS_ALLOCATED(iNewElts);
	iIsLen = ELTS_ALLOCATED(pDestGrouple->iElts);

	if (iShouldLen != iIsLen) {

	    iSuccess = VEOS_MEM_ERR;
	    pEltList = nil;


	    /**---------------------------------------------------**/
	    /** use fast in-house memory scheme for element lists **/
	    /**---------------------------------------------------**/
	    
	    if (iShouldLen <= 0) {
		
		/** want to dispose all elt list memory **/
		
		if (pDestGrouple->pEltList)
		    Shell_ReturnBlock(pDestGrouple->pEltList,
				      iIsLen * sizeof(TElt), "elt list");
		}
	    
	    else if (pDestGrouple->pEltList) {
		
		
		/** want to resize elt list array **/
		
		iSuccess = Shell_NewBlock(iShouldLen * sizeof(TElt),
					  &pEltList, "bigger elt list");
		if (iSuccess == VEOS_SUCCESS) {
		    
		    bcopy(pDestGrouple->pEltList,
			  pEltList,
			  (iIsLen < iShouldLen ? iIsLen : iShouldLen) * sizeof(TElt));
		    
		    Shell_ReturnBlock(pDestGrouple->pEltList,
				      iIsLen * sizeof(TElt), "smaller elt list");
		    }
		}
	    
	    
	    else {
		/** want to create elt list for first time **/
		
		iSuccess = Shell_NewBlock(iShouldLen * sizeof(TElt),
					  &pEltList, "elt list");
		}

	    /** attach new element array (contains old contents) **/

	    if (iSuccess = VEOS_SUCCESS)
		pDestGrouple->pEltList = pEltList;
	    }

	pDestGrouple->iElts = iNewElts;
	}

    return(iSuccess);

    } /* Nancy_ResizeEltList */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Nancy_DownShift(pGrouple, iStartElt, iElts)
    TPGrouple		pGrouple;
    int			iStartElt, iElts;
{
    TVeosErr		iSuccess;
    TPElt		pEltList;
    int			iNewElts, iIndex;

    
    /** use stack vars for speed **/
    
    pEltList = pGrouple->pEltList;
    iNewElts = pGrouple->iElts - iElts;
    
    
    
    iIndex = iStartElt;
    while (iIndex < iNewElts) {
	
	pEltList[iIndex] = pEltList[iIndex + iElts];
	
	iIndex ++;
	}
    
    iSuccess = Nancy_ResizeEltList(pGrouple, iNewElts);

    return(iSuccess);

    } /* Nancy_DownShift */
/****************************************************************************************/


/****************************************************************************************/
TVeosErr Nancy_ElementToStreamAux(pElt, iLevel)
    TPElt		pElt;
    int			iLevel;
{
    TPElt		pEltList;
    int			iElts, iEltIndex;
    str63		sHostName;

    if (pElt) {				/* sane? */

	Nancy_StreamTabs(iLevel, GR_STREAM);

	if (TESTFLAG(NANCY_EltMarkMask, pElt->iFlags))
	    fprintf(stderr, "> ");

	PRINT_TIME(pElt->tLastMod, stderr);

	
	switch (pElt->iType) {
	    
	case GR_vector:
	    fprintf(GR_STREAM, "#");
	    
	case GR_grouple:
	    fprintf(GR_STREAM, "[\n");
	    
	    pEltList = pElt->u.pGr->pEltList;
	    iElts = pElt->u.pGr->iElts;
	    
	    for (iEltIndex = 0; iEltIndex < iElts; iEltIndex ++) {
		
		/** recurs */
		Nancy_ElementToStreamAux(&pEltList[iEltIndex], iLevel + 1);
		}
	    
	    Nancy_StreamTabs(iLevel, GR_STREAM);
	    fprintf(GR_STREAM, "]\n");
	    break;
	    
	case GR_here:
	    fprintf(GR_STREAM, "^\n");
	    break;
	    
	case GR_some:
	    fprintf(GR_STREAM, "*%d\n", pElt->u.iVal);
	    break;
	    
	case GR_any:
	    fprintf(GR_STREAM, "**\n");
	    break;
	    
	case GR_these:
	    fprintf(GR_STREAM, "@%d\n", pElt->u.iVal);
	    break;
	    
	case GR_theseall:
	    fprintf(GR_STREAM, "@@\n");
	    break;
	    
	case GR_float:
	    fprintf(GR_STREAM, "%.2f\n", pElt->u.fVal);
	    break;
	    
	case GR_int:
	    fprintf(GR_STREAM, "%d\n", pElt->u.iVal);
	    break;
	    
	case GR_string:
	    fprintf(GR_STREAM, "\"%s\"\n", pElt->u.pS);
	    break;
	    
	case GR_prim:
	    fprintf(GR_STREAM, "'prim' %s\n", pElt->u.pS);
	    break;
	    
	case GR_unspecified:
	    fprintf(GR_STREAM, "()\n");
	    break;
	    
	default:
	    break;
	    
	    } /* switch */
	}

    return(VEOS_SUCCESS);

    } /* Nancy_ElementToStreamAux */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Nancy_TypeToString(iType, sName)
    int		iType;
    char	*sName;
{
    if (sName) {

	switch (iType) {
	    
	case GR_grouple:
	    strcpy(sName, "grouple");
	    break;
	case GR_vector:
	    strcpy(sName, "vector");
	    break;
	case GR_float:
	    strcpy(sName, "float");
	    break;
	case GR_int:
	    strcpy(sName, "int");
	    break;
	case GR_string:
	    strcpy(sName, "string");
	    break;
	case GR_prim:
	    strcpy(sName, "prim");
	    break;
	case GR_unspecified:
	    strcpy(sName, "unspecified");
	    break;
	case GR_these:
	    strcpy(sName, "these");
	    break;
	case GR_theseall:
	    strcpy(sName, "theseall");
	    break;
	case GR_some:
	    strcpy(sName, "some");
	    break;
	case GR_any:
	    strcpy(sName, "any");
	    break;
	case GR_here:
	    strcpy(sName, "here");
	    break;
	case GR_mark:
	    strcpy(sName, "mark");
	    break;
	case GR_touch:
	    strcpy(sName, "touch");
	    break;
	default:
	    break;
	    
	    } /* switch */
	}

    return(VEOS_SUCCESS);
    
    } /* Nancy_TypeToString */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Nancy_StreamTabs(iTabs, pStream)
    int		iTabs;
    FILE	*pStream;
{
    while (iTabs-- > 0)
	fprintf(pStream, "    ");

    return(VEOS_SUCCESS);

    } /* Nancy_StreamTabs */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Nancy_SetupFastMem()
{
    TVeosErr		iSuccess;
    int			i;

    iSuccess = VEOS_SUCCESS;

    TYPE_SIZES[GR_grouple] = TYPE_SIZES[GR_vector] = sizeof(TGrouple);

    TYPE_SIZES[GR_prim] = TYPE_SIZES[GR_string] = 0;

    TYPE_SIZES[GR_float] = 0;
    TYPE_SIZES[GR_int] = 0;
    TYPE_SIZES[GR_these] = 0;
    TYPE_SIZES[GR_theseall] = 0; 
    TYPE_SIZES[GR_some] = 0;
    TYPE_SIZES[GR_any] = 0;
    TYPE_SIZES[GR_here] = 0;


    /* the elt list for the empty grouple is nil */
    ALLOC_ELTS[0] = 0;

    /* optimize for pair-type grouples coming from lisp */
    ALLOC_ELTS[1] = 2;
    ALLOC_ELTS[2] = 2;

    for (i = 3; i < NANCY_AllocHashMax; i++)
	ALLOC_ELTS[i] = ELTS_TO_ALLOCATE(i);

    return(iSuccess);

    } /* Nancy_SetupFastMem */
/****************************************************************************************/


				 


