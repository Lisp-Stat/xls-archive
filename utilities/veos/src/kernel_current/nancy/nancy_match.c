/****************************************************************************************
 *											*
 * file: nancy_match.c									*
 *											*
 * February 15, 1992:  Matching semantics for grouples.				       	*
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
#include <malloc.h>
#include <varargs.h>

/****************************************************************************************/



/****************************************************************************************
 * Nancy_MatchGrouple									*/

TVeosErr Nancy_MatchGrouple(pMatchSpec)
    TPMatchRec		pMatchSpec;
{
    TVeosErr 		iErr;

    if (TESTFLAG(NANCY_ContentMask, pMatchSpec->pPatGr->iFlags))
	iErr = Nancy_MatchContentGrouple(pMatchSpec);
    else
	iErr = Nancy_MatchPositionGrouple(pMatchSpec);

    return(iErr);

    } /* Nancy_MatchGrouple */
/****************************************************************************************/



/****************************************************************************************
 * 				      private routines					*
 ****************************************************************************************/


/****************************************************************************************
 * Nancy_MatchPositionGrouple								*/

TVeosErr Nancy_MatchPositionGrouple(pMatchSpec)
    TPMatchRec		pMatchSpec;
{
    int			iPatElts, iSrcElts;
    int			iMoreSrcElts;
    TPElt		pPatFinger, pSrcFinger;
    TPGrouple		pPatGr, pSrcGr;
    int			iPatIndex, iSrcIndex;

    boolean		bMarked, bTouched;
    boolean		bMarkWithin, bTouchWithin;
    TPReplaceRec	pMarkPB = nil, pTouchPB = nil;
    TVeosErr		iErr = VEOS_SUCCESS;

    /** setup cached locals **/

    pPatGr = pMatchSpec->pPatGr;
    pSrcGr = pMatchSpec->pSrcGr;
    iSrcElts = pSrcGr->iElts;
    iPatElts = pPatGr->iElts;

    bMarkWithin = TESTFLAG(NANCY_MarkWithinMask, pPatGr->iFlags);
    bTouchWithin = TESTFLAG(NANCY_TouchWithinMask, pPatGr->iFlags);


    /** setup replace and touch descriptors to pass back.
     **/

    if (bMarkWithin) {
	Nancy_NewReplaceNode(&pMarkPB);
	pMarkPB->pEnviron = pSrcGr;
	}
    if (bTouchWithin) {
	Nancy_NewReplaceNode(&pTouchPB);
	pTouchPB->pEnviron = pSrcGr;
	}


    /** pattern controls the flow 
     ** loop through each pattern element until...
     ** - an element match fails, or
     ** - we run out of src elements (pattern too big)
     ** - we run out of pattern elements (pattern not sufficient)
     **/

    iSrcIndex = 0;
    iPatIndex = 0;

    while (iErr == VEOS_SUCCESS) {	

	/*******************************************************
	 ** first, pass the gauntlet of tests for continuance **
	 *******************************************************/

	/** check for end of pattern **/

	if (iPatIndex >= iPatElts) {
	    if (iSrcIndex != iSrcElts)
		iErr = NANCY_PatTooShort;
	    break;
	    }


	/** setup local info of current pattern element **/

	pPatFinger = &pPatGr->pEltList[iPatIndex];
	pSrcFinger = &pSrcGr->pEltList[iSrcIndex];

	bMarked = TESTFLAG(NANCY_EltMarkMask, pPatFinger->iFlags);
	bTouched = TESTFLAG(NANCY_EltTouchMask, pPatFinger->iFlags);


	/** check for end of source,
	 ** and not about to insert,
	 ** and matching zero or more.
	 **/

	if (iSrcIndex >= iSrcElts &&
	    pPatFinger->iType != GR_here &&
	    pPatFinger->iType != GR_theseall) {

	    /** must be more pattern elts, or would not have got this far **/

	    iErr = NANCY_SrcTooShort;
	    break;
	    }
	

	/**********************************************
	 ** second, perform the element match itself **
	 **********************************************/

	switch (pPatFinger->iType) {
	    
	case GR_theseall:
	    if (iSrcIndex < iSrcElts) {
		if (bMarked) {
		    pMarkPB->pWipeList[pMarkPB->iZones].iLeft = iSrcIndex;
		    pMarkPB->pWipeList[pMarkPB->iZones].iRight = iSrcElts - 1;
		    pMarkPB->iZones ++;
		    if (pMatchSpec->iDestroyFlag == NANCY_ReplaceMatch)
			pMarkPB->iInsertElt = iSrcIndex;
		    }
		else if (bTouched) {
		    pTouchPB->pWipeList[pTouchPB->iZones].iLeft = iSrcIndex;
		    pTouchPB->pWipeList[pTouchPB->iZones].iRight = iSrcElts - 1;
		    pTouchPB->iZones ++;
		    }
		iSrcIndex = iSrcElts;
		}
	    iSrcIndex = iSrcIndex - 1;
	    break;

	case GR_here:
	    pMarkPB->iInsertElt = iSrcIndex;
	    iSrcIndex = iSrcIndex - 1;
	    break;
	
	case GR_these:
	    iMoreSrcElts = pPatFinger->u.iVal - 1;

	    if (iSrcIndex + iMoreSrcElts >= iSrcElts)
		iErr = NANCY_SrcTooShort;

	    else {
		if (bMarked) {
		    pMarkPB->pWipeList[pMarkPB->iZones].iLeft = iSrcIndex;
		    pMarkPB->pWipeList[pMarkPB->iZones].iRight = iSrcIndex + iMoreSrcElts;
		    pMarkPB->iZones ++;
		    if (pMatchSpec->iDestroyFlag == NANCY_ReplaceMatch)
			pMarkPB->iInsertElt = iSrcIndex;
		    }
		else if (bTouched) {
		    pTouchPB->pWipeList[pTouchPB->iZones].iLeft = iSrcIndex;
		    pTouchPB->pWipeList[pTouchPB->iZones].iRight =
			iSrcIndex + iMoreSrcElts;
		    pTouchPB->iZones ++;
		    }
		iSrcIndex += iMoreSrcElts;
		}
	    break;

	case GR_grouple:
	case GR_vector:
	    if (pPatFinger->iType != pSrcFinger->iType)
		iErr = NANCY_NoMatch;

	    else {
		pMatchSpec->pSrcGr = pSrcFinger->u.pGr;
		pMatchSpec->pPatGr = pPatFinger->u.pGr;
		
		iErr = Nancy_MatchGrouple(pMatchSpec);
		
		pMatchSpec->pSrcGr = pSrcGr;
		pMatchSpec->pPatGr = pPatGr;
		
		if (iErr == VEOS_SUCCESS) {
		    if (bMarked) {
			pMarkPB->pWipeList[pMarkPB->iZones].iLeft = iSrcIndex;
			pMarkPB->pWipeList[pMarkPB->iZones].iRight = iSrcIndex;
			pMarkPB->iZones ++;
			if (pMatchSpec->iDestroyFlag == NANCY_ReplaceMatch)
			    pMarkPB->iInsertElt = iSrcIndex;
			}
		    else if (bTouched) {
			pTouchPB->pWipeList[pTouchPB->iZones].iLeft = iSrcIndex;
			pTouchPB->pWipeList[pTouchPB->iZones].iRight = iSrcIndex;
			pTouchPB->iZones ++;
			}
		    }
		}
	    break;

	default:
	    iErr = Nancy_EltIdentical(pPatFinger, pSrcFinger);
	    if (iErr == VEOS_SUCCESS) {
		if (bMarked) {
		    pMarkPB->pWipeList[pMarkPB->iZones].iLeft = iSrcIndex;
		    pMarkPB->pWipeList[pMarkPB->iZones].iRight = iSrcIndex;
		    pMarkPB->iZones ++;
		    if (pMatchSpec->iDestroyFlag == NANCY_ReplaceMatch)
			pMarkPB->iInsertElt = iSrcIndex;
		    }
		else if (bTouched) {
		    pTouchPB->pWipeList[pTouchPB->iZones].iLeft = iSrcIndex;
		    pTouchPB->pWipeList[pTouchPB->iZones].iRight = iSrcIndex;
		    pTouchPB->iZones ++;
		    }
		}
	    break;

	    } /* switch */

	iPatIndex ++;
	iSrcIndex ++;
	}

    /********************
     ** third, cleanup **
     ********************/
    
    if (iErr != VEOS_SUCCESS) {

	if (bMarkWithin)
	    Nancy_DisposeReplaceNode(pMarkPB);
	if (bTouchWithin)
	    Nancy_DisposeReplaceNode(pTouchPB);
	}
    else {
	if (bMarkWithin) {
	    pMarkPB->pNext = pMatchSpec->pReplaceList;
	    pMatchSpec->pReplaceList = pMarkPB;
	    }
	if (bTouchWithin) {
	    pTouchPB->pNext = pMatchSpec->pTouchList;
	    pMatchSpec->pTouchList = pTouchPB;
	    }
	}
    
    return(iErr);

    } /* MatchPositionGrouple */
/****************************************************************************************/



/****************************************************************************************
 * Nancy_MatchContentGrouple								*/

TVeosErr Nancy_MatchContentGrouple(pMatchSpec)
    TPMatchRec		pMatchSpec;
{
    int			iPatElts;
    int			iPatIndex;
    TPElt		pWildElt, pPatElt;
    TPGrouple		pPatGr;

    boolean		bMarkWithin, bTouchWithin;
    TPReplaceRec	pMarkPB = nil, pTouchPB = nil;
    TVeosErr		iErr = VEOS_SUCCESS;


    pPatGr = pMatchSpec->pPatGr;
    iPatElts = pPatGr->iElts;


    /** content addressable grouples are specified
     ** very precisely.  ie, there must be at least
     ** one element, and the last element must be a * form.
     **
     ** thus, if a pattern came this far...
     ** it better have a * element in last location.
     ** so, the last elt type must be GR_any or GR_some.
     **/

    pWildElt = &pPatGr->pEltList[iPatElts - 1];


    /** don't match normally against * form **/
    iPatElts --;

    
    bMarkWithin = TESTFLAG(NANCY_MarkWithinMask, pPatGr->iFlags);
    bTouchWithin = TESTFLAG(NANCY_TouchWithinMask, pPatGr->iFlags);

    /** setup replace descriptor to pass back.
     ** note, many descriptors can be created with one match...
     ** for example, when the caller calls this function again
     ** during a 'MatchMany' type match.
     **/
    
    if (bMarkWithin) {
	Nancy_NewReplaceNode(&pMarkPB);
	pMarkPB->pEnviron = pMatchSpec->pSrcGr;
	}
    if (bTouchWithin) {
	Nancy_NewReplaceNode(&pTouchPB);
	pTouchPB->pEnviron = pMatchSpec->pSrcGr;
	}
    
    
    /** pattern controls the flow 
     ** loop through each pattern element until...
     ** - an element match fails, or
     ** - we run out of pattern elements (match successful)
     **/
    
    for (iPatIndex = 0, pPatElt = pMatchSpec->pPatGr->pEltList;
	 iErr == VEOS_SUCCESS;
	 iPatIndex ++, pPatElt ++) {
	
	
	/** check for end of pattern **/
	
	if (iPatIndex >= iPatElts)
	    break;
	
	/** void matches instantly, no match necessary **/

	if (pPatElt->iType == GR_here)
	    pMarkPB->iInsertElt = 0;

	/** match pattern element against each elt in src grouple **/

	else 
	    iErr = Nancy_MapMatch(pMatchSpec, iPatIndex, pMarkPB, pTouchPB);

	} /* pattern element loop */



    Nancy_MapRestore(pMatchSpec,
		     ((TESTFLAG(NANCY_EltMarkMask, pWildElt->iFlags) &&
		       iErr == VEOS_SUCCESS) ? TRUE : FALSE),
		     ((TESTFLAG(NANCY_EltTouchMask, pWildElt->iFlags) &&
		       iErr == VEOS_SUCCESS) ? TRUE : FALSE),
		     pMarkPB, pTouchPB);
    


    /** cleanup **/
    
    if (iErr != VEOS_SUCCESS) {

	if (bMarkWithin)
	    Nancy_DisposeReplaceNode(pMarkPB);
	if (bTouchWithin)
	    Nancy_DisposeReplaceNode(pTouchPB);
	}
    else {
	if (bMarkWithin) {
	    pMarkPB->pNext = pMatchSpec->pReplaceList;
	    pMatchSpec->pReplaceList = pMarkPB;
	    }
	if (bTouchWithin) {
	    pTouchPB->pNext = pMatchSpec->pTouchList;
	    pMatchSpec->pTouchList = pTouchPB;
	    }
	}

    return(iErr);

    } /* MatchContentGrouple */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Nancy_MapMatch(pMatchSpec, iPatIndex, pMarkPB, pTouchPB)
    TPMatchRec		pMatchSpec;
    int			iPatIndex;
    TPReplaceRec	pMarkPB, pTouchPB;
{
    int			iSrcElts, iSrcIndex;
    TPElt		pSrcFinger, pPatElt;
    TPGrouple		pSrcGr, pPatGr;
    int			iMatches;
    boolean		bPatMarked, bPatTouched;
    TVeosErr		iErr = VEOS_SUCCESS;
    
    
    pSrcGr = pMatchSpec->pSrcGr;
    iSrcElts = pSrcGr->iElts;
    
    pPatGr = pMatchSpec->pPatGr;
    pPatElt = &pPatGr->pEltList[iPatIndex];
    
    bPatMarked = TESTFLAG(NANCY_EltMarkMask, pPatElt->iFlags);
    bPatTouched = TESTFLAG(NANCY_EltTouchMask, pPatElt->iFlags);
    
#ifndef OPTIMAL
    if (NANCY_BUGS) {
	fprintf(stderr, "matching:  ");
	Nancy_ElementToStream(pPatElt, stderr);
	fprintf(stderr, "against:  ");
	Nancy_GroupleToStream(pSrcGr, stderr);
	}
#endif
    
    /** perform exhaustive search for pat
     ** element through the source grouple.
     **/
    iMatches = 0;
    
    switch (pPatElt->iType) {

    case GR_grouple:
    case GR_vector:
	for (iSrcIndex = 0, pSrcFinger = pSrcGr->pEltList;
	     iSrcIndex < iSrcElts;
	     iSrcIndex ++, pSrcFinger ++) {
	    
	    if (pPatElt->iType == pSrcFinger->iType) {
		
		pMatchSpec->pSrcGr = pSrcFinger->u.pGr;
		pMatchSpec->pPatGr = pPatElt->u.pGr;
		
		iErr = Nancy_MatchGrouple(pMatchSpec);
		
		pMatchSpec->pSrcGr = pSrcGr;
		pMatchSpec->pPatGr = pPatGr;
		
		if (iErr == VEOS_SUCCESS) {
		    
		    if (bPatMarked) {
			pMarkPB->pWipeList[pMarkPB->iZones].iLeft = iSrcIndex;
			pMarkPB->pWipeList[pMarkPB->iZones].iRight = iSrcIndex;
			pMarkPB->iZones ++;
			if (pMatchSpec->iDestroyFlag == NANCY_ReplaceMatch)
			    pMarkPB->iInsertElt = iSrcIndex;
			}
		    else if (bPatTouched) {
			pTouchPB->pWipeList[pTouchPB->iZones].iLeft = iSrcIndex;
			pTouchPB->pWipeList[pTouchPB->iZones].iRight = iSrcIndex;
			pTouchPB->iZones ++;
			}
		    
		    iMatches ++;

		    if (NANCY_BUGS) {
			fprintf(stderr, "matched on:    ");
			Nancy_ElementToStream(pSrcFinger, stderr);
			}		    

		    /** mark src element as having been matched **/
		    SETFLAG(NANCY_EltMatchMask, pSrcFinger->iFlags);
		    
		    if (pMatchSpec->iFreqFlag == NANCY_MatchOne)
			break;
		    
		    } /* matched */
		} /* same type */
	    } /* for */
	break;

    default:
	for (iSrcIndex = 0, pSrcFinger = pSrcGr->pEltList;
	     iSrcIndex < iSrcElts;
	     iSrcIndex ++, pSrcFinger ++) {
	    
	    if (pPatElt->iType == pSrcFinger->iType &&
		Nancy_EltIdentical(pPatElt, pSrcFinger) == VEOS_SUCCESS) {
		
		if (bPatMarked) {
		    pMarkPB->pWipeList[pMarkPB->iZones].iLeft = iSrcIndex;
		    pMarkPB->pWipeList[pMarkPB->iZones].iRight = iSrcIndex;
		    pMarkPB->iZones ++;
		    if (pMatchSpec->iDestroyFlag == NANCY_ReplaceMatch)
			pMarkPB->iInsertElt = iSrcIndex;
		    }
		else if (bPatTouched) {
		    pTouchPB->pWipeList[pTouchPB->iZones].iLeft = iSrcIndex;
		    pTouchPB->pWipeList[pTouchPB->iZones].iRight = iSrcIndex;
		    pTouchPB->iZones ++;
		    }
		
		iMatches ++;

		if (NANCY_BUGS) {
		    fprintf(stderr, "matched on:    ");
		    Nancy_ElementToStream(pSrcFinger, stderr);
		    }

		/** mark src element as having been matched **/
		SETFLAG(NANCY_EltMatchMask, pSrcFinger->iFlags);
		
		if (pMatchSpec->iFreqFlag == NANCY_MatchOne)
		    break;
		
		} /* matched */
	    } /* for */
	break;

	} /* switch */
    
    if (iMatches == 0)
	iErr = NANCY_NoMatch;
    else
	iErr = VEOS_SUCCESS;
    
    return(iErr);
    
    } /* Nancy_MapMatch */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Nancy_MapRestore(pMatchSpec, bGatherUnmatched, bTouchUnmatched, pMarkPB, pTouchPB)
    TPMatchRec		pMatchSpec;
    boolean		bGatherUnmatched, bTouchUnmatched;
    TPReplaceRec	pMarkPB, pTouchPB;
{
    int			iSrcIndex, iSrcElts;
    TPElt		pSrcFinger;
    TVeosErr		iErr = VEOS_SUCCESS;
    
    iSrcElts = pMatchSpec->pSrcGr->iElts;

    if (bGatherUnmatched) {

	for (iSrcIndex = 0, pSrcFinger = pMatchSpec->pSrcGr->pEltList;
	     iSrcIndex < iSrcElts; 
	     iSrcIndex ++, pSrcFinger ++) {

	    if (TESTFLAG(NANCY_EltMatchMask, pSrcFinger->iFlags)) {
		
		/** clear the source marks **/
		CLRFLAG(NANCY_EltMatchMask, pSrcFinger->iFlags);
		}
	    
	    else {
		/** gather unmatched elements into replace list **/
		
		if (pMarkPB->pWipeList[pMarkPB->iZones - 1].iRight == (iSrcIndex - 1))
		    pMarkPB->pWipeList[pMarkPB->iZones - 1].iRight = iSrcIndex;
		else {
		    pMarkPB->pWipeList[pMarkPB->iZones].iLeft = iSrcIndex;
		    pMarkPB->pWipeList[pMarkPB->iZones].iRight = iSrcIndex;
		    pMarkPB->iZones ++;
		    }
		}
	    } /* for */
	} /* gather marked */

    else if (bTouchUnmatched) {

	for (iSrcIndex = 0, pSrcFinger = pMatchSpec->pSrcGr->pEltList;
	     iSrcIndex < iSrcElts; 
	     iSrcIndex ++, pSrcFinger ++) {

	    if (TESTFLAG(NANCY_EltMatchMask, pSrcFinger->iFlags)) {
		
		/** clear the source marks **/
		CLRFLAG(NANCY_EltMatchMask, pSrcFinger->iFlags);
		}
	    
	    else {
		/** gather unmatched elements into touch list **/
		
		if (pTouchPB->pWipeList[pTouchPB->iZones - 1].iRight == (iSrcIndex - 1))
		    pTouchPB->pWipeList[pTouchPB->iZones - 1].iRight = iSrcIndex;
		else {
		    pTouchPB->pWipeList[pTouchPB->iZones].iLeft = iSrcIndex;
		    pTouchPB->pWipeList[pTouchPB->iZones].iRight = iSrcIndex;
		    pTouchPB->iZones ++;
		    }
		}
	    } /* for */
	} /* gather touch */

    return(iErr);

    } /* Nancy_MapRestore */
/****************************************************************************************



/****************************************************************************************
 * Nancy_NewReplaceNode									*/

TVeosErr Nancy_NewReplaceNode(hNode)
    THReplaceRec	hNode;
{
    TVeosErr 		iErr;
    TPReplaceRec	pNode;

    iErr = Shell_NewBlock(sizeof(TReplaceRec), &pNode, "replace-bp");
    if (iErr == VEOS_SUCCESS) {
	pNode->pEnviron = nil;
	pNode->iZones = 0;
	pNode->iInsertElt = -1;
	pNode->pNext = nil;
	}

    *hNode = pNode;

    return(iErr);

    } /* Nancy_NewReplaceNode */
/****************************************************************************************/



/****************************************************************************************
 * Nancy_DisposeReplaceNode								*/

TVeosErr Nancy_DisposeReplaceNode(pNode)
    TPReplaceRec	pNode;
{
    TVeosErr 		iErr;

    iErr = Shell_ReturnBlock(pNode, sizeof(TReplaceRec), "replace-bp");

    return(iErr);

    } /* Nancy_DisposeReplaceNode */
/****************************************************************************************/



/****************************************************************************************
 * Nancy_										*/

TVeosErr Nancy_()
{
    TVeosErr 		iErr;


    return(iErr);

    } /* Nancy_ */
/****************************************************************************************/

