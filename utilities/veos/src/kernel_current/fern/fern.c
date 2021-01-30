/****************************************************************************************
 * file: fern.c										*
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
				 Preliminaries
 *--------------------------------------------------------------------------------*/


#include "xlisp.h"
#include "kernel.h"
#include "xv_native.h"
#include "fern.h"

#include <math.h>

/*--------------------------------------------------------------------------------*/

boolean		fbase_bInit = FALSE;
boolean		fbase_bGoing = FALSE;
LVAL		s_pPersistFunc, s_pPersistProcs;
TStampEntHash	fbase_pHashes[5];
int		fbase_iHashFree;
TXMandRRec	fbase_pbCopyIntSubs;
TXMandRRec	fbase_pbCopyBndryVrt;

/*--------------------------------------------------------------------------------*/

void Fbase_Frame();
TVeosErr Fbase_InitMatcherPBs();

/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*
			     Lisp Interface To Fern
 *--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
LVAL Fbase_Init()
{
    if (!fbase_bInit) {

	/** make permanent xlisp symbol to contain persist function call **/
	
	s_pPersistFunc = xlenter("FC-PRS-NTRY");
	setvalue(s_pPersistFunc, cons(xlenter("FCON-PERSIST"), NIL));

	s_pPersistProcs = xlenter("PERSIST-PROCS");

	fbase_iHashFree = 0;

	Fbase_InitMatcherPBs();
	}

    return(true);
    }
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
LVAL Fbase_fcon_time()
{
    xllastarg();

    Fbase_Frame();

    return(true);
    } 
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
LVAL Fbase_fcon_go()
{
    xllastarg();

    fbase_bGoing = TRUE;
    while (fbase_bGoing)
	Fbase_Frame();

    return(true);
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
LVAL Fbase_fcon_local_ungo()
{
    xllastarg();

    fbase_bGoing = FALSE;

    return(true);
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
/* returns: hash-table-index of new fern maintained hash table
 */
LVAL Fbase_Hash_NewTab()
{
    int		i, iHashTab;
    
    iHashTab = fbase_iHashFree++;
    for (i=0; i<12; i++)
	fbase_pHashes[iHashTab][i] = nil;

    return(cvfixnum(iHashTab));
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
/* args: hash-table-refnum, new-uid, initial-float-data 
 */
LVAL Fbase_Hash_AddUid()
{
    LVAL		pReturn = NIL, pUid;
    int			i, iHashTab, iHashIndex;
    float	    	fData;
    TPStampEntRec	pNode, pFinger;

    iHashTab = getfixnum(xlgafixnum());

    pUid = xlgavector();
#ifndef OPTIMAL
    if (!IsUidElt(pUid))
	xlbadtype(pUid);
#endif

    fData = getflonum(xlgaflonum());

    iHashIndex = FBASE_HASH_HOST(getstring(getelement(pUid, 0)));


    /** check for this uid already in table...
     ** if so, just update data
     **/
    for (pNode = fbase_pHashes[iHashTab][iHashIndex];
	 pNode;
	 pNode = pNode->pNext) {
	
	if (FBASE_HASH_HIT(pUid, pNode)) {
	    pNode->fData = fData;
	    pReturn = true;
	    break;
	    }
	}

    /** uid not found, add new hash entry.
     **/
    if (pReturn == NIL) {

	if (Shell_NewBlock(sizeof(TStampEntRec), 
			   &pNode, "fern-hash-node") == VEOS_SUCCESS) {
	    
	    strcpy(pNode->sHost, getstring(getelement(pUid, 0)));
	    pNode->iPort = getfixnum(getelement(pUid, 1));
	    pNode->fData = fData;
	    
	    pNode->pNext = fbase_pHashes[iHashTab][iHashIndex];
	    fbase_pHashes[iHashTab][iHashIndex] = pNode;
	    
	    pReturn = true;
	    }
	}

    return(pReturn);
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
/* args: hash-table-index, uid
 */
LVAL Fbase_Hash_RemoveUid()
{
    LVAL		pReturn = NIL, pUid;
    int			i, iHashTab, iHashIndex;
    THStampEntRec	hFinger;
    TPStampEntRec	pSave;

    iHashTab = getfixnum(xlgafixnum());

    pUid = xlgavector();
    if (!IsUidElt(pUid))
	xlbadtype(pUid);

    iHashIndex = FBASE_HASH_HOST(getstring(getelement(pUid, 0)));
    for (hFinger = &(fbase_pHashes[iHashTab][iHashIndex]);
	 *hFinger;
	 hFinger = &(*hFinger)->pNext) {

	if (FBASE_HASH_HIT(pUid, *hFinger)) {
	    pSave = *hFinger;
	    *hFinger = pSave->pNext;
	    Shell_ReturnBlock(pSave, sizeof(TStampEntRec), "fern-hash-node");
	    pReturn = true;
	    break;
	    }
	}

    return(pReturn);
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
/* args: hash-table-index, uid, float-to-place-data.
 * returns: true or NIL
 */
LVAL Fbase_Hash_HashUid()
{
    LVAL		pReturn = NIL, pUid, pData;
    int			i, iHashTab, iHashIndex;
    TPStampEntRec	pFinger;

    iHashTab = getfixnum(xlgafixnum());

    pUid = xlgavector();
    if (!IsUidElt(pUid))
	xlbadtype(pUid);

    pData = xlgaflonum();

    iHashIndex = FBASE_HASH_HOST(getstring(getelement(pUid, 0)));
    for (pFinger = fbase_pHashes[iHashTab][iHashIndex];
	 pFinger;
	 pFinger = pFinger->pNext) {
	
	if (FBASE_HASH_HIT(pUid, pFinger)) {
	    setflonum(pData, pFinger->fData);
	    pReturn = true;
	    break;
	    }
	}

    return(pReturn);
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
LVAL Fbase_Init_CopyIntSubs()
{
    TVeosErr		iErr;

    iErr = Native_GetPatternArg(&fbase_pbCopyIntSubs.pPatGr, NANCY_CopyMatch);

    return(iErr == VEOS_SUCCESS ? true : NIL);
    }
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
LVAL Fbase_CopyIntSubs()
{
    TVeosErr		iErr;
    LVAL		pReturn;
    TTimeStamp		tTest;


    /** look for optional time-stamp-test **/

    NATIVE_TIME_ARG(fbase_pbCopyIntSubs.pTestTime, tTest);


    /** dispatch the matcher **/

    xlsave1(fbase_pbCopyIntSubs.pXResult);
    
    Native_XMandR(&fbase_pbCopyIntSubs);

    xlpop();

    pReturn = consp(fbase_pbCopyIntSubs.pXResult) ?
	car(fbase_pbCopyIntSubs.pXResult) : fbase_pbCopyIntSubs.pXResult;

    return(pReturn);
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
LVAL Fbase_Init_CopyBndryVrt()
{
    TVeosErr		iErr;

    iErr = Native_GetPatternArg(&fbase_pbCopyBndryVrt.pPatGr, NANCY_CopyMatch);

    return(iErr == VEOS_SUCCESS ? true : NIL);
    }
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
LVAL Fbase_CopyBndryVrt()
{
    TVeosErr		iErr;
    LVAL		pReturn;
    TTimeStamp		tTest;


    /** look for optional time-stamp-test **/

    NATIVE_TIME_ARG(fbase_pbCopyBndryVrt.pTestTime, tTest);


    /** dispatch the matcher **/

    xlsave1(fbase_pbCopyBndryVrt.pXResult);
    
    Native_XMandR(&fbase_pbCopyBndryVrt);

    xlpop();

    pReturn = consp(fbase_pbCopyBndryVrt.pXResult) ?
	car(fbase_pbCopyBndryVrt.pXResult) : fbase_pbCopyBndryVrt.pXResult;

    return(pReturn);
    }
/*--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*
	       Beuratrcatic Linkage Between Fern Prims and XLISP
 *--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
TVeosErr Fern_LoadPrims()
{
#define FERN_LOAD
#include "fern_prims.h"
#define FERN_LOAD
    }
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*
			       Private Functions
 *--------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------*/
TVeosErr Fbase_()
{
    TVeosErr		iErr;

    return(iErr);
    }
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
void Fbase_Frame()
{
    LVAL		pMsg;


    /** pass time to veos kernel for accounting.
     **/
    Kernel_SystemTask();


    for (Native_NextMsg(&pMsg);
	 pMsg;
	 Native_NextMsg(&pMsg)) {

	/** invoke normal lisp evaluator on message. 
	 **/ 
	xlxeval(pMsg); 

	/** at top of loop, when msgVar is set to next msg, 
	 ** old contents of msgVar are detached from any protected xlisp ptr, 
	 ** thus it will be garbage collected. 
	 **/ 
	} 

    /** do the persist procs. 
     **/ 
    if (!null(getvalue(s_pPersistProcs)))
	xleval(getvalue(s_pPersistFunc));
    }
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
TVeosErr Fbase_InitMatcherPBs()
{
    /** copy-int-subs settings **/
    
    fbase_pbCopyIntSubs.pSrcGr = WORK_SPACE;
    fbase_pbCopyIntSubs.iDestroyFlag = NANCY_CopyMatch;
    fbase_pbCopyIntSubs.pXReplaceElt = nil;
    fbase_pbCopyIntSubs.pStampTime = nil;

    /** copy-bndry-vrt settings **/
    
    fbase_pbCopyBndryVrt.pSrcGr = WORK_SPACE;
    fbase_pbCopyBndryVrt.iDestroyFlag = NANCY_CopyMatch;
    fbase_pbCopyBndryVrt.pXReplaceElt = nil;
    fbase_pbCopyBndryVrt.pStampTime = nil;

    return(VEOS_SUCCESS);
    
    } /* Fbase_InitMatcherPBs */
/*--------------------------------------------------------------------------------*/



