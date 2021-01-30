/****************************************************************************************
 *											*
 * file: xv_native.c									*
 *											*
 * the xlisp wrappers for the VEOS native prims.					*
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



/****************************************************************************************
				      Preliminaries
 ****************************************************************************************/

#include <math.h>
#include "xlisp.h"

/* VEOS definitions: */
#include "kernel.h"

#define DEFINE_NATIVE_GLOBS
#include "xv_native.h"
#undef DEFINE_NATIVE_GLOBS

/****************************************************************************************/

TVeosErr Native_MessageToLSpace();
void Native_ShowMatchArgs();
void Native_ShowSite();
TVeosErr Native_XCopySiteMatches();
TVeosErr Native_XRemoveSiteMatches();
TVeosErr Native_XInsertEltAtSite();
void Native_NextMsg();
TVeosErr Native_DoThrow();

/****************************************************************************************/



/****************************************************************************************
				 Veos Primitive Wrappers
 ****************************************************************************************/


/****************************************************************************************/
LVAL Native_Init()
{	
    LVAL     	pXReturn;
    int		iPort;
    TVeosErr	iErr;
    
    xlsave1(pXReturn);

    if (!moreargs())
	iPort = TALK_BOGUS_FD;
    else
	iPort = getfixnum(xlgafixnum());

    xllastarg();


    /** invoke veos kernel inialization **/
    
    iErr = Kernel_Init(iPort, Native_MessageToLSpace);
    if (iErr == VEOS_SUCCESS) {


	/** create a lisp based inspace for messages **/

	s_InSpace = xlenter("VEOS_INSPACE");
	setvalue(s_InSpace, NIL);
	NATIVE_INSPACE = &getvalue(s_InSpace);


	/** create keyword symbols for nancy prims **/

	k_TestTime = xlenter(":TEST-TIME"); /* use with copy only */
	k_Freq = xlenter(":FREQ"); 	    /* use with copy, put or get */


	/** setup invariant matcher settings in global param blocks **/

	Native_InitMatcherPBs();


	/** make a uid return value to signify success **/


	Uid2XVect(&IDENT_ADDR, &pXReturn);
	}


    xlpop();


    return(pXReturn);

    }  /* Native_Init */
/****************************************************************************************/



/****************************************************************************************/
LVAL Native_Close()
{	
    if (!KERNEL_INIT)
	Native_TrapErr(NATIVE_NOKERNEL, nil);

    xllastarg();

    Kernel_Shutdown();

    return(true);

    } /* Native_Close */
/****************************************************************************************/



/****************************************************************************************/
LVAL Native_Task()
{	
#ifndef OPTIMAL
    if (!KERNEL_INIT)
	Native_TrapErr(NATIVE_NOKERNEL, nil);

    xllastarg();
#endif

    /** talk will call our message handler and stuff the inspace **/

    Kernel_SystemTask();


    return(true);

    } /* Native_Task */
/****************************************************************************************/




/****************************************************************************************/
LVAL Native_Put()
{
    TVeosErr	iErr;
    TTimeStamp	tNow;

#ifndef OPTIMAL
    if (!KERNEL_INIT)
	Native_TrapErr(NATIVE_NOKERNEL, nil);
#endif



    /** get mandatory data argument **/

    native_putPB.pXReplaceElt = xlgetarg();

    
    
    /** get pattern from xlisp args **/

    iErr = Native_GetPatternArg(&native_putPB.pPatGr, NANCY_ReplaceMatch);
    if (iErr != VEOS_SUCCESS)
	Native_TrapErr(iErr, nil);


    /** get optional frequency argument **/

    NATIVE_FREQ_ARG(native_putPB.iFreqFlag);


    /** set the data time-stamp **/

    GET_TIME(tNow);
    native_putPB.pStampTime = &tNow;


    /** dispatch the matcher **/

    xlsave1(native_putPB.pXResult);
    
    Native_XMandR(&native_putPB);

    xlpop();



    /** clean up **/

    Nancy_DisposeGrouple(native_putPB.pPatGr);



    return (native_putPB.pXResult);

    } /* Native_Put */
/****************************************************************************************/


/****************************************************************************************/
LVAL Native_Get()
{
    TVeosErr	iErr;

#ifndef OPTIMAL
    if (!KERNEL_INIT)
	Native_TrapErr(NATIVE_NOKERNEL, nil);
#endif

    /** get pattern from xlisp args **/

    iErr = Native_GetPatternArg(&native_getPB.pPatGr, NANCY_RemoveMatch);
    if (iErr != VEOS_SUCCESS)
	Native_TrapErr(iErr, nil);


    /** get optional frequency argument **/

    NATIVE_FREQ_ARG(native_getPB.iFreqFlag);


    /** dispatch the matcher **/
    
    xlsave1(native_getPB.pXResult);

    Native_XMandR(&native_getPB);

    xlpop();


    /** clean up **/

    Nancy_DisposeGrouple(native_getPB.pPatGr);



    return (native_getPB.pXResult);

    } /* Native_Get */
/****************************************************************************************/


/****************************************************************************************/
LVAL Native_Copy()
{
    TVeosErr	iErr;
    TTimeStamp	tTest;

#ifndef OPTIMAL
    if (!KERNEL_INIT)
	Native_TrapErr(NATIVE_NOKERNEL, nil);
#endif



    /** get pattern from xlisp args **/

    iErr = Native_GetPatternArg(&native_copyPB.pPatGr, NANCY_CopyMatch);
    if (iErr != VEOS_SUCCESS)
	Native_TrapErr(iErr, nil);


    /** look for optional time-stamp-test **/

    NATIVE_TIME_ARG(native_copyPB.pTestTime, tTest);


    /** get optional frequency argument **/

    NATIVE_FREQ_ARG(native_copyPB.iFreqFlag);


    /** dispatch the matcher **/

    xlsave1(native_copyPB.pXResult);
    
    Native_XMandR(&native_copyPB);

    xlpop();


    /** clean up **/

    Nancy_DisposeGrouple(native_copyPB.pPatGr);



    return (native_copyPB.pXResult);

    } /* Native_Copy */
/****************************************************************************************/


/****************************************************************************************/
LVAL Native_Throw()
{
    LVAL	pXData, pXDests;
    TVeosErr	iErr;

#ifndef OPTIMAL
    if (!KERNEL_INIT)
	Native_TrapErr(NATIVE_NOKERNEL, nil);
#endif

    /** get dests argument **/

    pXDests = xlgalist();


    /** get data argument **/

    pXData = xlgetarg();

#ifndef OPTIMAL
    xllastarg();
#endif

    iErr = Native_DoThrow(pXDests, pXData);

    return(iErr == VEOS_SUCCESS ? true : NIL);

    } /* Native_Throw */
/****************************************************************************************/



/****************************************************************************************/
LVAL Native_Catch()
{
    LVAL	pSave;
    TPElt	pElt;

#ifndef OPTIMAL
    if (!KERNEL_INIT)
	Native_TrapErr(NATIVE_NOKERNEL, nil);

    xllastarg();
#endif

    Native_NextMsg(&pSave);

    return (pSave);

    } /* Native_Catch */
/****************************************************************************************/



/****************************************************************************************/
LVAL Native_MinTime()
{	
    TF2L	fTrans;
    
    /* guaranteed to be earlier than any system time */

    fTrans.u.l = NANCY_MINTIME;

    return(cvflonum(fTrans.u.f));

    }  /* Native_MinTime */
/****************************************************************************************/



/****************************************************************************************/
LVAL Native_NoSignals()
{	
    SIG_ENABLE = FALSE;

    return(true);

    }  /* Native_NoSignals */
/****************************************************************************************/



/****************************************************************************************/
LVAL Native_Bugs()
{	
    LVAL	pXModule;
    char	*sName;

    pXModule = xlgastring();
    sName = (char *) getstring(pXModule);
    
    if (strcmp(sName, "talk") == 0)
	TALK_BUGS = TALK_BUGS ? FALSE : TRUE;

    else if (strcmp(sName, "nancy") == 0)
	NANCY_BUGS = NANCY_BUGS ? FALSE : TRUE;

    else if (strcmp(sName, "shell") == 0)
	SHELL_BUGS = SHELL_BUGS ? FALSE : TRUE;

    return(true);

    }  /* Native_Bugs */
/****************************************************************************************/

extern int iEvals;

/****************************************************************************************/
LVAL Native_Zoot()
{
    static int	iAlreadySeen = 0;
    int		iSinceLast;

    iSinceLast = iEvals - iAlreadySeen;
    iAlreadySeen = iEvals;

    return(cvfixnum(iSinceLast));

    } /* Native_Zoot */
/****************************************************************************************/




/****************************************************************************************
		     The Beuractratic Linkage Between Veos and XLISP
 ****************************************************************************************/


/****************************************************************************************/
TVeosErr Shell_LoadNativePrims()
{
#define VEOS_NATIVE_LOAD
#include "xv_native_prims.h"
#undef VEOS_NATIVE_LOAD

    return(VEOS_SUCCESS);
    }
/****************************************************************************************/


/****************************************************************************************/
TVeosErr Shell_BailOut(sErr)
    char		*sErr;
{

    xlfatal(sErr);

    /** not reached **/

    return(VEOS_SUCCESS);

    } /* Shell_BailOut */
/****************************************************************************************/



/****************************************************************************************
			 The Sticky Goo Just Beneath the Wrappers
 ****************************************************************************************/


/****************************************************************************************/
TVeosErr Native_InitMatcherPBs()
{
    /** vget settings **/
    
    native_getPB.pSrcGr = WORK_SPACE;
    native_getPB.iDestroyFlag = NANCY_RemoveMatch;
    native_getPB.pXReplaceElt = nil;
    native_getPB.pStampTime = nil;
    native_getPB.pTestTime = nil;
    
    /** vcopy settings **/
    
    native_copyPB.pSrcGr = WORK_SPACE;
    native_copyPB.iDestroyFlag = NANCY_CopyMatch;
    native_copyPB.pXReplaceElt = nil;
    native_copyPB.pStampTime = nil;

    /** vput settings **/
    
    native_putPB.pSrcGr = WORK_SPACE;
    native_putPB.iDestroyFlag = NANCY_ReplaceMatch;
    native_putPB.pTestTime = nil;


    return(VEOS_SUCCESS);
    
    } /* Native_InitMatcherPBs */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_DoThrow(pXDests, pXData)
    LVAL	pXData, pXDests;
{
    TPUidNode	pDests;
    TVeosErr	iErr;
    TMsgRec	msgOut;


    /** convert host/port vectors to talk uids **/

    iErr = Native_XVectsToUids(pXDests, &pDests);
    if (iErr != VEOS_SUCCESS) {
	Native_TrapErr(iErr, pXDests);
	}

    /** convert data element to flat network format **/

    iErr = Native_XEltToMsgRec(pXData, &msgOut);
    if (iErr != VEOS_SUCCESS) {
	Native_DisposeUids(pDests);
	Native_TrapErr(iErr, pXData);
	}

    /** pass the flat message to veos kernel **/

    iErr = Talk_SpeakToMany(pDests, &msgOut);


    Native_DisposeUids(pDests);

    return(iErr);

    } /* Native_DoThrow */
/****************************************************************************************/



/****************************************************************************************/
void Native_NextMsg(hMsg)
    LVAL      	*hMsg;
{
    *hMsg = NIL;

    if (!null(*NATIVE_INSPACE)) {
	
	/** get the oldest message **/
	
	*hMsg = car(*NATIVE_INSPACE);
	
	/** remove this msg from list immediately. 
	 ** first cons cell in this list will thus be garbage collected. 
	 ** pass back the new msg. 
	 **/ 
	
	*NATIVE_INSPACE = cdr(*NATIVE_INSPACE); 
	}
    }
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_XMandR(pMandRPB)
    TPXMandRRec		pMandRPB;
{
    TVeosErr		iErr;
    TMatchRec		matchSpec;
    TPReplaceRec	pSite, pSave;

    
    /** Initialize the match record.
     ** This record get passed through the entire match process.
     ** The matcher uses to record sites for removal and insertion.
     ** If the matcher returns success,
     ** we then perform any destructive operations on the gspace.
     **/

    matchSpec.pPatGr = pMandRPB->pPatGr;
    matchSpec.pSrcGr = pMandRPB->pSrcGr;
    matchSpec.iDestroyFlag = pMandRPB->iDestroyFlag;
    matchSpec.iFreqFlag = pMandRPB->iFreqFlag;
    matchSpec.pReplaceList = nil;
    matchSpec.pTouchList = nil;

#ifndef OPTIMAL
    if (NANCY_BUGS) 
	Native_ShowMatchArgs(pMandRPB);
#endif

    /************************************/

    iErr = Nancy_MatchGrouple(&matchSpec);

    /************************************/

#ifndef OPTIMAL
    if (NANCY_BUGS)
	fprintf(stderr, "nancy %s: match %s.\n", 
		WHOAMI, iErr == VEOS_SUCCESS ? "succeeded" : "failed");
#endif
		
    /** Perform any destructive operations on the gspace.
     ** These occur in on a per-site basis.
     ** A site is:
     **   an enclosing grouple,
     **   a set of element intervals,
     **   an element index at which to insert.
     ** Sites are generated by the matcher during matching.
     **/

    /** perform destructive element retrieval
     **/

    switch (pMandRPB->iDestroyFlag) {

    case NANCY_CopyMatch:
	for (pSite = matchSpec.pReplaceList;
	     pSite && iErr == VEOS_SUCCESS;
	     pSite = pSite->pNext) {
#ifndef OPTIMAL
	    if (NANCY_BUGS) 
		Native_ShowSite(pSite);
#endif
	    iErr = Native_XCopySiteMatches(pSite, pMandRPB->pTestTime,
					   &pMandRPB->pXResult);
	    }
	break;
	    
    case NANCY_RemoveMatch:
	for (pSite = matchSpec.pReplaceList;
	     pSite && iErr == VEOS_SUCCESS;
	     pSite = pSite->pNext) {

#ifndef OPTIMAL
	    if (NANCY_BUGS) 
		Native_ShowSite(pSite);
#endif
	    iErr = Native_XRemoveSiteMatches(pSite, pMandRPB->pTestTime,
					     &pMandRPB->pXResult);
	    }
	break;

    case NANCY_ReplaceMatch:
	for (pSite = matchSpec.pReplaceList;
	     pSite && iErr == VEOS_SUCCESS;
	     pSite = pSite->pNext) {

#ifndef OPTIMAL
	    if (NANCY_BUGS) 
		Native_ShowSite(pSite);
#endif
	    iErr = Native_XRemoveSiteMatches(pSite, pMandRPB->pTestTime,
					     &pMandRPB->pXResult);
	    if (iErr == VEOS_SUCCESS)
		iErr = Native_XInsertEltAtSite(pMandRPB->pXReplaceElt,
					       pMandRPB->pStampTime, pSite);
	    }
	break;
	    
    case NANCY_GimmeMatch:
	iErr = NANCY_NotSupported;
	break;
	    
	} /* switch */
		    

    /** perform destructive element time stamping
     **/

    if (pMandRPB->pStampTime) {

	for (pSite = matchSpec.pTouchList;
	     pSite && iErr == VEOS_SUCCESS;
	     pSite = pSite->pNext) {
#ifndef OPTIMAL	    
	    if (NANCY_BUGS) 
		Native_ShowSite(pSite);
#endif	    
	    Native_TouchSiteMatches(pSite, *pMandRPB->pStampTime);
	    
	    }
	}

    /** free all matcher memory (stays within veos kernel) **/

    pSite = matchSpec.pReplaceList;
    while (pSite) {
	pSave = pSite;
	pSite = pSite->pNext;
	Shell_ReturnBlock(pSave, sizeof(TReplaceRec), "replace-bp");
	}

    pSite = matchSpec.pTouchList;
    while (pSite) {
	pSave = pSite;
	pSite = pSite->pNext;
	Shell_ReturnBlock(pSave, sizeof(TReplaceRec), "replace-bp");
	}
    

    if (iErr == VEOS_SUCCESS) {

	/** check for successful insert (give caller appropriate feeback) **/

	if (pMandRPB->iDestroyFlag == NANCY_ReplaceMatch &&
	    pMandRPB->pXResult == NIL)

	    pMandRPB->pXResult = true;
	}

#ifndef OPTIMAL
    else {
	if (NANCY_BUGS)
	    Nancy_TrapErr(iErr);
	}
#endif

    return(iErr);

    } /* Native_MatchAndReplace */
/****************************************************************************************/

    

/****************************************************************************************/
TVeosErr Native_XCopySiteMatches(pSite, pTestTime, hXResult)
    TPReplaceRec	pSite;
    TPTimeStamp		pTestTime;
    LVAL		*hXResult;
{
    int			iZone, iToKill, iElt, iLeft, iRight;
    LVAL		pXElt;
    TPElt		pVElt;
    TVeosErr		iErr;

    xlsave1(pXElt);

    /** convert outgoing data into supplanted language format.
     ** lisp is the current control language
     **/
    
    if (pTestTime == nil) {
    
	for (iZone = pSite->iZones - 1; iZone >= 0; iZone --) {
	    iLeft = pSite->pWipeList[iZone].iLeft;
	    iRight = pSite->pWipeList[iZone].iRight;
	    iToKill = iRight - iLeft + 1;

#ifndef OPTIMAL	    
	    if (NANCY_BUGS) {
		fprintf(stderr, "nancy %s: left: %d right: %d\n",
			WHOAMI, iLeft, iRight);
		}
#endif	    
	    for (iElt = iRight, pVElt = &pSite->pEnviron->pEltList[iRight];
		 iElt >= iLeft;
		 iElt--, pVElt --) {

		if (Native_VEltToXElt(pVElt, &pXElt) == VEOS_SUCCESS)
		    
		    /** assume caller protected *hXResult **/
		    *hXResult = cons(pXElt, *hXResult);
		}
	    }
	}
    else {

	for (iZone = pSite->iZones - 1; iZone >= 0; iZone --) {
	    iLeft = pSite->pWipeList[iZone].iLeft;
	    iRight = pSite->pWipeList[iZone].iRight;
	    iToKill = iRight - iLeft + 1;

#ifndef OPTIMAL
	    if (NANCY_BUGS) {
		fprintf(stderr, "nancy %s: left: %d right: %d\n",
			WHOAMI, iLeft, iRight);
		}
#endif	

	    for (iElt = iRight, pVElt = &pSite->pEnviron->pEltList[iRight];
		 iElt >= iLeft;
		 iElt--, pVElt--) {
		
		iErr = Native_NewVEltToXElt(pVElt, &pXElt, *pTestTime);
		if (iErr == VEOS_SUCCESS) {

		    /** assume caller protected *hXResult **/
		    *hXResult = cons(pXElt, *hXResult);
		    }
		/*
		else if (iErr == NATIVE_STALE)
		    iErr = VEOS_SUCCESS;
		    */
		}
	    }
	}

    xlpop();

    return(VEOS_SUCCESS);

    } /* Native_XCopySiteMatches */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_XRemoveSiteMatches(pSite, pTestTime, hXResult)
    TPReplaceRec	pSite;
    TPTimeStamp		pTestTime;
    LVAL		*hXResult;
{
    int			iZone, iToKill, iElt, iLeft, iRight;
    LVAL		pXElt;
    TPElt		pVElt;

    xlsave1(pXElt);

    for (iZone = pSite->iZones - 1; iZone >= 0; iZone --) {
	iLeft = pSite->pWipeList[iZone].iLeft;
	iRight = pSite->pWipeList[iZone].iRight;
	iToKill = iRight - iLeft + 1;
	
	/** convert outgoing data into supplanted language format.
	 ** that format is xlisp, and in reverse order
	 **/
	
	for (iElt = iRight, pVElt = &pSite->pEnviron->pEltList[iRight];
	     iElt >= iLeft;
	     iElt--, pVElt--) {

	    if (Native_VEltToXElt(pVElt, &pXElt) == VEOS_SUCCESS)

		/** assume caller has protected *hXResult **/

		*hXResult = cons(pXElt, *hXResult);
	    }
	
	Nancy_DeleteElementsInGrouple(pSite->pEnviron,
				      iLeft,
				      iToKill);
	}

    xlpop();

    return(VEOS_SUCCESS);

    } /* Native_XRemoveSiteMatches */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_XInsertEltAtSite(pXReplaceElt, pStampTime, pSite)
    LVAL		pXReplaceElt;
    TPTimeStamp		pStampTime;
    TPReplaceRec	pSite;
{
    TElt		localElt;
    TVeosErr		iErr = VEOS_SUCCESS;

    if (pSite->iInsertElt >= 0) {
	
	localElt = NIL_ELT;

	if (pStampTime)
	    iErr = Native_XEltToNewVElt(pXReplaceElt, &localElt, *pStampTime);
	else
	    iErr = Native_XEltToVElt(pXReplaceElt, &localElt);

	if (iErr == VEOS_SUCCESS) {

	    Nancy_NewElementsInGrouple(pSite->pEnviron, pSite->iInsertElt, 1,
				       GR_unspecified, 0);
	    pSite->pEnviron->pEltList[pSite->iInsertElt] = localElt;
	    }
	}

    return(iErr);
    
    } /* Native_XInsertEltAtSite */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_TouchSiteMatches(pSite, time)
    TPReplaceRec	pSite;
    TTimeStamp		time;
{
    int			iZone, iElt, iLeft, iRight;
    TPElt		pVElt;

    for (iZone = pSite->iZones - 1; iZone >= 0; iZone --) {

	iLeft = pSite->pWipeList[iZone].iLeft;
	iRight = pSite->pWipeList[iZone].iRight;
	
	/** simply update time stamp of given elements **/
	
	for (iElt = iRight, pVElt = &pSite->pEnviron->pEltList[iRight];
	     iElt >= iLeft; 
	     iElt--, pVElt--)

	    pVElt->tLastMod = time;
	}

    return(VEOS_SUCCESS);

    } /* Native_TouchSiteMatches */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Native_MessageToLSpace(pMsgRec)
    TPMsgRec		pMsgRec;
{
    TVeosErr 		iErr;
    LVAL		pXElt, *hFinger;
    int			iLen;
    char		*pBuf;


    xlsave1(pXElt);

    /** return data to grouple form **/		    
    
    pBuf = pMsgRec->sMessage;
    iLen = 0;
    iErr = Native_MessageToXElt(pBuf, &pXElt, &iLen);
    
#ifndef OPTIMAL	
    if (TALK_BUGS) {
	fprintf(stderr, "listen %s: results of message conversion, native: %d\n",
		WHOAMI, iErr);
	}	
#endif
    
    if (iErr == VEOS_SUCCESS) {

#ifndef OPTIMAL	
	if (TALK_BUGS) {
	    fprintf(stderr, "listen %s: element in message:\n", WHOAMI);

	    errprint(pXElt);
	    }
#endif

	/** append message to native inspace list **/
	
	hFinger = NATIVE_INSPACE;
	while (!null(*hFinger))
	    hFinger = &cdr(*hFinger);

	*hFinger = cons(pXElt, NIL);
	}

    xlpop();

    return(iErr);

    } /* Native_MessageToLSpace */
/****************************************************************************************/



/****************************************************************************************/
void Native_ShowMatchArgs(pMandRPB)
    TPXMandRRec		pMandRPB;
{
    fprintf(stderr, "nancy %s: MandR arguments.\n", WHOAMI);

    fprintf(stderr, "nancy %s: source:\n", WHOAMI);
    Nancy_GroupleToStream(pMandRPB->pSrcGr, stderr);

    fprintf(stderr, "nancy %s: pattern:\n", WHOAMI);
    Nancy_GroupleToStream(pMandRPB->pPatGr, stderr);
    
    fprintf(stderr, "nancy %s: destroyFlag: %s\n", WHOAMI,
	    pMandRPB->iDestroyFlag == NANCY_RemoveMatch ? "remove" :
	    pMandRPB->iDestroyFlag == NANCY_CopyMatch ? "copy" :
	    pMandRPB->iDestroyFlag == NANCY_ReplaceMatch ? "replace" : "unknown");
    
    fprintf(stderr, "nancy %s: freqFlag: %s\n", WHOAMI,
	    pMandRPB->iFreqFlag == NANCY_MatchOne ? "one" : "all");
    
    fprintf(stderr, "nancy %s: replace elt:\n", WHOAMI);
    errprint(pMandRPB->pXReplaceElt);
    
    fprintf(stderr, "nancy %s: stamp-time: ", WHOAMI);
    if (pMandRPB->pStampTime)
	PRINT_TIME(*pMandRPB->pStampTime, stderr);
    else
	fprintf(stderr, "nil");
    fprintf(stderr, "\n");

    fprintf(stderr, "nancy %s: test-time: ", WHOAMI);
    if (pMandRPB->pTestTime)
	PRINT_TIME(*pMandRPB->pTestTime, stderr);
    else
	fprintf(stderr, "nil");
    fprintf(stderr, "\n");

    } 
/****************************************************************************************/



/****************************************************************************************/
void Native_ShowSite(pSite)
    TPReplaceRec       	pSite;
{
    fprintf(stderr, "nancy %s: site grouple:\n", WHOAMI);
    Nancy_GroupleToStream(pSite->pEnviron, stderr);
    fprintf(stderr, "nancy %s: site zones: %d\n", WHOAMI, pSite->iZones);
    fprintf(stderr, "nancy %s: site insert elt: %d\n", WHOAMI, pSite->iInsertElt);
    }
/****************************************************************************************/



