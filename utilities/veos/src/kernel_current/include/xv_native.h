/****************************************************************************************
 *											*
 * file: xv_native.h									*
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


#ifdef DEFINE_NATIVE_GLOBS
#define NEXTERN
#else
#define NEXTERN extern
#endif

/****************************************************************************************/


/****************************************************************************************/

typedef struct {

    TPGrouple		pSrcGr;		/* the actual src data */
    TPGrouple		pPatGr;		/* how to match */
    int			iDestroyFlag;	/* copy, remove, gimme, replace */
    int			iFreqFlag;	/* once or all occasions */

    LVAL		pXReplaceElt;	/* possible replacement data */
    TPTimeStamp		pStampTime;	/* optional time to stamp new data */
    TPTimeStamp		pTestTime;	/* optional time to compare with matched data */
    LVAL		pXResult;	/* result data to pass back */

    } TXMandRRec,
      *TPXMandRRec,
      **THXMandRRec;


typedef struct {
    boolean	bOrdered;
    boolean	bExpContent;
    boolean	bExpOrder;
    boolean	bMarkedWithin;
    boolean	bTouchedWithin;

    boolean	bMarkNextElt;
    boolean	bTouchNextElt;
    boolean	bMustEnd;
    boolean	bGetAnother;

    } TPatStatRec,
      *TPPatStatRec,
      **THPatStatRec;

/****************************************************************************************/

#define NATIVE_BADTYPE		-10
#define NATIVE_NOKERNEL		-11
#define NATIVE_BADFREQ		-12
#define NATIVE_2KERNELS		-13
#define NATIVE_BADVTYPE		-14
#define NATIVE_THISWHAT		-15
#define NATIVE_TOOMANYMARKS	-16
#define NATIVE_CANTMIX		-17
#define NATIVE_NOREPLACEMARK	-18
#define NATIVE_NOFETCHMARK	-19
#define NATIVE_NOVOID		-20
#define NATIVE_BADPATSYMBOL	-21
#define NATIVE_CRAZYWILD	-22
#define NATIVE_MATCHFAIL	-23
#define NATIVE_NODATA		-24
#define NATIVE_EMPTYELT		-25
#define NATIVE_STARMORE		-26
#define NATIVE_NOTEND		-27
#define NATIVE_BADVOID		-28
#define NATIVE_NOSTARN		-29
#define NATIVE_BADXTYPE		-30
#define NATIVE_NOHOST		-31
#define NATIVE_NOTOUCH		-32
#define NATIVE_MODVOID		-33

#define NATIVE_SYMBOL		10
#define NATIVE_STALE		-40

/****************************************************************************************/

extern LVAL 	xsendmsg0();
extern LVAL	s_unbound;
extern LVAL	true;
extern LVAL 	xlfatal();
extern LVAL	s_stderr;

/****************************************************************************************/

NEXTERN LVAL		s_InSpace, k_TestTime, k_Freq;
NEXTERN LVAL   		*hMsgList;
NEXTERN TXMandRRec	native_getPB, native_copyPB, native_putPB;

/****************************************************************************************/

#define NATIVE_INSPACE hMsgList

#define NATIVE_TIME_ARG(pTime, tTest) \
{ \
    LVAL		pXTime; \
    TTimeStamp		tRead; \
\
    if (xlgetkeyarg(k_TestTime, &pXTime) && !null(pXTime)) { \
	XELT2TIME(pXTime, tTest); \
	pTime = &tTest; \
\
	GET_TIME(tRead); \
	TIME2XELT(tRead, pXTime); \
	} \
    else \
	pTime = nil; \
    }


#define NATIVE_FREQ_ARG(iFlag) \
{ \
    LVAL	pXFreq; \
\
    if (xlgetkeyarg(k_Freq, &pXFreq) && \
	(strcmp((char *)getstring(pXFreq), "all") == 0)) \
	iFlag = NANCY_MatchMany; \
    else \
	iFlag = NANCY_MatchOne; \
    }

/****************************************************************************************/


