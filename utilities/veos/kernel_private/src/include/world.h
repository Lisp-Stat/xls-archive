/****************************************************************************************
 * file: world.h									*
 *											*
 * May 18, 1991:  any veos code - kernel or prims - should use this include.		*
 * 	       										*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/

/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/


/****************************************************************************************
 ** common includes **
 ****************************************************************************************/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>

/****************************************************************************************
 ** common useful structures **
 ****************************************************************************************/


typedef int		TVeosErr;   /* return type of all veos functions */

typedef char		boolean;			

typedef char		str63[63];
typedef char		str15[15];
typedef char		str255[255];


typedef u_long		TTimeStamp, *TPTimeStamp, **THTimeStamp;

typedef struct {
    union {
	float  f;
	long   l;
	} u;
    } TF2L;

/****************************************************************************************
 ** the grouple structure **
 ****************************************************************************************/

/** grouple element types **/

#define GR_unspecified	0

#define GR_grouple	1
#define GR_vector	2
#define GR_float	3
#define GR_int		4
#define GR_prim		5
#define GR_string	6

#define GR_these	10
#define GR_theseall	11
#define GR_some		12
#define GR_any	       	13
#define GR_here		14

#define GR_mark		15
#define GR_touch	16


typedef struct grouple 	*TPGrouple;
typedef struct grouple 	**THGrouple;


typedef struct {
    int			iType;
    union {
	char		*pU;

	char		*pS;
	TPGrouple	pGr;

	float		fVal;
	int		iVal;

	} u;

    TTimeStamp		tLastMod;
    int			iFlags;

    } TElt,
      *TPElt,
      **THElt;


typedef struct grouple {
    	int 	    	iElts;
    	TElt	    	*pEltList;

	int		iFlags;

    	} TGrouple;

/****************************************************************************************
 ** common VEOS constants **
 ****************************************************************************************/

#ifndef TRUE
#define TRUE	1
#endif

#ifndef FALSE
#define FALSE	0
#endif

#ifndef nil
#define nil	0
#endif

/****************************************************************************************
 ** VEOS-wide return values **
 ****************************************************************************************/

#define VEOS_FAILURE		-1	   /* values of type TVeosErr */
#define VEOS_NEUTRAL		0
#define VEOS_SUCCESS		1

#define VEOS_EOF		-2
#define VEOS_MEM_ERR		-3
#define VEOS_FILE_ERR		-4
#define VEOS_DATA_ERR		-5

/****************************************************************************************
 ** common Nancy constants **
 ****************************************************************************************/

#define NANCY_LessThan			-217
#define NANCY_GreaterThan		-218
#define NANCY_EndOfGrouple		-220

#define NANCY_MisplacedLeftBracket	-223
#define NANCY_MisplacedRightBracket	-222
#define NANCY_MissingRightBracket       -224

#define NANCY_NoTypeMatch		-225
#define NANCY_BadType			-226

#define NANCY_MatchIncomplete		-229
#define NANCY_MatchOne			-230
#define NANCY_MatchMany			-231

#define NANCY_CopyMatch			-232
#define NANCY_RemoveMatch		-233
#define NANCY_GimmeMatch		-234
#define NANCY_ReplaceMatch		-235

#define NANCY_NoMatch			-236
#define NANCY_NotSupported		-237

#define NANCY_SrcTooShort		-238
#define NANCY_PatTooShort		-239

#define NANCY_Explicit 			-245
#define NANCY_Implicit 			-246

/****************************************************************************************
 ** common Shell constants **
 ****************************************************************************************/



/****************************************************************************************
 ** common Talk constants **
 ****************************************************************************************/

#define TALK_BOGUS_FD	-1		/* real file descriptors are non-neg */
#define TALK_BOGUS_HOST	-1
#define TALK_BOGUS_PORT	-1

/****************************************************************************************
 ** common useful macros **
 ****************************************************************************************/



/** SunOS requires 4th-word alignment when allocating memory on Sun 4's 
 ** but other machines must use same scheme for network compatibility. 
 ** ... lowest common denominator ... 
 **/
#define MEMSIZE(sz)  (((sz) + 3) & 0xFFFFFFFC)

#define MALLOC(sz)  malloc(MEMSIZE(sz))
#define REMALLOC(ptr, sz) realloc(ptr, MEMSIZE(sz))

#define NEWPTR(ptr, type, size)  (ptr = (type) MALLOC(size))
#define AGAINPTR(destptr, srcptr, type, size)  (destptr = (type) REMALLOC(srcptr, size))

#define DELETE(var)  	free((char *) var)
#define DUMP(ptr)	free((char *) ptr)


#define SETFLAG(flag, flagvar)		flagvar |= flag
#define CLRFLAG(flag, flagvar)		flagvar &= ~flag
#define TESTFLAG(flag, flagvar)		((flag & flagvar) ? TRUE : FALSE)

#define SAVE_FLAGS(flag, save)      { save = flag & NANCY_FlagMask;   \
				     flag &= ~NANCY_FlagMask; }

#define RESTORE_FLAGS(flag, save)   { flag |= save; }
					

#define TIME_LESS_THAN(time1, time2)     (time1 < time2)

#define CATCH_TRAP(iSignal, bTrapped) \
    if (TRAP_FLAGS & 0x00000001 << iSignal) { \
	TRAP_FLAGS = TRAP_FLAGS & ~(0x00000001 << iSignal); \
	TERMINATE = FALSE; \
	bTrapped = TRUE; \
	} \
    else \
	bTrapped = FALSE;

#define NANCY_EltMarkMask	0x40000000
#define NANCY_EltMatchMask	0x20000000
#define NANCY_EltTouchMask	0x10000000
#define NANCY_FlagMask		0x70000000

#define NANCY_MarkWithinMask 	0x00000001
#define NANCY_TouchWithinMask 	0x00000008
#define NANCY_ContentMask 	0x00000002
#define NANCY_VectorMask 	0x00000004

#define NEW_GROUPLE(pGrouple) \
{ \
    Nancy_NewGrouple(&pGrouple); \
    }

#define NEW_ELT(iType, pData, pElt) \
{ \
    Nancy_CreateElement(pElt, iType, 0); \
    bcopy((char *) pData, pElt->u.pU, TYPE_SIZES[iType]); \
    }

#define INSERT_ELT(pGrouple, pElt, iLoc) \
{ \
    Nancy_NewElementsInGrouple(pGrouple, iLoc, 1, GR_unspecified, 0); \
    pGrouple->pEltList[iLoc] = *pElt; \
    }  

#define charsymbolp(s, ch)      (symbolp(s) &&			\
				getstring(getpname(s))[0] == ch &&	\
				getstring(getpname(s))[1] == '\0')

#define TIME2XELT(time, pElt) \
{ \
    TF2L   	fTrans; \
    fTrans.u.l = time; \
    setflonum(pElt, fTrans.u.f); \
    }


#define XELT2TIME(pElt, time) \
{ \
    TF2L   	fTrans; \
    fTrans.u.f = getflonum(pElt); \
    time = fTrans.u.l; \
    }


/****************************************************************************************
 ** public globals setup by the kernel **
 ****************************************************************************************/

#ifdef MAIN_MODULE
str63			Veos_sUid;
boolean			Veos_bTerminate;
#else
extern str63		Veos_sUid;
extern boolean		Veos_bTerminate;
#endif

#define WHOAMI		Veos_sUid
#define TERMINATE	Veos_bTerminate

/****************************************************************************************
 ** C utils for prim programmers **
 ****************************************************************************************/

#ifdef _DEC_
extern char *strdup();
#endif

/****************************************************************************************

 ****************************************************************************************/


