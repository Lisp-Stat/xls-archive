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

#include "xlisp.h"
#include "world.h"
#include <math.h>
#include <sys/time.h>

extern LVAL	true;

typedef float 	TMatrix[4][4];
typedef float 	TTriple[3];
typedef float 	TVector[4];

LVAL ReverseList();
boolean IsTripleElt();


/****************************************************************************************/
LVAL read_time ()
{
  struct timeval t;
  double          now, diff;
  static double then = 0.0;
  int            err;

  err = gettimeofday( &t, 0);
/*
  fprintf( stderr, "%d %d\n", t.tv_sec, t.tv_usec);
*/
  if( err == -1)
    xlerror( "read-time: timer barfed");
  else
    {
      now = (double)t.tv_sec + (double)t.tv_usec / 1000000.0;
/*
      fprintf( stderr, "%f %f\n", now, then);
*/
      diff = now - then;
      then = now;
    }
  return cvflonum( diff);
}
/****************************************************************************************/



/****************************************************************************************
 *.native_sprintf -- data conversion.							*
 ****************************************************************************************/

LVAL native_sprintf()
{
    str255	sLocal;

    util_sprintf(sLocal);

    return(cvstring(sLocal));

    } /* native_sprintf */
/****************************************************************************************/


/****************************************************************************************
 *.native_printf -- data conversion.							*
 ****************************************************************************************/

LVAL native_printf()
{
    str255	sLocal;

    util_sprintf(sLocal);
    fprintf(stderr, "%s\n", sLocal);

    return(true);

    } /* native_printf */
/****************************************************************************************/



/****************************************************************************************
 *.native_printf1 -- data conversion.							*
 ****************************************************************************************/

LVAL native_printf1()
{
    str255	sLocal;

    util_sprintf(sLocal);
    fprintf(stderr, "%s", sLocal);

    return(true);

    } /* native_printf1 */
/****************************************************************************************/


/****************************************************************************************/
TVeosErr util_sprintf(sDest)
    char	*sDest;
{
    LVAL	pXElt;
    str63	sZoot;

    sDest[0] = '\0';

    while (moreargs()) {

	pXElt = xlgetarg();

	if (!null(pXElt)) {

	    switch (ntype(pXElt)) {

	    case FIXNUM:
		sprintf(sZoot, "%d", getfixnum(pXElt));
		strcat(sDest, sZoot);
		break;
	    
	    case FLONUM:
		sprintf(sZoot, "%.2f", getflonum(pXElt));
		strcat(sDest, sZoot);
		break;
		
	    case STRING:
		strcat(sDest, (char *) getstring(pXElt));
		break;

	    default:
		break;
		}
	    }
	}

    return(VEOS_SUCCESS);

    } /* util_sprintf */
/****************************************************************************************/




/****************************************************************************************
 *.native_sscanf -- data conversion.							*
 ****************************************************************************************/

LVAL native_sscanf()
{
    LVAL	pData;
    LVAL	pList, pXElt;
    char	*pDataFinger;

    xlsave1(pList);
    xlsave1(pXElt);

    pData = xlgastring();
    xllastarg();

    pDataFinger = (char *) getstring(pData);
    while (pDataFinger) {

	/** skip white space **/

	while (pDataFinger[0] == ' ')
	    pDataFinger ++;

	if (pDataFinger[0] == '\0')
	    break;

	/** StrToXElt() looks for ' ' or '\0' as delimiter **/

	StrToXElt(pDataFinger, &pXElt);
	pList = cons(pXElt, pList);

	pDataFinger = strchr(pDataFinger, ' ');
	}

    pList = ReverseList(pList);
	
    xlpopn(2);

    return(pList);

    } /* native_sscanf */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr XVUtils_LoadPrims()
{
    Xform_LoadPrims();

#define UTIL_LOAD
#include "xv_utils.h"
#undef UTIL_LOAD

    }
/****************************************************************************************/




/****************************************************************************************
 * StrToXElt	 									*/

TVeosErr StrToXElt(sData, hXElt)
    char		*sData;
    LVAL		*hXElt;
{
    TVeosErr 		iErr;
    char		*pFinger, cSave;
    int			iDots, iChars, iDigits;
    int			iType;
    LVAL		pXElt;
    float		fVal;
    int			iVal;

    iErr = VEOS_SUCCESS;
    iType = FREE;
    iDigits = iDots = iChars = 0;

    xlsave1(pXElt);

    pFinger = sData;

    /** minus not necessarily a character **/

    if (pFinger[0] == '-')
	pFinger ++;


    while (TRUE) {

	if (pFinger[0] == ' ' || pFinger[0] == '\0') {
	    break;
	    }

	if (isdigit(pFinger[0]))
	    iDigits ++;
	else if (pFinger[0] == '.')
	    iDots ++;
	else 
	    iChars ++;
	
	pFinger ++;
	}

    cSave = pFinger[0];
    pFinger[0] = '\0';

    if (iChars > 0 || iDots > 1)
	pXElt = cvstring(sData);

    else {
	if (iDots == 0) {
	    sscanf(sData, "%d", &iVal);
	    pXElt = cvfixnum(iVal);
	    }
	else {
	    sscanf(sData, "%f", &fVal);
	    pXElt = cvflonum(fVal);
	    }
	}

    pFinger[0] = cSave;

    *hXElt = pXElt;

    xlpop();

    return(iErr);

    } /* StrToXElt */
/****************************************************************************************/



/****************************************************************************************/
LVAL ReverseList(pList)
    LVAL	pList;
{
    LVAL	pSave, pXElt; 
    
    xlsave1(pSave); 
    xlsave1(pXElt); 

    while (!null(pList)) { 
	pSave = cdr(pList); 
	rplacd(pList, pXElt); 
	pXElt = pList; 
	pList = pSave; 
	} 

    xlpopn(2); 

    return(pXElt);
    
    } /* Native_ReverseList */
/****************************************************************************************/



/****************************************************************************************/
boolean	IsQuatElt(pXElt)
    LVAL	pXElt;
{
    return(vectorp(pXElt) &&
	   getsz(pXElt) == 2 &&
	   floatp(getelement(pXElt, 0)) &&
	   IsTripleElt(getelement(pXElt, 1)));

    } /* IsQuatElt */
/****************************************************************************************/


/****************************************************************************************/
boolean	IsMatrixElt(pXElt)
    LVAL	pXElt;
{
    return(vectorp(pXElt) && getsz(pXElt) == 16);

    } /* IsMatrixElt */
/****************************************************************************************/


/****************************************************************************************/
void XVect2Mat(pXElt, pMat)
    LVAL	pXElt;
    TMatrix	pMat;
{
    int		iEltIndex;

    /** assume sanity is checked **/
    for (iEltIndex = 0; iEltIndex < 16; iEltIndex ++)
	pMat[iEltIndex / 4][iEltIndex % 4] = getflonum(getelement(pXElt, iEltIndex));

    } /* XVect2Mat */
/****************************************************************************************/


/****************************************************************************************/
LVAL Mat2XVect(pMat)
    TMatrix	pMat;
{
    LVAL	pXElt;
    int		iEltIndex;

    xlsave1(pXElt);

    /** assume sanity is checked **/
    pXElt = newvector(16);

    for (iEltIndex = 0; iEltIndex < 16; iEltIndex ++)
	setelement(pXElt, iEltIndex, cvflonum(pMat[iEltIndex / 4][iEltIndex % 4]));

    xlpop();

    return(pXElt);

    } /* Mat2XVect */
/****************************************************************************************/


/****************************************************************************************/
boolean	IsTripleElt(pXElt)
    LVAL	pXElt;
{
    return(vectorp(pXElt) && getsz(pXElt) == 3);

    } /* IsTripleElt */
/****************************************************************************************/


/****************************************************************************************/
void XVect2Tri(pXElt, pTri)
    LVAL	pXElt;
    TTriple	pTri;
{
    /** assume sanity is checked **/

    pTri[0] = getflonum(getelement(pXElt, 0));
    pTri[1] = getflonum(getelement(pXElt, 1));
    pTri[2] = getflonum(getelement(pXElt, 2));

    } /* XVect2Tri */
/****************************************************************************************/


/****************************************************************************************/
LVAL Tri2XVect(pTri)
    TTriple	pTri;
{
    LVAL	pXElt;

    xlsave1(pXElt);

    /** assume sanity is checked **/
    pXElt = newvector(3);

    setelement(pXElt, 0, cvflonum(pTri[0]));
    setelement(pXElt, 1, cvflonum(pTri[1]));
    setelement(pXElt, 2, cvflonum(pTri[2]));

    xlpop();

    return(pXElt);

    } /* Tri2XVect */
/****************************************************************************************/


/****************************************************************************************/
void XVect2Quat(pXElt, pVect)
    LVAL	pXElt;
    TVector	pVect;
{
    LVAL	pTri;

    /** assume sanity is checked **/

    pVect[0] = getflonum(getelement(pXElt, 0));

    pTri = getelement(pXElt, 1);
    pVect[1] = getflonum(getelement(pTri, 0));
    pVect[2] = getflonum(getelement(pTri, 1));
    pVect[3] = getflonum(getelement(pTri, 2));
    
    } /* XVect2Quat */
/****************************************************************************************/


/****************************************************************************************/
LVAL Quat2XVect(pVect)
    TVector	pVect;
{
    LVAL	pXElt, pMid;

    /** assume sanity is checked **/

    xlsave1(pXElt);
    xlsave1(pMid);

    pMid = newvector(3);

    setelement(pMid, 0, cvflonum(pVect[1]));
    setelement(pMid, 1, cvflonum(pVect[2]));
    setelement(pMid, 2, cvflonum(pVect[3]));

    pXElt = newvector(2);
	       
    setelement(pXElt, 0, cvflonum(pVect[0]));
    setelement(pXElt, 1, pMid);

    xlpopn(2);

    return(pXElt);

    } /* Quat2XVect */
/****************************************************************************************/



/****************************************************************************************/
void
LispMat2Mat(lMat, pMat)
	LVAL	lMat;
	float	pMat[4][4];
{	
    pMat[0][0] = getflonum(getelement(lMat, 0));
    pMat[0][1] = getflonum(getelement(lMat, 1));
    pMat[0][2] = getflonum(getelement(lMat, 2));
    pMat[0][3] = getflonum(getelement(lMat, 3));

    pMat[1][0] = getflonum(getelement(lMat, 4));
    pMat[1][1] = getflonum(getelement(lMat, 5));
    pMat[1][2] = getflonum(getelement(lMat, 6));
    pMat[1][3] = getflonum(getelement(lMat, 7));

    pMat[2][0] = getflonum(getelement(lMat, 8));
    pMat[2][1] = getflonum(getelement(lMat, 9));
    pMat[2][2] = getflonum(getelement(lMat, 10));
    pMat[2][3] = getflonum(getelement(lMat, 11));

    pMat[3][0] = getflonum(getelement(lMat, 12));
    pMat[3][1] = getflonum(getelement(lMat, 13));
    pMat[3][2] = getflonum(getelement(lMat, 14));
    pMat[3][3] = getflonum(getelement(lMat, 15));
              
    
    }/*LispMat2Mat*/
/****************************************************************************************/


/****************************************************************************************/
void
Mat2LispMat(pMat, lMat)
     float	pMat[4][4];
     LVAL	lMat;
{

    stuff_flonum(lMat, 0,     pMat[0][0]);
    stuff_flonum(lMat, 1,     pMat[0][1]);
    stuff_flonum(lMat, 2,     pMat[0][2]);
    stuff_flonum(lMat, 3,     pMat[0][3]);
    
    stuff_flonum(lMat, 4,     pMat[1][0]);
    stuff_flonum(lMat, 5,     pMat[1][1]);
    stuff_flonum(lMat, 6,     pMat[1][2]);
    stuff_flonum(lMat, 7,     pMat[1][3]);
    
    stuff_flonum(lMat, 8,     pMat[2][0]);
    stuff_flonum(lMat, 9,     pMat[2][1]);
    stuff_flonum(lMat, 10,     pMat[2][2]);
    stuff_flonum(lMat, 11,     pMat[2][3]);
    
    stuff_flonum(lMat, 12,     pMat[3][0]);
    stuff_flonum(lMat, 13,     pMat[3][1]);
    stuff_flonum(lMat, 14,     pMat[3][2]);
    stuff_flonum(lMat, 15,     pMat[3][3]);

    }/*Mat2LispMat*/
/****************************************************************************************/

