/* xform_prims.c

   by dav lion, at the HITLab, Seattle

   Copyright (C) 1992  Human Interface Technology Lab, Seattle

   xlisp wrappers for C based matrix geometrical transformation routines

   this code is part of VEOS.
*/


/* xform10_MakeQuaternion() and
   xform11_NormalizeQuaternion()
   by Andrew MacDonald,
   9 Mar 1992

   xform04_PosQuat2Mat and xform08_multQuats() fixed to normalize correctly
   9 Mar 1992

   xform12_PointTimesQuat()
   8 Apr 1992
*/

/****************************************************************************/
/* 			             preliminaries		       	*/

#include <stdio.h>
#include <math.h>
#include <world.h>
#include "xform_prims.h"
/* . . . . . . . . . . . . . . F O R W A R D S . . . . . . . . . . . .  */
LVAL xform01_identmatrix();
LVAL xform02_multmatrix();
LVAL xform03_translateMat();
LVAL xform04_PosQuat2Mat();
LVAL xform05_scaleMat();
LVAL xform06_shearMat();
LVAL xform07_copyMat();
LVAL xform08_multQuats();
LVAL xform09_Mat2PosQuat();
LVAL xform10_MakeQuaternion();
LVAL xform11_NormalizeQuaternion();
LVAL xform12_PointTimesQuat();

void Xform_LoadPrims();




/* . . . . . .. .... . . . . . G L O B A L S . . . . . . . . . . . . . .*/



/* . . . . . . . . . . . . . . E X T E R N S  . . . .  . . . . . . . . .*/


extern LVAL	true;    
extern int matrixp();    
    
/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/


void
Xform_LoadPrims()
{
    
    xldefine_prim("IDENTITYMATRIX", 	SUBR, xform01_identMat);
    xldefine_prim("MULTMATRIX", 	SUBR, xform02_multMat);
    xldefine_prim("TRANSLATEMATRIX", 	SUBR, xform03_translateMat);
    xldefine_prim("POSQUAT2MAT", 	SUBR, xform04_PosQuat2Mat);
    xldefine_prim("SCALEMATRIX", 	SUBR, xform05_scaleMat);
    xldefine_prim("SHEARMATRIX", 	SUBR, xform06_shearMat);
    xldefine_prim("COPYMATRIX", 	SUBR, xform07_copyMat);
    xldefine_prim("MULTQUATS",		SUBR, xform08_multQuats);
    xldefine_prim("MAT2POSQUAT",	SUBR, xform09_Mat2PosQuat);
    xldefine_prim("MAKEQUAT",		SUBR, xform10_MakeQuaternion);
    xldefine_prim("NORMQUAT",		SUBR, xform11_NormalizeQuaternion);
    xldefine_prim("POINTXQUAT",		SUBR, xform12_PointTimesQuat);
    
    }/*XForm_LoadPrims*/





void
xform_normalizeVec(target, pMagnitude)
float	target[3];
float	*pMagnitude;
{
    float	magnitude;
    
#ifdef SGI
    magnitude = fsqrt(target[0] * target[0] +
		      target[1] * target[1] +
		      target[2] * target[2] );
#else
    magnitude = (float)sqrt((double)(target[0] * target[0] +
			    target[1] * target[1] +
			    target[2] * target[2] ));
#endif
    
    
    if (magnitude > EPSILON) {
	
	target[0] /= magnitude;
	target[1] /= magnitude;
	target[2] /= magnitude;
	}/*endif sane */
    else
	magnitude = 0.0;
    
    *pMagnitude = magnitude;
    }/*vecNormalizeVec*/


float
xform_magnitude( v)
    Vector	v;
{
    return( sqrt( v[0] * v[0] + v[1] * v[1] + v[2] * v[2]));
}


LVAL
xform01_identMat()
{
    LVAL	lMat;
    int		iCounter;

    lMat = xlgetarg();

    if (matrixp(lMat)) {
	xllastarg();

	stuff_flonum(lMat, 0, 1.0);
	stuff_flonum(lMat, 1, 0.0);
	stuff_flonum(lMat, 2, 0.0);
	stuff_flonum(lMat, 3, 0.0);

	stuff_flonum(lMat, 4, 0.0);
	stuff_flonum(lMat, 5, 1.0);
	stuff_flonum(lMat, 6, 0.0);
	stuff_flonum(lMat, 7, 0.0);

	stuff_flonum(lMat, 8, 0.0);
	stuff_flonum(lMat, 9, 0.0);
	stuff_flonum(lMat, 10, 1.0);
	stuff_flonum(lMat, 11, 0.0);

	stuff_flonum(lMat, 12, 0.0);
	stuff_flonum(lMat, 13, 0.0);
	stuff_flonum(lMat, 14, 0.0);
	stuff_flonum(lMat, 15, 1.0);

	}/*endif happy */
    else
	xlerror("Not a Matrix");

    return (true);

    }/*xform01_identMat*/




LVAL
xform02_multMat()
{
    LVAL	lM1;
    LVAL	lM2;
    LVAL	lM3;

    static	Matrix	temp;

    int		y;
    int		x;

    lM1 = xlgetarg();
    if (matrixp(lM1)) {

	lM2 = xlgetarg();
	if (matrixp(lM2)) {
	    lM3 = xlgetarg();
	    if (matrixp(lM3)) {
		xllastarg();
		
		for(y=0; y<4 ; y++) 
		    for(x=0 ; x<4 ; x++) {
			temp[y][x] = ( m_float(lM2, y, 0) * m_float(lM1, 0, x)
				      + m_float(lM2, y, 1) * m_float(lM1, 1, x)
				      + m_float(lM2, y, 2) * m_float(lM1, 2, x)
				      + m_float(lM2, y, 3) * m_float(lM1, 3, x));
			}/*for x*/
		
		Mat2LispMat(temp,lM3);
	    
		}/*endif happy*/
	    else
		xlerror("arg 3 not a matrix");
	    }/*endif arg2 a matrix*/
	else
	    xlerror("arg2 not a matrix");
	}/*endif arg1 a matrix*/
    else
	xlerror("arg 1 not a matrix");

    return (true);

    }/*xform02_multMat*/



LVAL
xform03_translateMat()
{
    LVAL	lMat;
    LVAL	lTri;

    lMat = xlgetarg();
    if (matrixp(lMat)) {
	lTri = xlgetarg();
	if (triplep(lTri)) {
	    xllastarg();
	    
	    stuff_flonum(lMat, 0, 1.0);
	    stuff_flonum(lMat, 1, 0.0);
	    stuff_flonum(lMat, 2, 0.0);
	    stuff_flonum(lMat, 3, 0.0);
	    
	    stuff_flonum(lMat, 4, 0.0);
	    stuff_flonum(lMat, 5, 1.0);
	    stuff_flonum(lMat, 6, 0.0);
	    stuff_flonum(lMat, 7, 0.0);
	    
	    stuff_flonum(lMat, 8, 0.0);
	    stuff_flonum(lMat, 9, 0.0);
	    stuff_flonum(lMat, 10, 1.0);
	    stuff_flonum(lMat, 11, 0.0);
	    
	    stuff_flonum(lMat, 12, v_float(lTri, 0));
	    stuff_flonum(lMat, 13, v_float(lTri, 1));
	    stuff_flonum(lMat, 14, v_float(lTri, 2));
	    stuff_flonum(lMat, 15, 1.0);
	    
	    }/*endif happy*/
	else 
	    xlerror("arg 2 not a triple ");
	}/*endif found matrix */
    else
	xlerror("arg 1 not a matrix");

    return (true);

    }/*xform03_translateMat*/




LVAL
xform05_scaleMat()
{
    LVAL	lMat;
    LVAL	lTri;

    lMat = xlgetarg();
    if (matrixp(lMat)) {

	lTri = xlgetarg();
	if (triplep(lTri)) {
	    xllastarg();
	    
	    stuff_flonum(lMat, 0, v_float(lTri,0));
	    stuff_flonum(lMat, 1, 0.0);
	    stuff_flonum(lMat, 2, 0.0);
	    stuff_flonum(lMat, 3, 0.0);
	    
	    stuff_flonum(lMat, 4, 1.0);
	    stuff_flonum(lMat, 5, v_float(lTri,1));
	    stuff_flonum(lMat, 6, 0.0);
	    stuff_flonum(lMat, 7, 0.0);
	    
	    stuff_flonum(lMat, 8, 1.0);
	    stuff_flonum(lMat, 9, 0.0);
	    stuff_flonum(lMat, 10, v_float(lTri,2));
	    stuff_flonum(lMat, 11, 0.0);
	    
	    stuff_flonum(lMat, 12, 0.0);
	    stuff_flonum(lMat, 13, 0.0);
	    stuff_flonum(lMat, 14, 0.0);
	    stuff_flonum(lMat, 15, 1.0);
	    
	    }/*endif happy*/
	else 
	    xlerror("arg 2 not a triple");
	}/*endif found matrix*/
    else
	xlerror("arg 1 not a matrix");

    return (true);
    }/*xform05_scaleMat*/


LVAL
xform06_shearMat()
{
    LVAL	lMat;
    LVAL	lX;
    LVAL	lY;

    lMat = xlgetarg();
    if (matrixp(lMat)) {
	
	lX = xlgaflonum();
	lY = xlgaflonum();
	xllastarg();
	
	stuff_flonum(lMat, 0, 1.0);
	stuff_flonum(lMat, 1, 0.0);
	stuff_flonum(lMat, 2, 0.0);
	stuff_flonum(lMat, 3, 0.0);
	
	stuff_flonum(lMat, 4, getflonum(lX));
	stuff_flonum(lMat, 5, getflonum(lY));
	stuff_flonum(lMat, 6, 0.0);
	stuff_flonum(lMat, 7, 0.0);
	
	stuff_flonum(lMat, 8, 0.0);
	stuff_flonum(lMat, 9, 0.0);
	stuff_flonum(lMat, 10, 1.0);
	stuff_flonum(lMat, 11, 0.0);
	
	stuff_flonum(lMat, 12, 0.0);
	stuff_flonum(lMat, 13, 0.0);
	stuff_flonum(lMat, 14, 0.0);
	stuff_flonum(lMat, 15, 1.0);
	
	}/*endif found matrix*/
    else
	xlerror("arg 1 not a matrix");
    
    return (true);
    }/*xform06_shearMat*/



LVAL
xform07_copyMat()
{
    LVAL	lSrc;
    LVAL	lDest;

    lSrc = xlgetarg();
    if (matrixp(lSrc)) {
	lDest = xlgetarg();
	if (matrixp(lDest)){
	    
	    lDest->n_vdata[0]->n_flonum = lSrc->n_vdata[0]->n_flonum;
	    lDest->n_vdata[1]->n_flonum = lSrc->n_vdata[1]->n_flonum;
	    lDest->n_vdata[2]->n_flonum = lSrc->n_vdata[2]->n_flonum;
	    lDest->n_vdata[3]->n_flonum = lSrc->n_vdata[3]->n_flonum;


	    lDest->n_vdata[4]->n_flonum = lSrc->n_vdata[4]->n_flonum;
	    lDest->n_vdata[5]->n_flonum = lSrc->n_vdata[5]->n_flonum;
	    lDest->n_vdata[6]->n_flonum = lSrc->n_vdata[6]->n_flonum;
	    lDest->n_vdata[7]->n_flonum = lSrc->n_vdata[7]->n_flonum;

	    lDest->n_vdata[8]->n_flonum = lSrc->n_vdata[8]->n_flonum;
	    lDest->n_vdata[9]->n_flonum = lSrc->n_vdata[9]->n_flonum;
	    lDest->n_vdata[10]->n_flonum = lSrc->n_vdata[10]->n_flonum;
	    lDest->n_vdata[11]->n_flonum = lSrc->n_vdata[11]->n_flonum;

	    lDest->n_vdata[12]->n_flonum = lSrc->n_vdata[12]->n_flonum;
	    lDest->n_vdata[13]->n_flonum = lSrc->n_vdata[13]->n_flonum;
	    lDest->n_vdata[14]->n_flonum = lSrc->n_vdata[14]->n_flonum;
	    lDest->n_vdata[15]->n_flonum = lSrc->n_vdata[15]->n_flonum;

	    }/*endif happy*/
	else
	    xlerror("arg 2 not a matrix");
	}/*endif found src matrix*/
    else
	xlerror("arg 1 not a matrix");

    return (true);

    }/*xform07_copyMat*/



LVAL
xform04_PosQuat2Mat()
{
    LVAL	lPos;
    LVAL	lQuat;
    LVAL	lMat;

     float x2, y2, z2, xx2, yy2, zz2, xy2, xz2, xw2, yw2, yz2, zw2;
     float	fMagnitude;
     Quaternion q;
    
    lPos = xlgetarg();
    if (triplep(lPos)){
	lQuat = xlgetarg();
	if (quaternionp(lQuat)) {
	    lMat = xlgetarg();
	    if (matrixp(lMat)) {
		
		
		q[0] = getflonum(getelement(lQuat, 0));
		q[1] = getflonum(getelement(getelement(lQuat,1),0));
		q[2] = getflonum(getelement(getelement(lQuat,1),1));
		q[3] = getflonum(getelement(getelement(lQuat,1),2));
		
/*		fMagnitude = xform_magnitude(&(q[1]));
		if (fMagnitude == 0.0)
		    xlerror("rotate: zero length vector");
*/
#ifdef DEBUG
		fprintf(stderr, "quat after is <%f,%f,%f,%f>\n",
			q[0], q[1], q[2], q[3]);
#endif		
		x2 = q[1] + q[1];
		y2 = q[2] + q[2];
		z2 = q[3] + q[3];
		
		xx2 = q[1] * x2;
		yy2 = q[2] * y2;
		zz2 = q[3] * z2;
		xy2 = q[1] * y2;
		xz2 = q[1] * z2;
		yz2 = q[2] * z2;
		xw2 = q[0] * x2;
		yw2 = q[0] * y2;
		zw2 = q[0] * z2;
		
		stuff_flonum(lMat, 0, 1.0 - yy2 - zz2);
		stuff_flonum(lMat, 1, xy2 + zw2);
		stuff_flonum(lMat, 2,  xz2 - yw2);
		stuff_flonum(lMat, 4, xy2 - zw2);
		stuff_flonum(lMat, 5, 1.0 - xx2 - zz2);
		stuff_flonum(lMat, 6, yz2 + xw2);
		stuff_flonum(lMat, 8, xz2 + yw2);
		stuff_flonum(lMat, 9, yz2 - xw2);
		stuff_flonum(lMat, 10, 1.0 - xx2 - yy2);

		stuff_flonum(lMat, 12, v_float(lPos,0));
		stuff_flonum(lMat, 13, v_float(lPos,1));
		stuff_flonum(lMat, 14, v_float(lPos,2));

		stuff_flonum(lMat, 3, 0.0);
		stuff_flonum(lMat, 7, 0.0);
		stuff_flonum(lMat, 11, 0.0);
		stuff_flonum(lMat, 15, 1.0);
		
		
		}/*endif sane*/
	    else
		xlerror("PosQuat2Mat: arg 3 not a matrix");
	    }/*endif found matrix*/
	else
	    xlerror("PosQuat2Mat: arg 2 not a quaternion");
	}/*endif found triple */
    else
	xlerror("PosQuat2Mat: arg 1 not a triple");

    return (true);

     }/*xform04_PosQuat2Mat*/
	 


LVAL
xform08_multQuats()
{
    Quaternion	q1;
    Quaternion 	q2;
    Quaternion	q3;
    
    LVAL	lQ1;
    LVAL	lQ2;
    LVAL	lQ3;
    LVAL	lQ3elt;				  /* element of lQ3          */

    float	fMagnitude;
    Vector	vCrossProduct;

    lQ1 = xlgetarg();
    if (quaternionp(lQ1)) {
	lQ2 = xlgetarg();
	if (quaternionp(lQ2)){
	    lQ3 = xlgetarg();
	    if (quaternionp(lQ3)) {

		lQ3elt = getelement(lQ3,1);	  /* will need this for stuffing */

		q1[0] = getflonum(getelement(lQ1, 0));
		q1[1] = getflonum(getelement(getelement(lQ1,1),0));
		q1[2] = getflonum(getelement(getelement(lQ1,1),1));
		q1[3] = getflonum(getelement(getelement(lQ1,1),2));
		
		
		q2[0] = getflonum(getelement(lQ2, 0));
		q2[1] = getflonum(getelement(getelement(lQ2,1),0));
		q2[2] = getflonum(getelement(getelement(lQ2,1),1));
		q2[3] = getflonum(getelement(getelement(lQ2,1),2));

		if (FEPS(q1[0],0.0)) {
		    if (!FEPS(q2[0],0.0)) {
			stuff_flonum(lQ3, 0, q2[0]);
			stuff_flonum(lQ3elt, 0, q2[1]);
			stuff_flonum(lQ3elt, 1, q2[2]);
			stuff_flonum(lQ3elt, 2, q2[3]);
			}/*endif q1 bad, q2 good*/
/*		    else {
			xlerror("multquats: q1 and q2 have no rotation");
			}/*else both bad*/
		    }/*endif q1 bad*/
		else
		    { /* q1 must be ok, so check q2 */
		    if (FEPS(q2[0],0.0)) {
			stuff_flonum(lQ3, 0, q1[0]);
			stuff_flonum(lQ3elt, 0, q1[1]);
			stuff_flonum(lQ3elt, 1, q1[2]);
			stuff_flonum(lQ3elt, 2, q1[3]);
			}/*endif q1 ok, q2 bad*/
		    else 
			{ /* else both ok*/
/*			fMagnitude = xform_magnitude(&(q1[1]));
			if (fMagnitude == 0.0)
			    xlerror("multquats: arg 1 zero length vector");
			
			fMagnitude = xform_magnitude(&(q1[1]));
			if (fMagnitude == 0.0)
			    xlerror("mulquats: arg 2 zero length vector");
*/			
			
			vCrossProduct[0] = q1[2] * q2[3] - q1[3] * q2[2];
			vCrossProduct[1] = q1[3] * q2[1] - q1[1] * q2[3];
			vCrossProduct[2] = q1[1] * q2[2] - q1[2] * q2[1];

			
			q3[0] = (q1[0] * q2[0]) - 
			    ((q1)[1]*(q2)[1] + (q1)[2]*(q2)[2] + (q1)[3]*(q2)[3]);
						  /* line above is dot product */

			q3[1] = (q1[0] * q2[1]) + (q2[0] * q1[1]) + vCrossProduct[0];
			q3[2] = (q1[0] * q2[2]) + (q2[0] * q1[2]) + vCrossProduct[1];
			q3[3] = (q1[0] * q2[3]) + (q2[0] * q1[3]) + vCrossProduct[2];
			
/*			fMagnitude = xform_magnitude(&(q3[1]));
			if (fMagnitude == 0.0)
			    xlerror("multquats: result zero length vector");
*/			
			stuff_flonum(lQ3, 0, q3[0]);
			stuff_flonum(lQ3elt, 0, q3[1]);
			stuff_flonum(lQ3elt, 1, q3[2]);
			stuff_flonum(lQ3elt, 2, q3[3]);
			
			
			}/*else both are ok*/
		    }/*else*/
		}/*endif happy*/
	    else
		xlerror("multquats: arg3 not quaternion");
	    }/*endif got quat2*/
	else
	    xlerror("multquats, arg2 not quaternion");
	}/*endif arg1 quat*/
    else
	xlerror("multquats: arg1 not quaternion");
    
    

    return (true);

    }/*xform08_multiplyQuats*/



LVAL
xform09_Mat2PosQuat()
{
    LVAL	lPos;
    LVAL	lQuat;
    LVAL	lMat;

    float w2, w4, x2, y2;
    Quaternion q;
    Matrix	m;
    
    lMat = xlgetarg();
    if (matrixp(lMat)) {
	lPos = xlgetarg();
	if (triplep(lPos)){
	    lQuat = xlgetarg();
	    if (quaternionp(lQuat)) {
		
		w2 = 0.25 * (m_float(lMat, 0, 0) + m_float(lMat, 1, 1) 
			     + m_float(lMat, 2, 2) + m_float(lMat, 3, 3));
		if (w2 > EPSILON)
		    {
		    q[0] = sqrt(w2);
		    w4 = 4.0 * q[0];
		    q[1] = (m_float(lMat, 1, 2) - m_float(lMat, 2, 1)) / w4;
		    q[2] = (m_float(lMat, 2, 0) - m_float(lMat, 0, 2)) / w4;
		    q[3] = (m_float(lMat, 0, 1) - m_float(lMat, 1, 0)) / w4;
		    }
		else
		    {
		    q[0] = 0.0;
		    x2 = -0.5 * (m_float(lMat, 1, 1) + m_float(lMat, 2, 2));
		    if (x2 > EPSILON)
			{
			q[1] = sqrt(x2);
			x2 = 2.0 * q[1];
			q[2] = m_float(lMat, 0, 1) / x2;
			q[3] = m_float(lMat, 0, 2) / x2;
			}
		    else
			{
			q[1] = 0.0;
			y2 = 0.5 * (1.0 - m_float(lMat, 2, 2));
			if (y2 > EPSILON)
			    {
			    q[2] = sqrt(y2);
			    q[3] = m_float(lMat, 1, 2) / (2.0 * q[2]);
			    }
			else
			    {
			    q[2] = 0.0;
			    q[3] = 1.0;
			    }
			}
		    }
	    
		stuff_flonum(lQuat, 0, q[0]);
		stuff_flonum(lQuat, 1, q[1]);
		stuff_flonum(lQuat, 2, q[2]);
		stuff_flonum(lQuat, 3, q[3]);

		stuff_flonum(lPos, 0, m_float(lMat, 3, 0));
		stuff_flonum(lPos, 1, m_float(lMat, 3, 1));
		stuff_flonum(lPos, 2, m_float(lMat, 3, 2));
		
		
	    }/*endif sane*/
	    else
		xlerror("Mat2Quat: arg 3 not a quaternion");
	    }/*endif found matrix*/
	else
	    xlerror("Mat2Quat: arg 2 not a triple");
	}/*endif found triple */
    else
	xlerror("Mat2Quat: arg 1 not a matrix");

    return (true);
    }/*xform09_Mat2PosQuat*/


/* make a normalized quaternion from an angle (in radians) and a non-zero vector
   (indicating axis of rotation).  Third argument is quaternion to return result
   in. */
LVAL
xform10_MakeQuaternion()
{
    LVAL	lRot;
    LVAL	lVec;
    LVAL	lQuat;

    LVAL	lQuatElt;
    register float	norm, mag, w, x, y, z;
    
    lRot = xlgaflonum();
    lVec = xlgetarg();
    if( triplep( lVec))
    {
	lQuat = xlgetarg();
	if( quaternionp( lQuat))
	{
	    w = cos( getflonum(lRot) / 2.0) * (getflonum(lRot) < 0.0 ? -1.0 : 1.0);
	    x = getflonum( getelement( lVec, 0));
	    y = getflonum( getelement( lVec, 1));
	    z = getflonum( getelement( lVec, 2));
	    
	       /* actually magnitude squared */
	    mag = x * x + y * y + z * z;

	    if( mag == 0.0)
		xlerror("MakeQuaternion: vector has zero length");
	    else
	    {
		   /* normalizing factor to apply to the vector.
		      the angle element is already the RIGHT THING, so we have
		      to make magnitude 1.0 while keeping w constant */
		norm = sqrt( (1.0 - w * w) / mag);
		
		lQuatElt = getelement( lQuat, 1);
		stuff_flonum( lQuat, 0, w);
		stuff_flonum( lQuatElt, 0, x * norm);
		stuff_flonum( lQuatElt, 1, y * norm);
		stuff_flonum( lQuatElt, 2, z * norm);
	    }
	}
	else
	    xlerror("MakeQuaternion: arg3 not quaternion");
    }
    else
	xlerror("MakeQuaternion: arg2 not vector");
    
    return( true);
}

	    
		
/* normalize a quaternion (the argument).  Assumes the first component, the
   angle, is already in standard form -- cos( angle / 2).  Only fusses with
   the vector component */
LVAL
xform11_NormalizeQuaternion()
{
    LVAL	lQuat;
    
    LVAL	lQuatElt;
    register float	norm, mag, w, x, y, z;
    
    lQuat = xlgetarg();
    if( quaternionp( lQuat))
    {
	w = getflonum( getelement( lQuat, 0));
	lQuatElt = getelement( lQuat, 1);
	x = getflonum( getelement( lQuatElt, 0));
	y = getflonum( getelement( lQuatElt, 1));
	z = getflonum( getelement( lQuatElt, 2));

	    /* actually magnitude squared */
	mag = x * x + y * y + z * z;

	if( mag == 0.0)
	    xlerror("NormalizeQuaternion: vector part has zero length");
	else
	{
	        /* normalizing factor to apply to the vector.
		   the angle element is already the RIGHT THING, so we have
		   to make magnitude 1.0 while keeping w constant */
	    norm = sqrt( (1.0 - w * w) / mag);
		
	    stuff_flonum( lQuat, 0, w);
	    stuff_flonum( lQuatElt, 0, x * norm);
	    stuff_flonum( lQuatElt, 1, y * norm);
	    stuff_flonum( lQuatElt, 2, z * norm);
	}
    }
    else
	xlerror("NormalizeQuaternion: arg1 not quaternion");

    return( true);
}

LVAL
xform12_PointTimesQuat()
{
    LVAL	lPoint;
    LVAL	lQuat;
    LVAL	lResult;

    float x2, y2, z2, xx2, yy2, zz2, xy2, xz2, xw2, yw2, yz2, zw2;
    float a, b, c;
    Quaternion q;
    
    lPoint = xlgetarg();
    if (triplep(lPoint)){
	lQuat = xlgetarg();
	if (quaternionp(lQuat)) {
	    lResult = xlgetarg();
	    if (triplep(lResult)) {
		
		q[0] = getflonum(getelement(lQuat, 0));
		q[1] = getflonum(getelement(getelement(lQuat,1),0));
		q[2] = getflonum(getelement(getelement(lQuat,1),1));
		q[3] = getflonum(getelement(getelement(lQuat,1),2));
		
		x2 = q[1] + q[1];
		y2 = q[2] + q[2];
		z2 = q[3] + q[3];
		
		xx2 = q[1] * x2;
		yy2 = q[2] * y2;
		zz2 = q[3] * z2;
		xy2 = q[1] * y2;
		xz2 = q[1] * z2;
		yz2 = q[2] * z2;
		xw2 = q[0] * x2;
		yw2 = q[0] * y2;
		zw2 = q[0] * z2;
		
		a = getflonum(getelement(lPoint, 0));
		b = getflonum(getelement(lPoint, 1));
		c = getflonum(getelement(lPoint, 2));

		stuff_flonum(lResult, 0, 
			     a * (1.0 - yy2 - zz2) +
			     b * (xy2 - zw2) +
			     c * (xz2 + yw2));

		stuff_flonum(lResult, 1,
			     a * (xy2 + zw2) +
			     b * (1 - xx2 - zz2) +
			     c * (yz2 - xw2));

		stuff_flonum(lResult, 2,
			     a * (xz2 - yw2) +
			     b * (yz2 + xw2) +
			     c * (1 - xx2 - yy2));
		}/*endif sane*/
	    else
		xlerror("PointTimesQuat: arg 3 not a triple");
	    }/*endif found matrix*/
	else
	    xlerror("PointTimesQuat: arg 2 not a quaternion");
	}/*endif found triple */
    else
	xlerror("PointTimesQuat: arg 1 not a triple");

    return (true);

     }/* xform12_PointTimesQuat */
	 
