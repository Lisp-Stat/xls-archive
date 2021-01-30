/* xform_prims.h

   by dav lion, at the HITLab, Seattle

   Copyright (C) 1992  Human Interface Technology Lab, Seattle

   xlisp wrappers for C based matrix geometrical transformation routines

   this code is part of VEOS.
*/   


#ifndef _XFORMH_
#define _XFORMH_

#include <xlisp.h>

typedef float Matrix[4][4];		/* 4x4 matrix			 */
typedef float Vector[3];		/* Vector			 */
typedef float Quaternion[4];

extern LVAL xform01_identMat();
extern LVAL xform02_multMat();
extern LVAL xform03_translateMat();
extern LVAL xform04_PosQuat2Mat();
extern LVAL xform05_scaleMat();
extern LVAL xform06_shearMat();
extern LVAL xform07_copyMat();
extern LVAL xform08_multQuats();
extern LVAL xform09_Mat2PosQuat();

/* from vogl distribution
 * How to convert degrees to radians
 */
#define	PI	3.14159265358979
#define D2R	(PI / 180.0)


#define EPSILON 0.000001
#define FEPS(a,b)	    ((a>(b-.0001)) && (a<(b+.0001)))

static	Matrix	mIdentityMatrix = {
	    {1., 0., 0., 0.},
	    {0., 1., 0., 0.,},
	    {0., 0., 1., 0.,},
	    {0., 0., 0., 1.,}
	    };/*identityMatrix*/

#define v_float(v, n) getflonum(getelement(v, n))
#define m_float(m, i, j) getflonum(getelement(m, (i * 4) + j))
#define v_fixnum(v, n) getfixnum(getelement(v, n))

#define stuff_fixnum(arg, ind, val) ((arg)->n_vdata[ind])->n_fixnum = (val)
#define stuff_flonum(arg, ind, val) ((arg)->n_vdata[ind])->n_flonum = (val)



#endif
