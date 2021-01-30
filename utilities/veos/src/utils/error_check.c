/* error_check.c

   by Fran Taylor, at the HITLab, Seattle

   Copyright (C) 1992  Human Interface Technology Lab, Seattle

   Error checking functions for Xlisp functions written in C */

#include "xlisp.h"

int triplep(v)
LVAL v;
{
  int i;
  if (!vectorp(v) || (getsz(v) != 3))
    return 0;
  for(i = 0; i < 3; i++)
    if (!floatp(getelement(v, i)))
      return 0;
  return 1;
}

int quaternionp(v)
LVAL v;
{
  LVAL p;
  int i;

  if (!vectorp(v) || (getsz(v) != 2) || !floatp(getelement(v, 0)))
    return 0;
  if (!vectorp(p = getelement(v, 1)) || (getsz(p) != 3))
    return 0;
  for(i = 0; i < 3; i++)
    if (!floatp(getelement(p, i)))
      return 0;
  return 1;
}

int pt_quatp(x)
LVAL x;
{
  if (!vectorp(x) || (getsz(x) != 2) || !triplep(getelement(x, 0)) ||
      !quaternionp(getelement(x, 1)))
    return 0;
  return 1;
}

int pt_eulerp(x)
LVAL x;
{
  if (!vectorp(x) || (getsz(x) != 2) || !triplep(getelement(x, 0)) ||
      !triplep(getelement(x, 1)))
    return 0;
  return 1;
}

int matrixp(m)
LVAL m;
{
  int i;

  if (!vectorp(m) || (getsz(m) != 16))
    return 0;
  for(i = 0; i < 16; i++)
    if (!floatp(getelement(m, i)))
      return 0;
  return 1;
}

   
