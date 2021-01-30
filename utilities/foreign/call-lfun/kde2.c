/* 

This is the same as kde1.c, but I use xscall_alloc here.

*/


#include "xlisp.h"
#include "myxlisp.h"
#include <math.h> 

#define value(x) (getflonum(car(x)))

extern LVAL sk_type, sk_width;

#ifndef ROOT2PI
#define ROOT2PI 2.50662827463100050241
#endif ROOT2PI

#ifndef ROOT5
#define ROOT5   2.23606797749978969641
#endif ROOT5


LVAL kde()
{
  LVAL x_arg, xo_arg, yo_arg, next, nextx, nexty, w_arg, t_arg;
  FIXTYPE i, n, ns, ktype;
  FLOTYPE xmin, xmax, temp, temp1, width, sum, *p;
  
  x_arg = xlgalist() ; /* the list of data points */
  n = llength(x_arg);
  ns = 30;

  if (! xlgetkeyarg(sk_type, &t_arg)) t_arg = NIL;
  if (! xlgetkeyarg(sk_width, &w_arg)) w_arg = NIL;
  
  if (symbolp(t_arg)) {
    switch (getstring(getpname(t_arg))[0]) {
    case 'U': ktype = 'U'; break;
    case 'T': ktype = 'T'; break;
    case 'G': ktype = 'G'; break;
    default:  ktype = 'B'; break;
    }
  }
  else
    ktype='B';

/*  xllastarg();  /* Complain if more args */
  xlstkcheck(2);
  xlsave(xo_arg);
  xlsave(yo_arg);

  xo_arg = mklist(ns,NIL);
  yo_arg = mklist(ns,NIL);

  /* Find max and min of the data */
  xmax = value(x_arg);
  xmin = xmax;

  for (next = cdr(x_arg); consp(next); next = cdr(next)) {
    temp = value(next);
    if (xmax < temp) xmax = temp;
    if (xmin > temp) xmin = temp;
  }

  /* Set kernel width */
  if (w_arg)
    width = makedouble(w_arg);
  else 
    width = (xmax - xmin) / (1.0 + log((double) n));

  if (width <= 0.0) xlfail("bad window width");

  /* Create an equally spaced grid between xmax and ymax in a work array*/
  p = (FLOTYPE *) xscall_alloc(ns,sizeof(FLOTYPE)); 
  temp = (xmax - xmin) / (ns - 1);
  for (i = 0; i < ns; i++)
    p[i]=xmin + i * temp;
  
  /* Compute the Y values */
  switch (ktype) {
  case 'G':			/* Gaussian Kernel */
    for (i = 0, nextx = xo_arg, nexty = yo_arg; i < ns; i++, nextx = cdr(nextx), nexty = cdr(nexty)) {
      temp1 = p[i];
      for (next = x_arg, sum = 0.0; consp(next); next = cdr(next)) {
	temp = 4.0 * (temp1  - value(next)) / width;
	sum += exp(-0.5 * temp * temp);
      }
      rplaca(nextx,cvflonum(temp1));
      rplaca(nexty,cvflonum(sum / (n * 0.25 * width * ROOT2PI)));
    }
    break;
  case 'U':			/* Uniform Kernel */
    for (i = 0, nextx = xo_arg, nexty = yo_arg; i < ns; i++, nextx = cdr(nextx), nexty = cdr(nexty)) {
      temp1 = p[i];
      for (next = x_arg, sum = 0.0; consp(next); next = cdr(next)) {
	temp = 0.75 * (temp1 - value(next)) / width;
	sum += (fabs(temp) < 0.5) ? 1.0 : 0.0;
      }
      rplaca(nextx,cvflonum(temp1));
      rplaca(nexty,cvflonum(sum / (n * 1.5 * width)));
    }
    break;
    
  case 'T':                       /* Triangular Kernel */
    for (i = 0, nextx = xo_arg, nexty = yo_arg; i < ns; i++, nextx = cdr(nextx), nexty = cdr(nexty)) {
      temp1 = p[i];
      for (next = x_arg, sum = 0.0; consp(next); next = cdr(next)) {
	temp = (temp1 - value(next)) / width;
	if (-1.0 < temp && temp < 0.0) 
	  sum += (1.0 + temp);
	else if (0.0 <= temp && temp < 1.0)
	  sum += (1.0 - temp);
      }
      rplaca(nextx,cvflonum(temp1));
      rplaca(nexty,cvflonum(sum / (n * width)));
    }
    break;
    
  case 'E':                       /* Epanechnikov Kernel */
    for (i = 0, nextx = xo_arg, nexty = yo_arg; i < ns; i++, nextx = cdr(nextx), nexty = cdr(nexty)) {
      temp1 = p[i];
      for (next = x_arg, sum = 0.0; consp(next); next = cdr(next)) {
	temp = (temp1 - value(next)) / width;
	if (fabs(temp) < ROOT5) 
	  sum += (1.0 - 0.2 * temp * temp);
      }
      rplaca(nextx,cvflonum(temp1));
      rplaca(nexty,cvflonum(.75 * sum / (n * ROOT5 * width)));
    }
    break;

  default:		/* Biweight Kernel */
    for (i = 0, nextx = xo_arg, nexty = yo_arg; i < ns; i++, nextx = cdr(nextx), nexty = cdr(nexty)) {
      temp1 = p[i];
      for (next = x_arg, sum = 0.0; consp(next); next = cdr(next)) {
	temp = (temp1 - value(next)) / width;
	if (fabs(temp) < 1.0) {
	  temp = 1.0 - temp * temp;
	  sum += temp * temp;
	}
      }
      rplaca(nextx,cvflonum(temp1));
      rplaca(nexty,cvflonum(0.9375 * sum /(n * width)));
    }
    break;
    
  } /* switch statement */
  xlpopn(2);
  return cons(xo_arg,cons(yo_arg,NIL));
}
