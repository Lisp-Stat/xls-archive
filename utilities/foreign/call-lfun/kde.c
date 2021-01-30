/*  

This is probably the first useful example. It shows how to use
the keyword arguments to return a kernel density estimate. This
routine accepts two keyword arguments:
  :type denotes the kernel type (one of 'B 'G 'T 'U 'E)
  :width is the kernel window width. 

In fact, this should return the same values as the built in function
kernel-dens in xlispstat. It is a direct hack of kernel.c in the
distribution.

The default kernel is 'B (Bi-weight). 

To use, compile into kde.c and
type
(dyn-load "kde.o")
(def z (coerce (normal-rand 1000) 'vector))
(call-lfun "kde" z)
(plot-lines *)


You can give keyword arguments to call-lfun, for example,
(call-lfun "kde" z :type 'G) 
will use the Gaussian Kernel.
*/ 


#include "xlisp.h"
#include <math.h> 

#define value(x,i) (getflonum(getelement(x,i)))

extern LVAL sk_type, sk_width;

#ifndef ROOT2PI
#define ROOT2PI 2.50662827463100050241
#endif ROOT2PI

#ifndef ROOT5
#define ROOT5   2.23606797749978969641
#endif ROOT5


LVAL kde()
{
  LVAL x_arg, xo_arg, yo_arg, w_arg, t_arg;
  FIXTYPE i, j, n, ns, ktype;
  FLOTYPE xmin, xmax, temp, temp1, width, sum;
  
  x_arg = xlgavector() ; /* the vector of data points */
  n = getsize(x_arg);
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

  xo_arg = newvector(ns);
  yo_arg = newvector(ns);

  /* Find max and min of the data */
  xmax = value(x_arg,0);
  xmin = xmax;

  for (i = 1; i < n; i++) {
    temp = value(x_arg,i);
    if (xmax < temp) xmax = temp;
    if (xmin > temp) xmin = temp;
  }

  /* Set kernel width */
  if (w_arg)
    width = makedouble(w_arg);
  else 
    width = (xmax - xmin) / (1.0 + log((double) n));

  if (width <= 0.0) xlfail("bad window width");

  /* Create an equally spaced grid between xmax and ymax */
  temp = (xmax - xmin) / (ns - 1);
  for (i = 0; i < ns; i++)
    setelement(xo_arg,i,cvflonum(xmin + i * temp)); 
  
  /* Compute the Y values */
  switch (ktype) {
  case 'G':			/* Gaussian Kernel */
    for (i = 0; i < ns; i++) {
      temp1 = value(xo_arg,i);
      for (j = 0, sum = 0.0; j < n; j++) {
	temp = 4.0 * (temp1  - value(x_arg,j)) / width;
	sum += exp(-0.5 * temp * temp);
      }
      setelement(yo_arg,i,cvflonum(sum / (n * 0.25 * width * ROOT2PI)));
    }
    break;
  case 'U':			/* Uniform Kernel */
    for (i = 0; i < ns; i++) {
      temp1 = value(xo_arg,i);
      for (j = 0, sum = 0.0; j < n; j++) {
	temp = 0.75 * (temp1 - value(x_arg,j)) / width;
	sum += (fabs(temp) < 0.5) ? 1.0 : 0.0;
      }
      setelement(yo_arg,i,cvflonum(sum / (n * 1.5 * width)));
    }
    break;
    
  case 'T':                       /* Triangular Kernel */
    for (i = 0; i < ns; i++) {
      temp1 = value(xo_arg,i);
      for (j = 0, sum = 0.0; j < n; j++) {
	temp = (temp1 - value(x_arg,j)) / width;
	if (-1.0 < temp && temp < 0.0) 
	  sum += (1.0 + temp);
	else if (0.0 <= temp && temp < 1.0)
	  sum += (1.0 - temp);
      }
      setelement(yo_arg,i,cvflonum(sum / (n * width)));
    }
    break;
    
  case 'E':                       /* Epanechnikov Kernel */
    for (i = 0; i < ns; i++) {
      temp1 = value(xo_arg,i);
      for (j = 0, sum = 0.0; j < n; j++) {
	temp = (temp1 - value(x_arg,j)) / width;
	if (fabs(temp) < ROOT5) 
	  sum += (1.0 - 0.2 * temp * temp);
      }
      setelement(yo_arg,i,cvflonum(.75 * sum / (n * ROOT5 * width)));
    }
    break;

  default:		/* Biweight Kernel */
    for (i = 0; i < ns; i++) {
      temp1 = value(xo_arg,i);
      for (j = 0, sum = 0.0 ; j < n; j++) {
	temp = (temp1 - value(x_arg,j)) / width;
	if (fabs(temp) < 1.0) {
	  temp = 1.0 - temp * temp;
	  sum += temp * temp;
	}
      }
      setelement(yo_arg,i,cvflonum(0.9375 * sum /(n * width)));
    }
    break;
    
  } /* switch statement */
  xlpopn(2);
  return cons(xo_arg,cons(yo_arg,NIL));
}


