#include "xlisp.h"
#include "myxlisp.h"
#include <math.h> 

#define value(x) (getflonum(car(x)))

#ifndef ROOT2PI
#define ROOT2PI 2.50662827463100050241
#endif ROOT2PI

#ifndef ROOT5
#define ROOT5   2.23606797749978969641
#endif ROOT5

LVAL kde()
{
  LVAL x_arg, t_arg, wt_arg;
  LVAL xo_arg, yo_arg;
  LVAL next, nextx, nexty, nextw;
  FIXTYPE i, n, ns, ktype;
  FLOTYPE xmin, xmax, temp, obs, width, sum, wt;
  
/* 
   Arguments come as:
   data kernel-type xvals width weights 
*/

  x_arg = xlgalist() ; /* the list of data points */
  n = llength(x_arg);

  t_arg= xlgasymbol(); 
  
  switch (getstring(getpname(t_arg))[0]) {
  case 'U': ktype = 'U'; break;
  case 'T': ktype = 'T'; break;
  case 'E': ktype = 'E'; break;
  case 'G': ktype = 'G'; break;
  default:  ktype = 'B'; break;
  }
  
  ns = getfixnum(xlgafixnum());  /* The number of x values */
  
  if ((width = getflonum(xlgaflonum())) <= 0.0)
    xlfail("bad window width");   /* Kernel window width */

  wt_arg = xlgalist();  /* The list of weights */
  
  /* Save our pointers */
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

  /* Create an equally spaced grid between xmax and ymax */
  temp = (xmax - xmin) / (ns - 1);
  for (next = xo_arg, i = 0; consp(next); next = cdr(next), i++)
    rplaca(next,cvflonum(xmin + i * temp));
  
  /* Compute the Y values */
  switch (ktype) {
  case 'G':			/* Gaussian Kernel */ 
    nexty = yo_arg;
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = 4.0 * (obs  - value(next)) / width;
	sum += value(nextw) * exp(-0.5 * temp * temp);
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(sum / (0.25 * width * ROOT2PI)));
      nexty = cdr(nexty);
    }
    break;
  case 'U':			/* Uniform Kernel */
    nexty = yo_arg;
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = 0.75 * (obs - value(next)) / width;
	sum += (fabs(temp) < 0.5) ? value(nextw) : 0.0;
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(sum / (1.5 * width)));
      nexty = cdr(nexty);
    }
    break;
    
  case 'T':                       /* Triangular Kernel */
    nexty = yo_arg;
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = (obs - value(next)) / width;
	if (-1.0 < temp && temp < 0.0) 
	  sum += value(nextw) * (1.0 + temp);
	else if (0.0 <= temp && temp < 1.0)
	  sum += value(nextw) * (1.0 - temp);
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(sum / width));
      nexty = cdr(nexty);
    }
    break;
    
  case 'E':                       /* Epanechnikov Kernel */
    nexty = yo_arg;
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = (obs - value(next)) / width;
	if (fabs(temp) < ROOT5) 
	  sum += value(nextw) * (1.0 - 0.2 * temp * temp);
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(.75 * sum / (ROOT5 * width)));
      nexty = cdr(nexty);
    }
    break;

  default:		/* Biweight Kernel */
    nexty = yo_arg;
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = (obs - value(next)) / width;
	if (fabs(temp) < 1.0) {
	  temp = 1.0 - temp * temp;
	  sum += value(nextw) * temp * temp;
	}
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(0.9375 * sum /width));
      nexty = cdr(nexty);
    }
    break;
    
  } /* switch statement */
  xlpopn(2);
  return cons(xo_arg,cons(yo_arg,NIL));
}

