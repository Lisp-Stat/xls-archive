#include <xlisp.h>
#include <xldmem.h>
#include <kde.h>
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
  FLOTYPE xmin, xmax, temp, obs, width, sum, wt, c;
  
/* 
   Arguments come as:
   data kernel-type xvals width weights 
*/

  x_arg = xlgalist() ; /* the list of data points */
  n = llength(x_arg);

  t_arg= xlgasymbol(); 
  
  ktype = getstring(getpname(t_arg))[0];
  
  xo_arg = xlgalist();   /* The x values */
  ns = llength(xo_arg); /* The number of x values */
  
  if ((width = getflonum(xlgaflonum())) <= 0.0)
    xlfail("bad window width");   /* Kernel window width */

  wt_arg = xlgalist();  /* The list of weights */
  
  /* Save our pointers */
  xlsave1(yo_arg);
  
  yo_arg = mklist(ns,NIL);
  
  /* Compute the Y values */
  switch (ktype) {
  case 'G':			/* Gaussian Kernel */ 
    nexty = yo_arg;
    c = 1.0 / (width * ROOT2PI);
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = (obs  - value(next)) / width;
	sum += value(nextw) * exp(-0.5 * temp * temp);
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(sum * c));
      nexty = cdr(nexty);
    }
    break;
  case 'U':			/* Uniform Kernel */
    nexty = yo_arg;
    c = 1.0 / width;
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = (obs - value(next)) * c;
	sum += (fabs(temp) < 1.0) ? value(nextw) : 0.0;
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(sum * 0.5 * c));
      nexty = cdr(nexty);
    }
    break;
    
  case 'T':                       /* Triangular Kernel */
    nexty = yo_arg;
    c = 1.0 / width;
    for (nextx = xo_arg; consp(nextx); nextx = cdr(nextx)) {
      obs = value(nextx);
      nextw = wt_arg;
      sum = 0.0;
      for (next = x_arg; consp(next); next = cdr(next)) {
	temp = fabs((obs - value(next)) * c);
	sum += (temp < 1.0) ? value(nextw) * (1.0 - temp): 0.0;
	nextw = cdr(nextw);
      }
      rplaca(nexty,cvflonum(sum * c));
      nexty = cdr(nexty);
    }
    break;
    
  case 'E':                       /* Epanechnikov Kernel */
    nexty = yo_arg;
    c = .75 / (ROOT5 * width);
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
      rplaca(nexty,cvflonum(sum * c));
      nexty = cdr(nexty);
    }
    break;

  default:		/* Biweight Kernel */
    nexty = yo_arg;
    c = 0.9375 / width;
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
      rplaca(nexty,cvflonum(sum * c));
      nexty = cdr(nexty);
    }
    break;
    
  } /* switch statement */
  xlpop();
  return yo_arg;
}




