/*
 Example 7. In this example, we show how to use optional args.   
                                                                 
 Compile into "example7.o" and type                              
 (dyn-load "example6.o")                                         
 (call-lfun "myargs" (vector (vector 1 2 3) (vector 7 3))        
                                                                 
 and you should get (3 7)                                        
 If you use                                                      
 (call-lfun "myargs" (vector (vector 1 2 3) (vector 7 3) 1 2)    
                                                                 
 instead, you should get (1 2 3 7)                               
                                                                 
 A maximum of 10 optional args is allowed.                       
*/

#include "xlisp.h"
#include "myxlisp.h"

#define MYLIMIT 10

LVAL myargs()
{ int i, m, n, ourmax, dummy[MYLIMIT];
  
  LVAL vec_ptr, next, result, elem;

  vec_ptr = xlgavector(); /* Get the vector */

  m = 0;
  while (moreargs()) {
    dummy[m] = getfixnum(xlgafixnum());
    m++;
    if (m == MYLIMIT) xlfail("Too many optional args");
  }

  xlstkcheck(3);
  xlsave(result);
  xlsave(next);
  xlsave(elem);

  n = getsize(vec_ptr);

  result=mklist(n+m,NIL);
  
  for (i = 0, next = result; i < m; i++) {
    rplaca(next, cvfixnum(dummy[i]));
    next = cdr(next);
  }

  for (i = 0; i < n; i++) {
    elem = getelement(getelement(vec_ptr,i),0);
    if (! fixp(elem) && ! floatp(result)) xlfail("bad vectors");
    ourmax = getfixnum(elem);
    rplaca(next, cvfixnum(ourmax));
    next = cdr(next);
  }

  xlpopn(3);
  return result;
}
