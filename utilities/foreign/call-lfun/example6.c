/*
 Example 6. In this example, we take a vector of vectors and     
 return a list of the first elements of each.                               
                                                                 
 Compile into "example6.o" and type                              
 (dyn-load "example6.o")                                         
 (call-lfun "vecelems" (vector (vector 1 2 3) (vector 7 3))     
                                                                 
 and you should get (1 7)                                      

*/

#include "xlisp.h"
#include "myxlisp.h"

LVAL vecelems()
{ int i, n, ourval;
  
  LVAL vec_ptr, next, result, elem;

  vec_ptr = xlgavector(); /* Get the vector */

  xllastarg();  /* Complain if more arguments are given. */

  xlstkcheck(3);
  xlsave(result);
  xlsave(next);
  xlsave(elem);

  n = getsize(vec_ptr);

  result=mklist(n,NIL);
  next=result;

  for (i = 0; i < n; i++) {
    elem = getelement(getelement(vec_ptr,i),0);
    if (! fixp(elem) && ! floatp(result)) xlfail("bad vectors");
    ourval = getfixnum(elem);
    
    rplaca(next, cvfixnum(ourval));
    next = cdr(next);
  }

  xlpopn(3);
  return result;
}
