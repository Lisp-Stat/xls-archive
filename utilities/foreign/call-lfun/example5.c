/*
 Example 5. In this example, we return the length of a list      
                                                                 
 Compile into "example5.o" and type                              
 (dyn-load "example5.o")                                         
 (call-lfun "mylength" (list (list 1 2) (list 1.2 7.8 2))        
                                                                 
 and you should get 2                                
*/

#include "xlisp.h"
#include "myxlisp.h"

LVAL mylength()
{
  LVAL list_ptr, result;

  list_ptr = xlgalist(); /* Get the list */

  xllastarg();  /* Complain if more arguments are given. */
  xlsave1(result);
  result=cvfixnum(llength(list_ptr));
  xlpop();
  return result;
}

