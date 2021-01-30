/*
 Example 10. In this example, we study how mklist works 
 Compile into "example10.o" and type                              
 (dyn-load "example10.o")                                         
 (call-lfun "makelist" 10 20)     
                                                                 
 and you should get (20 20 20 20 20 20 20 20 20 20)                                      
*/

#include "xlisp.h"
#include "myxlisp.h"

LVAL makelist()
{ int i, n;
  
  LVAL len_ptr, val_ptr;

  len_ptr = xlgafixnum(); /* Get the length */
  val_ptr = xlgafixnum(); /* The value to be put in */

  xllastarg();  /* Complain if more arguments are given. */

  return(mklist(getfixnum(len_ptr),val_ptr));
}
