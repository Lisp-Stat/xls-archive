/*
 Example 4. This example is similar to example 2 except that it  
 creates a list and shows how to traverse a list.                
 Sometimes, when you use functions not defined in xlisp.h, the   
 compilation might bomb, as in this example, since mklist is not 
 defined in xlisp.h.  However, a little bit of browsing shows    
 that the definition should be extern LVAL mklist().  It is best 
 to put such stuff in a header file, say "myxlisp.h".            
                                                                 
 Compile into "example4.o" and type                              
 (dyn-load "example4.o")                                         
 (call-lfun "mycreatelist" 10)                                   
                                                                 
 and you should get (0 2 4 6 8 10 12 14 16 18)                   
*/

#include "xlisp.h"
#include "myxlisp.h"

LVAL mycreatelist()
{
  int i,n;
  LVAL list_ptr, len_ptr, next;

  len_ptr = xlgafixnum(); /* Get the length of the desired list */

  xllastarg();  /* Complain if more arguments are given. */

  xlsave1(list_ptr);  /* We only have one thing to save */

  n = getfixnum(len_ptr);

  list_ptr = mklist(n, NIL);  
	/* This function creates a new list of length n, initialized to
           nil */

  i = 0;
  for (next = list_ptr; consp(next); next = cdr(next), i++)
        rplaca(next, cvfixnum((FIXTYPE) i*2));
                    /* Also shows how to traverse a list */
  xlpop();
  return list_ptr;	
}
