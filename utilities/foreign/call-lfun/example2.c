/*
 Example 2.  This example demonstrates how to create a new vector and  
 return it as a result.                                                
                                                                       
 To use it, compile it into "example2.o" and type                      
 (dyn-load "example2.o")                                               
 (call-lfun "mycreatevec" 10)                                          
                                                                       
 It should return #(0 2 4 6 8 10 12 14 16 18)                          

*/

/* This example creates a new vector, puts some values in it,
and returns the vector */

#include "xlisp.h"

LVAL mycreatevec()
{
  int i,n;
  LVAL vec_len_ptr, vec_ptr, int_ptr;

  vec_len_ptr = xlgafixnum(); /* Get the length of the desired vector */

  xllastarg();  /* Complain if more arguments are given. */

  xlstkcheck(2);
  xlsave(vec_ptr);   /* I am not sure which ptrs need to be saved from gc, 
			but I'll be safe anyway.... */
  xlsave(int_ptr);

  n = getfixnum(vec_len_ptr);
  vec_ptr = newvector(n);  
	/* This function creates a new vector of the desired length
	Note the use of getfixnum to get the length value */

  for (i = 0; i < n; i++){
	int_ptr = cvfixnum(i*2);
	setelement(vec_ptr,i,int_ptr); /* Set the element */
  }
  xlpopn(2);
  return vec_ptr;	
}
