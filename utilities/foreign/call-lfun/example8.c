/*
 Example 8. In this example, we show how to use keyword args.    
 From my understanding of things, if one is using a new keyword  
 it must be known to the interpreter, as shown in statinit.c     
 For example, sk_type = xlenter(":TYPE").  In this example, I am 
 going to use :TYPE as my keyword since it has already been      
 entered. Otherwise, I will have to call a routine that enters the
 keyword using xlenter first. 
                                                                 
 Compile into "example8.o" and type                              
 (dyn-load "example8.o")                                         
 (call-lfun "mykey" (list 1 2 3) :type '(4 5 6))                 
                                                                 
 and you should get ((1 2 3) 4 5 6) back.                        
 If you use                                                      
 (call-lfun "mykey" (list 1 2 3)                                 
                                                                 
 instead, you should just get ((1 2 3)) back.                    
*/

#include "xlisp.h"
#include "myxlisp.h"

extern LVAL sk_type;

LVAL mykey()
{ int i, n;
  
  LVAL list_arg, t_arg, next, result;

  list_arg = xlgalist(); /* Get the list */
  
  if (! xlgetkeyarg(sk_type, &t_arg)) t_arg = NIL;
  xllastkey();   /* Last key argument */

  xlsave1(result);

  result = cons(list_arg,t_arg);

  xlpop();
  return result;
}

