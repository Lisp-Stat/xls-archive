/*
 Example 3. This was given to me by Tierney to show one could call an   
 xlisp function from within a C program.                                
                                                                        
 Compile it into a file "example3.o" and type                           
 (dyn-load "example3.o")                                                
 (call-cfun "myfun" ' 2.0 3)                                           
                                                                        
 This should return 6 as the answer. You can use any valid lisp       
 function instead of ' that uses two arguments. So,                    
 (call-lfun "myfun" '/ 3.0 2) should give you 1.5 as answer.            

*/

#include "xlisp.h" /* put the source on the include path to get this */

LVAL myfun()  /* no arguments - pick them up with the stack reading macros */
{
  LVAL args, f, x, y, result;
  double val;

  f = xlgetarg();    /* no argument checking */
  x = xlgaflonum();  /* checks type; signals error if it is not right */
  y = xlgafixnum();  /* again checks arguments */
  xllastarg();       /* This signals an error if there are more       */
                     /* arguments. The macro call moreargs() tests    */
                     /* for more arguments and can be used to make    */
                     /* a function with optional arguments.           */

  xlsave1(args);     /* protect from garbage collect - MUST be mathed */
                     /* by a pop at the end or the stack gets         */
                     /* confused and you bomb.                        */

  /* build up the arguments list - do this once for multiple calls    */
  /* and use rplaca to fill it with new arguments                     */
  args = consa(y);   /* same as cons(y, NIL);                         */
  args = cons(x, args);

  /* apply the function */
  result = xlapply(pushargs(f, args));

  /* check and examine result */
  if (! fixp(result) && ! floatp(result)) xlfail("bad result type");
  val = (floatp(result)) ? getflonum(result) : getflonum(result);

  /* pop the saved entry -- I forgot this in the previous version!! */
  xlpop();

  /* repackage and return the result as a FLONUM */
  return(cvflonum((FLOTYPE) val));
}




