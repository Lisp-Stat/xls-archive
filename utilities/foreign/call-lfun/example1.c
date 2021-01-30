/*
 Example 1. The first example is from Jeff's document itself. He gives 
 an example of how to add a native xlisp function. I have corrected    
 typo(s) in his example.                                               

 To demonstrate the example, make sure you have the relevant includes  
 in the include path and compile the program to get an object, call it 
 "example1.o".                                                         
                                                                       
 Then use it as follows:                                               
 (dyn-load "example1.o")                                               
                                                                       
 (call-lfun "xlsamplefun" '(a b c) 1 2.0)                              
                                                                       
 It should return                                                      
 (((a b c) a b c) 3 6)                                               
                                                                       
*/

/* xlsamplefun - do useless stuff. */
#include "xlisp.h"

LVAL samplefun( list_arg, integer_arg, float_arg )
LVAL            list_arg, integer_arg, float_arg;
{
    FIXTYPE val_of_integer_arg;
    FLOTYPE val_of_float_arg;

    /* Variables which will point to LISP objects: */
    LVAL result;

    LVAL list_ptr;
    LVAL float_ptr;
    LVAL integer_ptr;

    /* Protect our internal pointers by */
    /* pushing them on the evaluation   */
    /* stack so the garbage collector   */
    /* can't recycle them in the middle */
    /* of the routine:                  */
    xlstkcheck(3);    /* Make sure following xlsave */
                      /* calls won't overrun stack. */
    xlsave(list_ptr); /* Use xlsave1() if you don't */
    xlsave(float_ptr);/* do an xlstkcheck().        */
    xlsave(integer_ptr);

    /* Create an internal list structure, protected */
    /* against garbage collection until we exit fn: */
    list_ptr = cons(list_arg,list_arg);

    /* Get the actual values of our fixnum and flonum: */
    val_of_integer_arg = getfixnum( integer_arg );
    val_of_float_arg   = getflonum( float_arg   );


    /*******************************************/
    /* You can have any amount of intermediate */
    /* computations at this point in the fn... */
    /*******************************************/


    /* Make new numeric values to return: */
    integer_ptr = cvfixnum( val_of_integer_arg * 3   );
    float_ptr   = cvflonum( val_of_float_arg   * 3.0 );

    /* Cons it all together to produce a return value: */
    result = cons(
        list_ptr,
        cons(
            integer_ptr,
            cons(
                float_ptr,
                NIL
            )
        )
    );

    /* Restore the stack, cancelling the xlsave()s: */
    xlpopn(3); /* Use xlpop() for a single argument.*/

    return result;
}

LVAL xlsamplefun()
{
    /* Variables to hold the arguments: */
    LVAL    list_arg, integer_arg, float_arg;

    /* Get the arguments, with appropriate errors */
    /* if any are of the wrong type.  Look in     */
    /* xlisp.h for macros to read other types of  */
    /* arguments.  Look in xlmath.c for examples  */
    /* of functions which can handle an argument  */
    /* which may be either int or float:          */
    list_arg    = xlgalist()  ;  /* "XLisp Get A LIST"   */
    integer_arg = xlgafixnum();  /* "XLisp Get A FIXNUM" */
    float_arg   = xlgaflonum();  /* "XLisp Get A FLONUM" */

    /* Issue an error message if there are any extra arguments: */
    xllastarg();

    /* Call a separate C function to do the actual  */
    /* work.  This way, the main function can       */
    /* be called from both xlisp code and C code.   */
    /* By convention, the name of the xlisp wrapper */
    /* starts with "xl", and the native C function  */
    /* has the same name minus the "xl" prefix:     */
    return samplefun( list_arg, integer_arg, float_arg );
}
