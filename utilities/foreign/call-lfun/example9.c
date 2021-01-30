/*
Example 9. This example shows how to create your own keywords.
The routine createkeyword must be called before the routine
testkeyword.

Here is how it works.

> (call-lfun "createkeyword")
T

> (call-lfun "testkeyword" '(1 2 3))
You did not give a keyword argument
NIL

> (call-lfun "testkeyword" '(1 2 3) :junk-key-word 3)
You gave a keyword argument 3
T

*/


#include "xlisp.h"

extern LVAL s_true;  /* for returning true or false */
LVAL junk_keyword;

LVAL createkeyword()
{
  junk_keyword = xlenter(":JUNK-KEY-WORD");
  return s_true;
}
   

LVAL testkeyword()
{
  LVAL list_ptr, junk_arg;

  list_ptr = xlgalist() ; /* the list of data points */

  if (! xlgetkeyarg(junk_keyword, &junk_arg)) junk_arg = NIL;

  if (junk_arg){
    printf ("You gave a keyword argument %d\n", getfixnum(junk_arg));
    return s_true;
  }
  else {
    printf ("You did not give a keyword argument\n");
    return NIL;
  }
}

