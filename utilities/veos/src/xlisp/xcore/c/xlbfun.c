/* -*-C-*-
********************************************************************************
*
* File:         xlbfun.c
* RCS:          $Header: xlbfun.c,v 1.2 89/11/25 05:13:34 mayer Exp $
* Description:  xlisp basic built-in functions
* Author:       David Michael Betz
* Created:      
* Modified:     Sat Nov 25 05:13:11 1989 (Niels Mayer) mayer@hplnpm
* Language:     C
* Package:      N/A
* Status:       X11r4 contrib tape release
*
* WINTERP 1.0 Copyright 1989 Hewlett-Packard Company (by Niels Mayer).
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Hewlett-Packard and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Hewlett-Packard and David Betz
* make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* HEWLETT-PACKARD AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL HEWLETT-PACKARD NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
* See ./winterp/COPYRIGHT for information on contacting the authors.
* 
* Please send modifications, improvements and bugfixes to mayer@hplabs.hp.com
* Post XLISP-specific questions/information to the newsgroup comp.lang.lisp.x
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: xlbfun.c,v 1.2 89/11/25 05:13:34 mayer Exp $";

#include "xlisp.h"

/* external variables */
extern LVAL xlenv,xlfenv,xldenv,true;
extern LVAL s_evalhook,s_applyhook;
extern LVAL s_car,s_cdr,s_nth,s_get,s_svalue,s_splist,s_aref;
extern LVAL s_lambda,s_macro;
extern LVAL s_comma,s_comat;
extern LVAL s_unbound;
extern char gsprefix[];
extern int gsnumber;

/* external routines */
extern LVAL xlxeval();

/* forward declarations */
FORWARD LVAL bquote1();
FORWARD LVAL defun();
FORWARD LVAL makesymbol();

/* xeval - the built-in function 'eval' */
LVAL xeval()
{
    LVAL expr;

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    /* evaluate the expression */
    return (xleval(expr));
}

/* xapply - the built-in function 'apply' */
LVAL xapply()
{
    LVAL fun,arglist;

    /* get the function and argument list */
    fun = xlgetarg();
    arglist = xlgalist();
    xllastarg();

    /* apply the function to the arguments */
    return (xlapply(pushargs(fun,arglist)));
}

/* xfuncall - the built-in function 'funcall' */
LVAL xfuncall()
{
    LVAL *newfp;
    int argc;
    
    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(xlgetarg());
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; moreargs(); ++argc)
	pusharg(nextarg());

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* apply the function to the arguments */
    return (xlapply(argc));
}

/* xmacroexpand - expand a macro call repeatedly */
LVAL xmacroexpand()
{
    LVAL form;
    form = xlgetarg();
    xllastarg();
    return (xlexpandmacros(form));
}

/* x1macroexpand - expand a macro call */
LVAL x1macroexpand()
{
    LVAL form,fun,args;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(args);

    /* get the form */
    form = xlgetarg();
    xllastarg();

    /* expand until the form isn't a macro call */
    if (consp(form)) {
	fun = car(form);		/* get the macro name */
	args = cdr(form);		/* get the arguments */
	if (symbolp(fun) && fboundp(fun)) {
	    fun = xlgetfunction(fun);	/* get the expansion function */
	    macroexpand(fun,args,&form);
	}
    }

    /* restore the stack and return the expansion */
    xlpopn(2);
    return (form);
}

/* xatom - is this an atom? */
LVAL xatom()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (atom(arg) ? true : NIL);
}

/* xsymbolp - is this an symbol? */
LVAL xsymbolp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (arg == NIL || symbolp(arg) ? true : NIL);
}

/* xnumberp - is this a number? */
LVAL xnumberp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (fixp(arg) || floatp(arg) ? true : NIL);
}

/* xintegerp - is this an integer? */
LVAL xintegerp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (fixp(arg) ? true : NIL);
}

/* xfloatp - is this a float? */
LVAL xfloatp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (floatp(arg) ? true : NIL);
}

/* xcharp - is this a character? */
LVAL xcharp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (charp(arg) ? true : NIL);
}

/* xstringp - is this a string? */
LVAL xstringp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (stringp(arg) ? true : NIL);
}

/* xarrayp - is this an array? */
LVAL xarrayp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (vectorp(arg) ? true : NIL);
}

/* xstreamp - is this a stream? */
LVAL xstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (streamp(arg) || ustreamp(arg) ? true : NIL);
}

/* xobjectp - is this an object? */
LVAL xobjectp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (objectp(arg) ? true : NIL);
}

/* xboundp - is there a value bound to this symbol? */
LVAL xboundp()
{
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (boundp(sym) ? true : NIL);
}

/* xfboundp - is there a functional value bound to this symbol? */
LVAL xfboundp()
{
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (fboundp(sym) ? true : NIL);
}

/* xnull - is this null? */
LVAL xnull()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (null(arg) ? true : NIL);
}

/* xlistp - is this a list? */
LVAL xlistp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (listp(arg) ? true : NIL);
}

/* xendp - is this the end of a list? */
LVAL xendp()
{
    LVAL arg;
    arg = xlgalist();
    xllastarg();
    return (null(arg) ? true : NIL);
}

/* xconsp - is this a cons? */
LVAL xconsp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (consp(arg) ? true : NIL);
}

/* xeq - are these equal? */
LVAL xeq()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (arg1 == arg2 ? true : NIL);
}

/* xeql - are these equal? */
LVAL xeql()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (eql(arg1,arg2) ? true : NIL);
}

/* xequal - are these equal? (recursive) */
LVAL xequal()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (equal(arg1,arg2) ? true : NIL);
}

/* xset - built-in function set */
LVAL xset()
{
    LVAL sym,val;

    /* get the symbol and new value */
    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    /* assign the symbol the value of argument 2 */
    setvalue(sym,val);

    /* return the result value */
    return (val);
}

/* xgensym - generate a symbol */
LVAL xgensym()
{
    char sym[STRMAX+11]; /* enough space for prefix and number */
    LVAL x;

    /* get the prefix or number */
    if (moreargs()) {
	x = xlgetarg();
	switch (null(x) ? CONS : ntype(x)) {
	case SYMBOL:
		x = getpname(x);
                /*** FALL INTO STRING ***/
	case STRING:
		strncpy(gsprefix,getstring(x),STRMAX);
		gsprefix[STRMAX] = '\0';
		break;
	case FIXNUM:
		gsnumber = getfixnum(x);
		break;
	default:
		xlerror("bad argument type",x);
	}
    }
    xllastarg();

    /* create the pname of the new symbol */
    sprintf(sym,"%s%d",gsprefix,gsnumber++);

    /* make a symbol with this print name */
    return (xlmakesym(sym));
}

/* xmakesymbol - make a new uninterned symbol */
LVAL xmakesymbol()
{
    LVAL pname;

    /* get the print name of the symbol */
    pname = xlgastring();
    xllastarg();

    /* make the symbol */
    return xlmakesym(getstring(pname));
}

/* xintern - make a new interned symbol */
LVAL xintern()
{
    LVAL pname;

    /* get the print name of the symbol to intern */
    pname = xlgastring();
    xllastarg();

    /* make the symbol */
    return xlenter(getstring(pname));
}

/* xsymname - get the print name of a symbol */
LVAL xsymname()
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* return the print name */
    return (getpname(sym));
}

/* xsymvalue - get the value of a symbol */
LVAL xsymvalue()
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* get the global value */
    while ((val = getvalue(sym)) == s_unbound)
	xlunbound(sym);

    /* return its value */
    return (val);
}

/* xsymfunction - get the functional value of a symbol */
LVAL xsymfunction()
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* get the global value */
    while ((val = getfunction(sym)) == s_unbound)
	xlfunbound(sym);

    /* return its value */
    return (val);
}

/* xsymplist - get the property list of a symbol */
LVAL xsymplist()
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* return the property list */
    return (getplist(sym));
}

/* xget - get the value of a property */
LVAL xget()
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgasymbol();
    xllastarg();

    /* retrieve the property value */
    return (xlgetprop(sym,prp));
}

/* xputprop - set the value of a property */
LVAL xputprop()
{
    LVAL sym,val,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    val = xlgetarg();
    prp = xlgasymbol();
    xllastarg();

    /* set the property value */
    xlputprop(sym,val,prp);

    /* return the value */
    return (val);
}

/* xremprop - remove a property value from a property list */
LVAL xremprop()
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgasymbol();
    xllastarg();

    /* remove the property */
    xlremprop(sym,prp);

    /* return nil */
    return (NIL);
}

/* xhash - compute the hash value of a string or symbol */
LVAL xhash()
{
    unsigned char *str;
    LVAL len,val;
    int n;

    /* get the string and the table length */
    val = xlgetarg();
    len = xlgafixnum(); n = (int)getfixnum(len);
    xllastarg();

    /* get the string */
    if (symbolp(val))
	str = getstring(getpname(val));
    else if (stringp(val))
	str = getstring(val);
    else
	xlerror("bad argument type",val);

    /* return the hash index */
    return (cvfixnum((FIXTYPE)hash(str,n)));
}

/* xaref - array reference function */
LVAL xaref()
{
    LVAL array,index;
    int i;

    /* get the array and the index */
    array = xlgavector();
    index = xlgafixnum(); i = (int)getfixnum(index);
    xllastarg();

    /* range check the index */
    if (i < 0 || i >= getsz(array))
	xlerror("array index out of bounds",index);

    /* return the array element */
    return (getelement(array,i));
}

/* xmkarray - make a new array */
LVAL xmkarray()
{
    LVAL size;
    int n;

    /* get the size of the array */
    size = xlgafixnum() ; n = (int)getfixnum(size);
    xllastarg();

    /* create the array */
    return (newvector(n));
}

/* xvector - make a vector */
LVAL xvector()
{
    LVAL val;
    int i;

    /* make the vector */
    val = newvector(xlargc);

    /* store each argument */
    for (i = 0; moreargs(); ++i)
	setelement(val,i,nextarg());
    xllastarg();

    /* return the vector */
    return (val);
}

/******************************************************************************
 * (copy-array <src> <dest> [<pos>]) --> returns <dest>
 * This function copies from array <src> into the preallocated array <dest>
 * (allocate with 'make-array'). If the optional arg <pos> is given, then
 * elements from <src> will be written into <dest> at index <pos>, otherwise
 * <pos> defaults to 0. 
 *
 * This function was added to xlisp by Niels Mayer.
 ******************************************************************************/
LVAL Prim_COPY_ARRAY()
{
  register int size;
  register LVAL *src, *dest;
  LVAL src_array, dest_array, lval_pos;

  src_array = xlgavector();	/* get <src> */
  dest_array = xlgavector();	/* get <dest> */
  if moreargs()
    lval_pos = xlgafixnum();	/* get optional <pos> */
  else
    lval_pos = NIL;
  xllastarg();

  src = src_array->n_vdata;
  dest = dest_array->n_vdata;

  if (getsz(src_array) < getsz(dest_array))	/* which is shortest? */
    size = getsz(src_array);
  else
    size = getsz(dest_array);

  if (lval_pos) {
    int pos = getfixnum(lval_pos);
    int len = getsz(dest_array) - pos;
    if ((len <= 0) || (pos < 0))
      xlerror("Array position out of bounds.", lval_pos);    
    if (len < size)
      size = len;
    dest = dest + pos;
  }

  while (size--)
    *dest++ = *src++;

  return (dest_array);
}

/******************************************************************************
 * (array-insert-pos <array> <pos> <elt>) --> returns the new <array>
 * inserts <elt> at index <pos> in <array>. if <pos> < 0, then <elt> is
 * appended to the end of <array>.
 *
 * This function was added to xlisp by Niels Mayer.
 ******************************************************************************/
LVAL Prim_ARRAY_INSERT_POS()
{
  register int i;
  register LVAL *src, *dest;
  LVAL src_array, dest_array, elt, lval_position;
  int src_size, position;

  src_array = xlgavector();	/* get <array> */
  lval_position = xlgafixnum();	/* get <pos>, a fixnum */
  elt = nextarg();		/* get <elt>, which can be any lisp type */
  xllastarg();

  src_size = getsz(src_array);
  position = getfixnum(lval_position);
  if (position >= src_size)
    xlerror("Array insertion position out of bounds.", lval_position);
  dest_array = newvector(src_size + 1);

  src = src_array->n_vdata;
  dest = dest_array->n_vdata;

  if (position < 0) {		/* append <elt> to end of array */
    i = src_size;
    while (i--)
      *dest++ = *src++;
    *dest = elt;
  }
  else {			/* insert <elt> at <position> */
    i = position;
    while (i--)
      *dest++ = *src++;
    *dest++ = elt;
    i = src_size - position;
    while (i--)
      *dest++ = *src++;
  }
  return (dest_array);
}

/******************************************************************************
 * (array-delete-pos <array> <pos>) --> returns the new <array>
 * deletes the element at index <pos> in <array>. If <pos>==-1, then it
 * will delete the last element in the array. 
 * Note that this function is destructive. It reuses the old <array>'s
 * elements.
 *
 * This function was added to xlisp by Niels Mayer.
 ******************************************************************************/
LVAL Prim_ARRAY_DELETE_POS()
{
  register int i;
  register LVAL *src, *dest;
  LVAL src_array, dest_array, lval_position;
  int src_size, position;

  src_array = xlgavector();	/* get <array> */
  lval_position = xlgafixnum();	/* get <pos>, a fixnum */
  xllastarg();

  src_size = getsz(src_array);
  position = getfixnum(lval_position);
  if (position >= src_size)
    xlerror("Array insertion position out of bounds.", lval_position);
  if ((src_size - 1) > 0)
    dest_array = newvector(src_size - 1);
  else
    return (NIL);

  src = src_array->n_vdata;
  dest = dest_array->n_vdata;

  if (position < 0) {		/* remove last element of array */
    i = src_size - 1;
    while (i--)
      *dest++ = *src++;
  }
  else {			/* remove <elt> at <position> */
    i = position;
    while (i--)
      *dest++ = *src++;
    src++;			/* don't copy the deleted elt */
    i = src_size - (position + 1);
    while (i--)
      *dest++ = *src++;
  }
  return (dest_array);
}

/* xerror - special form 'error' */
LVAL xerror()
{
    LVAL emsg,arg;

    /* get the error message and the argument */
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    xlerror(getstring(emsg),arg);
}

/* xcerror - special form 'cerror' */
LVAL xcerror()
{
    LVAL cmsg,emsg,arg;

    /* get the correction message, the error message, and the argument */
    cmsg = xlgastring();
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    xlcerror(getstring(cmsg),getstring(emsg),arg);

    /* return nil */
    return (NIL);
}

/* xbreak - special form 'break' */
LVAL xbreak()
{
    LVAL emsg,arg;

    /* get the error message */
    emsg = (moreargs() ? xlgastring() : NIL);
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* enter the break loop */
    xlbreak((emsg ? getstring(emsg) : (unsigned char *)"**BREAK**"),arg);

    /* return nil */
    return (NIL);
}

/* xcleanup - special form 'clean-up' */
LVAL xcleanup()
{
    xllastarg();
    xlcleanup();
}

/* xtoplevel - special form 'top-level' */
LVAL xtoplevel()
{
    xllastarg();
    xltoplevel();
}

/* xcontinue - special form 'continue' */
LVAL xcontinue()
{
    xllastarg();
    xlcontinue();
}

/* xevalhook - eval hook function */
LVAL xevalhook()
{
    LVAL expr,newehook,newahook,newenv,oldenv,oldfenv,olddenv,val;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(newenv);

    /* get the expression, the new hook functions and the environment */
    expr = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    newenv = (moreargs() ? xlgalist() : NIL);
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* establish the environment for the hook function */
    if (newenv) {
	oldenv = xlenv;
	oldfenv = xlfenv;
	xlenv = car(newenv);
	xlfenv = cdr(newenv);
    }

    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr);

    /* restore the old environment */
    xlunbind(olddenv);
    if (newenv) {
	xlenv = oldenv;
	xlfenv = oldfenv;
    }

    /* restore the stack */
    xlpopn(3);

    /* return the result */
    return (val);
}

