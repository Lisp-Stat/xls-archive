/* -*-C-*-
********************************************************************************
*
* File:         xlsubr.c
* RCS:          $Header: xlsubr.c,v 1.2 89/11/25 05:48:29 mayer Exp $
* Description:  xlisp builtin function support routines
* Author:       David Michael Betz
* Created:      
* Modified:     Sat Nov 25 05:48:21 1989 (Niels Mayer) mayer@hplnpm
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
static char rcs_identity[] = "@(#)$Header: xlsubr.c,v 1.2 89/11/25 05:48:29 mayer Exp $";


#include "xlisp.h"

/* external variables */
extern LVAL k_test,k_tnot,s_eql;

/* xlsubr - define a builtin function */
LVAL xlsubr(sname,type,fcn,offset)
  char *sname; int type; LVAL (*fcn)(); int offset;
{
    LVAL sym;
    sym = xlenter(sname);
    setfunction(sym,cvsubr(fcn,type,offset));
    return (sym);
}

/* xlgetkeyarg - get a keyword argument */
int xlgetkeyarg(key,pval)
  LVAL key,*pval;
{
    LVAL *argv;
    int argc;

    for (argv = xlargv, argc = xlargc; (argc -= 2) >= 0; argv += 2) {
	if (*argv == key) {
	    *pval = *++argv;
	    return (TRUE);
	}
    }
    return (FALSE);
}

/* xlgkfixnum - get a fixnum keyword argument */
int xlgkfixnum(key,pval)
  LVAL key,*pval;
{
    if (xlgetkeyarg(key,pval)) {
	if (!fixp(*pval))
	    xlbadtype(*pval);
	return (TRUE);
    }
    return (FALSE);
}

/* xltest - get the :test or :test-not keyword argument */
xltest(pfcn,ptresult)
  LVAL *pfcn; int *ptresult;
{
    if (xlgetkeyarg(k_test,pfcn))	/* :test */
	*ptresult = TRUE;
    else if (xlgetkeyarg(k_tnot,pfcn))	/* :test-not */
	*ptresult = FALSE;
    else {
	*pfcn = getfunction(s_eql);
	*ptresult = TRUE;
    }
}

/* xlgetfile - get a file or stream */
LVAL xlgetfile()
{
    LVAL arg;

    /* get a file or stream (cons) or nil */
    if (arg = xlgetarg()) {
	if (streamp(arg)) {
	    if (getfile(arg) == NULL)
		xlfail("file not open");
	}
	else if (!ustreamp(arg))
	    xlerror("bad argument type",arg);
    }
    return (arg);
}

/* xlgetfname - get a filename */
LVAL xlgetfname()
{
    LVAL name;

    /* get the next argument */
    name = xlgetarg();

    /* get the filename string */
    if (symbolp(name))
	name = getpname(name);
    else if (!stringp(name))
	xlerror("bad argument type",name);

    /* return the name */
    return (name);
}

/* needsextension - check if a filename needs an extension */
int needsextension(name)
  char *name;
{
    char *p;

    /* check for an extension */
    for (p = &name[strlen(name)]; --p >= &name[0]; )
	if (*p == '.')
	    return (FALSE);
	else if (!islower(*p) && !isupper(*p) && !isdigit(*p))
	    return (TRUE);

    /* no extension found */
    return (TRUE);
}

/* xlbadtype - report a "bad argument type" error */
LVAL xlbadtype(arg)
  LVAL arg;
{
    xlerror("bad argument type",arg);
}

/* xlbadinit - report a "bad initializer list" error */
LVAL xlbadinit(arg)
  LVAL arg;
{
    xlerror("bad initializer list",arg);
}

/* xltoofew - report a "too few arguments" error */
LVAL xltoofew()
{
    xlfail("too few arguments");
}

/* xltoomany - report a "too many arguments" error */
xltoomany()
{

    xlfail("too many arguments");
}

/* eq - internal eq function */
int eq(arg1,arg2)
  LVAL arg1,arg2;
{
    return (arg1 == arg2);
}

/* eql - internal eql function */
int eql(arg1,arg2)
  LVAL arg1,arg2;
{
    /* compare the arguments */
    if (arg1 == arg2)
	return (TRUE);
    else if (arg1) {
	switch (ntype(arg1)) {
	case FIXNUM:
	    return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
	case FLONUM:
	    return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
	default:
	    return (FALSE);
	}
    }
    else
	return (FALSE);
}

/* equal - internal equal function */
int equal(arg1,arg2)
  LVAL arg1,arg2;
{
    /* compare the arguments */
    if (arg1 == arg2)
	return (TRUE);
    else if (arg1) {
	switch (ntype(arg1)) {
	case FIXNUM:
	    return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
	case FLONUM:
	    return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
	case STRING:
	    return (stringp(arg2) ? strcmp(getstring(arg1),
					   getstring(arg2)) == 0 : FALSE);
	case CONS:
	    return (consp(arg2) ? equal(car(arg1),car(arg2))
			       && equal(cdr(arg1),cdr(arg2)) : FALSE);
/* awm */
	case VECTOR:
	    if( vectorp( arg2) && (getsz( arg1) == getsz( arg2))) {
	      int i;
	      for( i = 0; i < getsz( arg1); i++) {
		if( !equal( getelement( arg1, i), getelement( arg2, i)))
		  return FALSE;
	      }
	      return TRUE;
	    }
	    else {
	      return FALSE;
	    }
/* awm */
	default:
	    return (FALSE);
	}
    }
    else
	return (FALSE);
}
