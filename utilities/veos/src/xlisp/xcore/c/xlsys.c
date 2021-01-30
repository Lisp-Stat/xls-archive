/* -*-C-*-
********************************************************************************
*
* File:         xlsys.c
* RCS:          $Header: xlsys.c,v 1.5 89/11/25 05:49:55 mayer Exp $
* Description:  xlisp builtin system functions
* Author:       David Michael Betz
* Created:      
* Modified:     Sat Nov 25 05:49:49 1989 (Niels Mayer) mayer@hplnpm
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
static char rcs_identity[] = "@(#)$Header: xlsys.c,v 1.5 89/11/25 05:49:55 mayer Exp $";


#include "xlisp.h"

/* external variables */
extern jmp_buf top_level;
extern FILE *tfp;

/* external symbols */
extern LVAL a_subr,a_fsubr,a_cons,a_symbol;
extern LVAL a_fixnum,a_flonum,a_string,a_object,a_stream;
extern LVAL a_vector,a_closure,a_char,a_ustream;
extern LVAL k_verbose,k_print;
extern LVAL true;


/* external routines */
extern FILE *osaopen();

/* Include hybrid-class functions: *//* JSP */
#define MODULE_XLSYS_C_GLOBALS
#include "../../xmodules.h"
#undef MODULE_XLSYS_C_GLOBALS


/* xload - read and evaluate expressions from a file */
LVAL xload()
{
    unsigned char *name;
    int vflag,pflag;
    LVAL arg;

    /* get the file name */
    name = getstring(xlgetfname());

    /* get the :verbose flag */
    if (xlgetkeyarg(k_verbose,&arg))
	vflag = (arg != NIL);
    else
	vflag = TRUE;

    /* get the :print flag */
    if (xlgetkeyarg(k_print,&arg))
	pflag = (arg != NIL);
    else
	pflag = FALSE;

    /* load the file */
    return (xlload(name,vflag,pflag) ? true : NIL);
}

/* xtranscript - open or close a transcript file */
LVAL xtranscript()
{
    unsigned char *name;

    /* get the transcript file name */
    name = (moreargs() ? getstring(xlgetfname()) : NULL);
    xllastarg();

    /* close the current transcript */
    if (tfp) osclose(tfp);

    /* open the new transcript */
    tfp = (name ? osaopen(name,"w") : NULL);

    /* return T if a transcript is open, NIL otherwise */
    return (tfp ? true : NIL);
}

/* xtype - return type of a thing */
LVAL xtype()
{
    LVAL arg;

    if (!(arg = xlgetarg()))
	return (NIL);

    switch (ntype(arg)) {
    case SUBR:		return (a_subr);
    case FSUBR:		return (a_fsubr);
    case CONS:		return (a_cons);
    case SYMBOL:	return (a_symbol);
    case FIXNUM:	return (a_fixnum);
    case FLONUM:	return (a_flonum);
    case STRING:	return (a_string);
    case OBJECT:	return (a_object);
    case STREAM:	return (a_stream);
    case VECTOR:	return (a_vector);
    case CLOSURE:	return (a_closure);
    case CHAR:		return (a_char);
    case USTREAM:	return (a_ustream);
    case STRUCT:	return (getelement(arg,0));
/* Include hybrid-class functions: *//* JSP */
#define MODULE_XLSYS_C_XTYPE
#include "../../xmodules.h"
#undef MODULE_XLSYS_C_XTYPE
    default:		xlfail("bad node type");
    }
}

/* xbaktrace - print the trace back stack */
LVAL xbaktrace()
{
    LVAL num;
    int n;

    if (moreargs()) {
	num = xlgafixnum();
	n = getfixnum(num);
    }
    else
	n = -1;
    xllastarg();
    xlbaktrace(n);
    return (NIL);
}

/* xexit - get out of xlisp */
LVAL xexit()
{
    xllastarg();
    wrapup();
}

/* xpeek - peek at a location in memory */
LVAL xpeek()
{
    LVAL num;
    int *adr;

    /* get the address */
    num = xlgafixnum(); adr = (int *)getfixnum(num);
    xllastarg();

    /* return the value at that address */
    return (cvfixnum((FIXTYPE)*adr));
}

/* xpoke - poke a value into memory */
LVAL xpoke()
{
    LVAL val;
    int *adr;

    /* get the address and the new value */
    val = xlgafixnum(); adr = (int *)getfixnum(val);
    val = xlgafixnum();
    xllastarg();

    /* store the new value */
    *adr = (int)getfixnum(val);

    /* return the new value */
    return (val);
}

/* xaddrs - get the address of an XLISP node */
LVAL xaddrs()
{
    LVAL val;

    /* get the node */
    val = xlgetarg();
    xllastarg();

    /* return the address of the node */
    return (cvfixnum((FIXTYPE)val));
}

