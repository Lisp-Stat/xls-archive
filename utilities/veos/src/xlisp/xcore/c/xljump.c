/* -*-C-*-
********************************************************************************
*
* File:         xljump.c
* RCS:          $Header: xljump.c,v 1.2 89/11/25 05:38:38 mayer Exp $
* Description:  execution context routines
* Author:       David Michael Betz
* Created:      
* Modified:     Sat Nov 25 05:38:31 1989 (Niels Mayer) mayer@hplnpm
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
static char rcs_identity[] = "@(#)$Header: xljump.c,v 1.2 89/11/25 05:38:38 mayer Exp $";


#include "xlisp.h"

/* external variables */
extern CONTEXT *xlcontext,*xltarget;
extern LVAL xlvalue,xlenv,xlfenv,xldenv;
extern int xlmask;

/* xlbegin - beginning of an execution context */
xlbegin(cptr,flags,expr)
  CONTEXT *cptr; int flags; LVAL expr;
{
    cptr->c_flags = flags;
    cptr->c_expr = expr;
    cptr->c_xlstack = xlstack;
    cptr->c_xlenv = xlenv;
    cptr->c_xlfenv = xlfenv;
    cptr->c_xldenv = xldenv;
    cptr->c_xlcontext = xlcontext;
    cptr->c_xlargv = xlargv;
    cptr->c_xlargc = xlargc;
    cptr->c_xlfp = xlfp;
    cptr->c_xlsp = xlsp;
    xlcontext = cptr;
}

/* xlend - end of an execution context */
xlend(cptr)
  CONTEXT *cptr;
{
    xlcontext = cptr->c_xlcontext;
}

/* xlgo - go to a label */
xlgo(label)
  LVAL label;
{
    CONTEXT *cptr;
    LVAL *argv;
    int argc;

    /* find a tagbody context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_GO) {
	    argc = cptr->c_xlargc;
	    argv = cptr->c_xlargv;
	    while (--argc >= 0)
		if (*argv++ == label) {
		    cptr->c_xlargc = argc;
		    cptr->c_xlargv = argv;
		    xljump(cptr,CF_GO,NIL);
		}
	}
    xlfail("no target for GO");
}

/* xlreturn - return from a block */
xlreturn(name,val)
  LVAL name,val;
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_RETURN && cptr->c_expr == name)
	    xljump(cptr,CF_RETURN,val);
    xlfail("no target for RETURN");
}

/* xlthrow - throw to a catch */
xlthrow(tag,val)
  LVAL tag,val;
{
    CONTEXT *cptr;

    /* find a catch context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if ((cptr->c_flags & CF_THROW) && cptr->c_expr == tag)
	    xljump(cptr,CF_THROW,val);
    xlfail("no target for THROW");
}

/* xlsignal - signal an error */
xlsignal(emsg,arg)
  char *emsg; LVAL arg;
{
    CONTEXT *cptr;

    /* find an error catcher */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_ERROR) {
	    if (cptr->c_expr && emsg)
		xlerrprint("error",NULL,emsg,arg);
	    xljump(cptr,CF_ERROR,NIL);
	}
}

/* xltoplevel - go back to the top level */
xltoplevel()
{
    stdputstr("[ back to top level ]\n");
    findandjump(CF_TOPLEVEL,"no top level");
}

/* xlbrklevel - go back to the previous break level */
xlbrklevel()
{
    findandjump(CF_BRKLEVEL,"no previous break level");
}

/* xlcleanup - clean-up after an error */
xlcleanup()
{
    stdputstr("[ back to previous break level ]\n");
    findandjump(CF_CLEANUP,"not in a break loop");
}

/* xlcontinue - continue from an error */
xlcontinue()
{
    findandjump(CF_CONTINUE,"not in a break loop");
}

/* xljump - jump to a saved execution context */
xljump(target,mask,val)
  CONTEXT *target; int mask; LVAL val;
{
    /* unwind the execution stack */
    for (; xlcontext != target; xlcontext = xlcontext->c_xlcontext)

	/* check for an UNWIND-PROTECT */
	if ((xlcontext->c_flags & CF_UNWIND)) {
	    xltarget = target;
	    xlmask = mask;
	    break;
	}
	   
    /* restore the state */
    xlstack = xlcontext->c_xlstack;
    xlenv = xlcontext->c_xlenv;
    xlfenv = xlcontext->c_xlfenv;
    xlunbind(xlcontext->c_xldenv);
    xlargv = xlcontext->c_xlargv;
    xlargc = xlcontext->c_xlargc;
    xlfp = xlcontext->c_xlfp;
    xlsp = xlcontext->c_xlsp;
    xlvalue = val;

    /* call the handler */
    xllongjmp(xlcontext->c_jmpbuf,mask);
}

/* findandjump - find a target context frame and jump to it */
LOCAL findandjump(mask,error)
  int mask; char *error;
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & mask)
	    xljump(cptr,mask,NIL);
    xlabort(error);
}

