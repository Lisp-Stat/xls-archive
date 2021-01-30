/* -*-C-*-
********************************************************************************
*
* File:         xlpp.c
* RCS:          $Header: xlpp.c,v 1.2 89/11/25 05:42:08 mayer Exp $
* Description:  xlisp pretty printer
* Author:       David Michael Betz
* Created:      
* Modified:     Sat Nov 25 05:42:00 1989 (Niels Mayer) mayer@hplnpm
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
static char rcs_identity[] = "@(#)$Header: xlpp.c,v 1.2 89/11/25 05:42:08 mayer Exp $";


#include "xlisp.h"

/* external variables */
extern LVAL s_stdout, s_stderr;
extern int xlfsize;

/* local variables */
static int pplevel,ppmargin,ppmaxlen;
static LVAL ppfile;

/* xpp - pretty-print an expression */
LVAL xpp()
{
    LVAL expr;

    /* get expression to print and file pointer */
    expr = xlgetarg();
    ppfile = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    xllastarg();

    /* pretty print the expression */
    pplevel = ppmargin = 0; ppmaxlen = 40;
    pp(expr); ppterpri(ppfile);

    /* return nil */
    return (NIL);
}

/* pp - pretty print an expression */
LOCAL pp(expr)
  LVAL expr;
{
    if (consp(expr))
	pplist(expr);
    else
	ppexpr(expr);
}

/* pplist - pretty print a list */
LOCAL pplist(expr)
  LVAL expr;
{
    int n;

    /* if the expression will fit on one line, print it on one */
    if ((n = sexpflatsize(expr)) < ppmaxlen) {
	xlprint(ppfile,expr,TRUE);
	pplevel += n;
    }

    /* otherwise print it on several lines */
    else {
	n = ppmargin;
	ppputc('(');
	if (atom(car(expr))) {
	    ppexpr(car(expr));
	    ppputc(' ');
	    ppmargin = pplevel;
	    expr = cdr(expr);
	}
	else
	    ppmargin = pplevel;
	for (; consp(expr); expr = cdr(expr)) {
	    pp(car(expr));
	    if (consp(cdr(expr)))
		ppterpri();
	}
	if (expr != NIL) {
	    ppputc(' '); ppputc('.'); ppputc(' ');
	    ppexpr(expr);
	}
	ppputc(')');
	ppmargin = n;
    }
}

/* ppexpr - print an expression and update the indent level */
LOCAL ppexpr(expr)
  LVAL expr;
{
    xlprint(ppfile,expr,TRUE);
    pplevel += sexpflatsize(expr);
}

/* ppputc - output a character and update the indent level */
LOCAL ppputc(ch)
  int ch;
{
    xlputc(ppfile,ch);
    pplevel++;
}

/* ppterpri - terminate the print line and indent */
LOCAL ppterpri()
{
    xlterpri(ppfile);
    for (pplevel = 0; pplevel < ppmargin; pplevel++)
	xlputc(ppfile,' ');
}

/* sexpflatsize - compute the flat size of an expression */
/* name change from flatsize to sexpflatsize */ /* Voodoo */
LOCAL int sexpflatsize(expr)	
  LVAL expr;
{
    xlfsize = 0;
    xlprint(NIL,expr,TRUE);
    return (xlfsize);
}
