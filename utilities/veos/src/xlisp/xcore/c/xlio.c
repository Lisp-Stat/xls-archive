/* -*-C-*-
********************************************************************************
*
* File:         xlio.c
* RCS:          $Header: xlio.c,v 1.2 89/11/25 05:33:04 mayer Exp $
* Description:  xlisp i/o routines
* Author:       David Michael Betz
* Created:      
* Modified:     Sat Nov 25 05:32:45 1989 (Niels Mayer) mayer@hplnpm
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
static char rcs_identity[] = "@(#)$Header: xlio.c,v 1.2 89/11/25 05:33:04 mayer Exp $";

#include "xlisp.h"

/* external variables */
extern LVAL s_stdin,s_stdout,s_stderr,s_debugio,s_traceout,s_unbound;
extern int xlfsize;

/* xlgetc - get a character from a file or stream */
int xlgetc(fptr)
  LVAL fptr;
{
    LVAL lptr,cptr;
    FILE *fp;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
	if ((lptr = gethead(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
		xlfail("bad stream");
	    sethead(fptr,lptr = cdr(lptr));
	    if (lptr == NIL)
		settail(fptr,NIL);
	    ch = getchcode(cptr);
	}
    }

    /* otherwise, check for a buffered character */
    else if (ch = getsavech(fptr))
	setsavech(fptr,'\0');

    /* otherwise, check for terminal input or file input */
    else {
	fp = getfile(fptr);
	if (fp == stdin || fp == stderr)
	    ch = ostgetc();
	else
	    ch = osagetc(fp);
    }

    /* return the character */
    return (ch);
}

/* xlungetc - unget a character */
xlungetc(fptr,ch)
  LVAL fptr; int ch;
{
    LVAL lptr;
    
    /* check for ungetc from nil */
    if (fptr == NIL)
	;
	
    /* otherwise, check for ungetc to a stream */
    if (ustreamp(fptr)) {
	if (ch != EOF) {
	    lptr = cons(cvchar(ch),gethead(fptr));
	    if (gethead(fptr) == NIL)
		settail(fptr,lptr);
	    sethead(fptr,lptr);
	}
    }
    
    /* otherwise, it must be a file */
    else
	setsavech(fptr,ch);
}

/* xlpeek - peek at a character from a file or stream */
int xlpeek(fptr)
  LVAL fptr;
{
    LVAL lptr,cptr;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
	if ((lptr = gethead(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
		xlfail("bad stream");
	    ch = getchcode(cptr);
	}
    }

    /* otherwise, get the next file character and save it */
    else {
	ch = xlgetc(fptr);
	setsavech(fptr,ch);
    }

    /* return the character */
    return (ch);
}

/* xlputc - put a character to a file or stream */
xlputc(fptr,ch)
  LVAL fptr; int ch;
{
    LVAL lptr;
    FILE *fp;

    /* count the character */
    ++xlfsize;

    /* check for output to nil */
    if (fptr == NIL)
	;

    /* otherwise, check for output to an unnamed stream */
    else if (ustreamp(fptr)) {
	lptr = consa(cvchar(ch));
	if (gettail(fptr))
	    rplacd(gettail(fptr),lptr);
	else
	    sethead(fptr,lptr);
	settail(fptr,lptr);
    }

    /* otherwise, check for terminal output or file output */
    else {
	fp = getfile(fptr);
	if (fp == stdout || fp == stderr)
	    ostputc(ch);
	else
	    osaputc(ch,fp);
    }
}

/* xlflush - flush the input buffer */
int xlflush()
{
    osflush();
}

/* stdprint - print to *standard-output* */
stdprint(expr)
  LVAL expr;
{
    xlprint(getvalue(s_stdout),expr,TRUE);
    xlterpri(getvalue(s_stdout));
}

/* stdputstr - print a string to *standard-output* */
stdputstr(str)
  char *str;
{
    xlputstr(getvalue(s_stdout),str);
}

/* errprint - print to *error-output* */
errprint(expr)
  LVAL expr;
{
    xlprint(getvalue(s_stderr),expr,TRUE);
    xlterpri(getvalue(s_stderr));
}

/* errputstr - print a string to *error-output* */
errputstr(str)
  char *str;
{
    xlputstr(getvalue(s_stderr),str);
}

/* dbgprint - print to *debug-io* */
dbgprint(expr)
  LVAL expr;
{
    xlprint(getvalue(s_debugio),expr,TRUE);
    xlterpri(getvalue(s_debugio));
}

/* dbgputstr - print a string to *debug-io* */
dbgputstr(str)
  char *str;
{
    xlputstr(getvalue(s_debugio),str);
}

/* trcprin1 - print to *trace-output* */
trcprin1(expr)
  LVAL expr;
{
    xlprint(getvalue(s_traceout),expr,TRUE);
}

/* trcputstr - print a string to *trace-output* */
trcputstr(str)
  char *str;
{
    xlputstr(getvalue(s_traceout),str);
}


