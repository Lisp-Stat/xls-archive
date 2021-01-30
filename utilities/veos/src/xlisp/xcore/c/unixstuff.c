/* -*-C-*-
********************************************************************************
*
* File:         unixstuff.c
* RCS:          $Header: unixstuff.c,v 1.3 89/11/25 05:12:16 mayer Exp $
* Description:  UNIX-Specific interfaces for XLISP
* Author:       David Michael Betz; Niels Mayer
* Created:      
* Modified:     Sat Nov 25 05:12:04 1989 (Niels Mayer) mayer@hplnpm
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
static char rcs_identity[] = "@(#)$Header: unixstuff.c,v 1.3 89/11/25 05:12:16 mayer Exp $";


#include "xlisp.h"

/******************************************************************************
 * Prim_POPEN - start a process and open a pipe for read/write 
 * (code stolen from xlfio.c:xopen())
 *
 * syntax: (popen <command line> :direction <direction>)
 *                <command line> is a string to be sent to the subshell (sh).
 *                <direction> is either :input (to read from the pipe) or
 *                                      :output (to write to the pipe).
 *                                      (:input is the default)
 *
 * Popen returns a stream, or NIL if files or processes couldn't be created.
 * The  success  of  the  command  execution  can be checked by examining the 
 * return value of pclose. 
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_POPEN()
{
  extern LVAL k_direction, k_input, k_output;
  char *name,*mode;
  FILE *fp;
  LVAL dir;

  /* get the process name and direction */
  name = (char *) getstring(xlgastring());
  if (!xlgetkeyarg(k_direction, &dir))
    dir = k_input;
  
  /* get the mode */
  if (dir == k_input)
    mode = "r";
  else if (dir == k_output)
    mode = "w";
  else
    xlerror("bad direction",dir);
  
  /* try to open the file */
  return ((fp = popen(name,mode)) ? cvfile(fp) : NIL);
}


/******************************************************************************
 * Prim_PCLOSE - close a pipe opened by Prim_POPEN().
 * (code stolen from xlfio.c:xclose())
 *
 * syntax: (pclose <stream>)
 *                  <stream> is a stream created by popen.
 * returns T if the command executed successfully, otherwise, 
 * returns the exit status of the opened command.
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_PCLOSE()
{
  extern LVAL true;
  LVAL fptr;
  int  result;

  /* get file pointer */
  fptr = xlgastream();
  xllastarg();

  /* make sure the file exists */
  if (getfile(fptr) == NULL)
    xlfail("file not open");

  /* close the pipe */
  result = pclose(getfile(fptr));

  if (result == -1)
    xlfail("<stream> has not been opened with popen");
    
  setfile(fptr,NULL);

  /* return T if success (exit status 0), else return exit status */
  return (result ? cvfixnum(result) : true);
}


/******************************************************************************
 * Prim_SYSTEM - run a process, sending output (if any) to stdout/stderr
 *
 * syntax: (system <command line>)
 *                 <command line> is a string to be sent to the subshell (sh).
 *
 * Returns T if the command executed succesfully, otherwise returns the 
 * integer shell exit status for the command.
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_SYSTEM()
{
  extern LVAL true;
  extern int sys_nerr;
  extern char *sys_errlist[];
  extern int errno;
  LVAL command;
  int  result;
  char temptext[1024];

  /* get shell command */
  command = xlgastring();
  xllastarg();
  
  /* run the process */
  result = system((char *) getstring(command));

  if (result == -1) {		/* if a system error has occured */
    if (errno < sys_nerr)
      (void) sprintf(temptext, "Error in system(3S): %s\n", sys_errlist[errno]);
    else
      (void) strcpy(temptext, "Error in system(3S): unknown error\n");
    xlfail(temptext);
  }

  /* return T if success (exit status 0), else return exit status */
  return (result ? cvfixnum(result) : true);
}


/******************************************************************************
 * (FSCANF-FIXNUM <stream> <scanf-format>)
 * This routine calls fscanf(3s) on a <stream> that was previously openend
 * via open or popen. It will not work on an USTREAM.
 * <scanf-format> is a format string containing a single conversion
 * directive that will result in an integer valued conversion.
 * %d, %u, %o, %x, %ld, %lu, %lo and %lx style conversions 
 * are acceptable for this routine.
 * WARNING: specifying a <scanf-format> that will result in the conversion
 * of a result larger than sizeof(long) will result in corrupted memory and
 * core dumps. 
 * 
 * This routine will return an FIXNUM if fscanf() returns 1 (i.e. if
 * the one expected conversion has succeeded. It will return NIL if the
 * conversion wasn't successful, or if EOF was reached.
 ******************************************************************************/
LVAL Prim_FSCANF_FIXNUM()
{
  LVAL  lval_stream;
  char* fmt;
  long  result;
  
  lval_stream = xlgastream();
  if (getfile(lval_stream) == NULL)
    xlerror("File not opened.", lval_stream);
  fmt = (char *) getstring(xlgastring());
  xllastarg();
  
  result = 0L;			/* clear it out hibits incase short is written */
  /* if scanf returns result <1 then an error or eof occured. */
  if (fscanf(getfile(lval_stream), fmt, &result) < 1)
    return (NIL);
  else
    return (cvfixnum((FIXTYPE) result));
}


/******************************************************************************
 * (FSCANF-STRING <stream> <scanf-format>)
 * This routine calls fscanf(3s) on a <stream> that was previously openend
 * via open or popen. It will not work on an USTREAM.
 * <scanf-format> is a format string containing a single conversion
 * directive that will result in a string valued conversion.
 * %s, %c, and %[...] style conversions are acceptable for
 * this routine.
 * WARNING: specifying a <scanf-format> that will result in the conversion
 * of a result larger than 1024 characters will result in corrupted
 * memory and core dumps.
 * 
 * This routine will return a string if fscanf() returns 1 (i.e. if
 * the one expected conversion has succeeded. It will return NIL if the
 * conversion wasn't successful, or if EOF was reached.
 ******************************************************************************/
LVAL Prim_FSCANF_STRING()
{
  LVAL lval_stream;
  char* fmt;
  char result[BUFSIZ];

  
  lval_stream = xlgastream();
  if (getfile(lval_stream) == NULL)
    xlerror("File not opened.", lval_stream);
  fmt = (char *) getstring(xlgastring());
  xllastarg();
  
  result[0] = result[1] = '\0';	/* if the conversion is %c, then fscanf
				   doesn't null terminate the string,
				   so do it just incase */

  /* if scanf returns result <1 then an error or eof occured. */
  if (fscanf(getfile(lval_stream), fmt, result) < 1)
    return (NIL);
  else
    return (cvstring(result));
}


/******************************************************************************
 * (FSCANF-FLONUM <stream> <scanf-format>)
 * This routine calls fscanf(3s) on a <stream> that was previously openend
 * via open or popen. It will not work on an USTREAM.
 * <scanf-format> is a format string containing a single conversion
 * directive that will result in an FLONUM valued conversion.
 * %e %f or %g are valid conversion specifiers for this routine.
 *
 * WARNING: specifying a <scanf-format> that will result in the conversion
 * of a result larger than sizeof(float) will result in corrupted memory and
 * core dumps. 
 * 
 * This routine will return a FLONUM if fscanf() returns 1 (i.e. if
 * the one expected conversion has succeeded. It will return NIL if the
 * conversion wasn't successful, or if EOF was reached.
 ******************************************************************************/
LVAL Prim_FSCANF_FLONUM()
{
  LVAL lval_stream;
  char* fmt;
  FILE * fp;
  float result;
  
  lval_stream = xlgastream();
  if (getfile(lval_stream) == NULL)
    xlerror("File not opened.", lval_stream);
  fmt = (char *) getstring(xlgastring());
  xllastarg();
  
  /* if scanf returns result <1 then an error or eof occured. */
  if (fscanf(getfile(lval_stream), fmt, &result) < 1)
    return (NIL);
  else
    return (cvflonum((FLOTYPE) result));
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/* -- stuff.c  -- operating system specific routines */
/* -- Written by dbetz for XLISP 2.0 */
/* -- Copied by EFJohnson from a BIX message */
/* -- Unix System V */

#define	LBSIZE	200

/* -- external variables */
extern	FILE	*tfp;

/* -- local variables */
static	long	rseed = 1L;

static	char	lbuf[LBSIZE];
static	int	lindex;
static	int	lcount;


/* -- osinit - initialize */
osinit(banner)
char	*banner;
{
	printf("%s\n", banner );
	lindex	= 0;
	lcount	= 0;
}

/* -- osfinish - clean up before returning to the operating system */
osfinish()
{
}


/* -- xoserror - print an error message */
xoserror(msg)

char	*msg;

{
	printf( "error: %s\n", msg );
}


/* -- osrand - return a random number between 0 and n-1 */
int osrand(n)

int	n;

{
	long k1;

	/* -- make sure we don't get stuck at zero */
	if ( rseed == 0L ) rseed = 1L;

	/* -- algorithm taken from Dr Dobbs Journal, Nov. 1985, page 91 */
	k1 = rseed / 127773L;
	if ( ( rseed = 16807L * (rseed - k1 * 127773L) -k1 * 2836L) < 0L )
		rseed += 2147483647L;

	/* -- return a random number between 0 and n-1 */
	return( (int) (rseed % (long) n ) );
}



/* -- osaopen -- open an ascii file */
FILE	*osaopen( name, mode )
char	*name, *mode;
{
	return( fopen( name, mode ) );
}



/* -- osbopen -- open a binary file */
FILE	*osbopen( name, mode )
char	*name, *mode;
{
	return( fopen( name, mode ) );
}


/* -- osclose -- close a file */
int	osclose( fp )
FILE	*fp;
{
	return( fclose( fp ) );
}


/* -- osagetc - get a character from an ASCII file */
int	osagetc( fp )
FILE	*fp;
{
	return( getc(fp) );
}

/* -- osaputc - put a character to an ASCII file */
int	osaputc( ch, fp )
int	ch;
FILE	*fp;
{
	return( putc( ch, fp ) );
}



/* -- osbgetc - get a character from a binary file */
int	osbgetc( fp )
FILE	*fp;
{
	return( getc(fp) );
}

/* -- osbputc - put a character to a binary file */
int	osbputc( ch, fp )
int	ch;
FILE	*fp;
{
	return( putc( ch, fp ) );
}


/* -- ostgetc - get a character from the terminal */
int	ostgetc()
{
	while(--lcount < 0 )
		{
		if ( fgets(lbuf,LBSIZE,stdin) == NULL )
			return( EOF );
		if ( tfp )
			fputs( lbuf, tfp );
		lcount = strlen( lbuf );
		lindex = 0;
		}

	return( lbuf[lindex++] );
}


/* -- ostputc - put a character to the terminal */
ostputc( ch )
int	ch;
{
	/* -- check for control characters */
	oscheck();
	
	/* -- output the character */
	putchar( ch );

	/* -- output the char to the transcript file */
	if ( tfp )
		osaputc( ch, tfp );
}




/* -- osflush - flush the terminal input buffer */
osflush()
{
	lindex = lcount = 0;
}


/* -- oscheck - check for control characters during execution */
oscheck()
{
}


/* -- ossymbols - enter os-specific symbols */
ossymbols()
{
}

/******************************************************************************
 * xosgetenv - get string from environment
 * 
 * syntax: (getenv key)
 *                <key> is something like TERM to look up in the unix environment.
 * 
 * If "<key>=<val> is not found in the environment, xosgetenv returns NIL.
 * Otherwise, xosgetenv returns a list of strings, one for each ':'-delimited
 * component of <val>.
 *
 * Added to XLISP by Jeff Prothero
 ******************************************************************************/
LVAL envget( key_as_asciz )
char*        key_as_asciz;
{
    extern char* getenv();
    LVAL result;
    char *val_as_asciz = getenv( key_as_asciz );
    xlsave1( result );
    if (val_as_asciz != NULL) {
	do {
	    char buf[ 1024 ];
            char *dst = buf;
	    while (*val_as_asciz   &&   *val_as_asciz != ':') {
		*dst++ = *val_as_asciz++;
	    }
	    *dst = '\0';
	    result = cons( cvstring(buf), result );
	} while (*val_as_asciz++);
    }
    xlpop();
    return result;
}
LVAL xosenvget()
{
    char *key_as_asciz = (char *) getstring(xlgastring());
    xllastarg();
    return envget( key_as_asciz );
}
