/*
* -*-C-*-
********************************************************************************
*
* File:         xlisp.h
* RCS:          $Header: xlisp.h,v 1.6 89/12/17 19:05:05 mayer Exp $
* Description:  libXlisp.a external interfaces
* Author:       David Michael Betz; Niels Mayer
* Created:      
* Modified:     Sun Dec 17 04:50:59 1989 (Niels Mayer) mayer@hplnpm
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

#ifndef __XLISP_H__
#define __XLISP_H__

#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>

/* NNODES	number of nodes to allocate in each request (1000) */
/* EDEPTH	evaluation stack depth (2000) */
/* ADEPTH	argument stack depth (1000) */
/* FORWARD	type of a forward declaration () */
/* LOCAL	type of a local function (static) */
/* AFMT		printf format for addresses ("%x") */
/* FIXTYPE	data type for fixed point numbers (long) */
/* ITYPE	fixed point input conversion routine type (long atol()) */
/* ICNV		fixed point input conversion routine (atol) */
/* IFMT		printf format for fixed point numbers ("%ld") */
/* FLOTYPE	data type for floating point numbers (float) */
/* OFFTYPE	number the size of an address (int) */


/* for BSD & SYSV Unix. */
#ifdef UNIX
#define NNODES		2000
#define AFMT		"%lx"	/* added by NPM */
#define OFFTYPE		long	/* added by NPM */
#define SAVERESTORE
#endif

/* for Mips C compiler - Silicon Graphhics */
#ifdef _BSD_COMPAT
#define LOCAL
#endif

/* for the Turbo C compiler - MS-DOS, large model */
#ifdef _TURBOC_
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define SAVERESTORE
#endif

/* for the AZTEC C compiler - MS-DOS, large model */
#ifdef AZTEC_LM
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define CVPTR(x)	ptrtoabs(x)
#define NIL		(void *)0
extern long ptrtoabs();
#define SAVERESTORE
#endif

/* for the AZTEC C compiler - Macintosh */
#ifdef AZTEC_MAC
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define NIL		(void *)0
#define SAVERESTORE
#endif

/* for the AZTEC C compiler - Amiga */
#ifdef AZTEC_AMIGA
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define NIL		(void *)0
#define SAVERESTORE
#endif

/* for the Lightspeed C compiler - Macintosh */
#ifdef LSC
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define NIL		(void *)0
#define SAVERESTORE
#endif

/* for the Microsoft C compiler - MS-DOS, large model */
#ifdef MSC
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#endif

/* for the Mark Williams C compiler - Atari ST */
#ifdef MWC
#define AFMT		"%lx"
#define OFFTYPE		long
#endif

/* for the Lattice C compiler - Atari ST */
#ifdef LATTICE
#define FIXTYPE		int
#define ITYPE		int atoi()
#define ICNV(n)		atoi(n)
#define IFMT		"%d"
#endif

/* for the Digital Research C compiler - Atari ST */
#ifdef DR
#define LOCAL
#define AFMT		"%lx"
#define OFFTYPE		long
#undef NULL
#define NULL		0L
#endif


/* default important definitions */
#ifndef NNODES
#define NNODES		1000
#endif
#ifndef EDEPTH
#define EDEPTH		2000
#endif
#ifndef ADEPTH
#define ADEPTH		1000
#endif
#ifndef FORWARD
#define FORWARD
#endif
#ifndef LOCAL
#define LOCAL		static
#endif
#ifndef AFMT
#define AFMT		"%x"
#endif
#ifndef FIXTYPE
#define FIXTYPE		long
#endif
#ifndef ITYPE
#define ITYPE		long atol()
#endif
#ifndef ICNV
#define ICNV(n)		atol(n)
#endif
#ifndef IFMT
#define IFMT		"%ld"
#endif
#ifndef FLOTYPE
#define FLOTYPE		double
#endif
#ifndef OFFTYPE
#define OFFTYPE		int
#endif
#ifndef CVPTR
#define CVPTR(x)	(x)
#endif
#ifndef UCHAR
#define UCHAR		unsigned char
#endif

/* useful definitions */
#ifndef TRUE
#define TRUE	(1)
#endif
#ifndef FALSE
#define FALSE	(0)
#endif
#ifndef NIL
#define NIL	(LVAL )0
#endif

/* instance variable numbers for the class 'Class' */
#define MESSAGES	0	/* list   of messages */
#define IVARS		1	/* list   of instance variable names */
#define CVARS		2	/* list   of class variable names */
#define CVALS		3	/* vector of class variable values */
#define SUPERCLASS	4	/* pointer to the superclass */
#define IVARCNT		5	/* number of class instance variables */
#define IVARTOTAL	6	/* total number of instance variables */
/* number of instance variables for the class 'Class' */
#define CLASSSIZE	7

/* Include PROVIDE_XXX #defines for extension modules. *//* JSP */
#define MODULE_XLISP_H_PROVIDES
#include "../../xmodules.h"
#undef MODULE_XLISP_H_PROVIDES

/* include the dynamic memory definitions */
#include "xldmem.h"

/* program limits */
#define STRMAX		100		/* maximum length of a string constant */
#define HSIZE		199		/* symbol hash table size */
#define SAMPLE		100		/* control character sample rate */

/* function table offsets for the initialization functions */
#define FT_RMHASH	0
#define FT_RMQUOTE	1
#define FT_RMDQUOTE	2
#define FT_RMBQUOTE	3
#define FT_RMCOMMA	4
#define FT_RMLPAR	5
#define FT_RMRPAR	6
#define FT_RMSEMI	7
/* #define xxxxxx       8 */
/* #define yyyyyy       9 */

#define FT_CLNEW	10
#define FT_CLISNEW	11
#define FT_CLANSWER	12
#define FT_OBISNEW	13
#define FT_OBCLASS	14
#define FT_OBSHOW	15

#define LAST_FUNTAB_POINTER_USED_BY_libXlisp FT_OBSHOW

/* include hybrid function in xlisp symbol table */ /* Voodoo */
/* use from within user implemented xlinclude_hybrid_prims */	
/* or from within user implemented .h which xmodules.h includes */
#define DEFINE_SUBR(a,b)   xldefine_prim(a, SUBR, b);
#define DEFINE_FSUBR(a,b)  xldefine_prim(a, FSUBR, b);

/* macro to push a value onto the argument stack */
#define pusharg(x)	{if (xlsp >= xlargstktop) xlargstkoverflow();\
			 *xlsp++ = (x);}

/* macros to protect pointers */
#define xlstkcheck(n)	{if (xlstack - (n) < xlstkbase) xlstkoverflow();}
#define xlsave(n)	{*--xlstack = &n; n = NIL;}
#define xlprotect(n)	{*--xlstack = &n;}

/* check the stack and protect a single pointer */
#define xlsave1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n; n = NIL;}
#define xlprot1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n;}

/* macros to pop pointers off the stack */
#define xlpop()		{++xlstack;}
#define xlpopn(n)	{xlstack+=(n);}

/* macros to manipulate the lexical environment */
#define xlframe(e)	cons(NIL,e)
#define xlbind(s,v)	xlpbind(s,v,xlenv)
#define xlfbind(s,v)	xlpbind(s,v,xlfenv);
#define xlpbind(s,v,e)	{rplaca(e,cons(cons(s,v),car(e)));}

/* macros to manipulate the dynamic environment */
#define xldbind(s,v)	{xldenv = cons(cons(s,getvalue(s)),xldenv);\
			 setvalue(s,v);}
#define xlunbind(e)	{for (; xldenv != (e); xldenv = cdr(xldenv))\
			   setvalue(car(car(xldenv)),cdr(car(xldenv)));}

/* type predicates */			       
#define atom(x)		((x) == NIL || ntype(x) != CONS)
#define null(x)		((x) == NIL)
#define listp(x)	((x) == NIL || ntype(x) == CONS)
#define consp(x)	((x) && ntype(x) == CONS)
#define subrp(x)	((x) && ntype(x) == SUBR)
#define fsubrp(x)	((x) && ntype(x) == FSUBR)
#define stringp(x)	((x) && ntype(x) == STRING)
#define symbolp(x)	((x) && ntype(x) == SYMBOL)
#define streamp(x)	((x) && ntype(x) == STREAM)

#define objectp(x)	((x) && ntype(x) == OBJECT)

#define fixp(x)		((x) && ntype(x) == FIXNUM)
#define floatp(x)	((x) && ntype(x) == FLONUM)
#define vectorp(x)	((x) && ntype(x) == VECTOR)
#define closurep(x)	((x) && ntype(x) == CLOSURE)
#define charp(x)	((x) && ntype(x) == CHAR)
#define ustreamp(x)	((x) && ntype(x) == USTREAM)
#define structp(x)	((x) && ntype(x) == STRUCT)
#define boundp(x)	(getvalue(x) != s_unbound)
#define fboundp(x)	(getfunction(x) != s_unbound)

/* shorthand functions */
#define consa(x)	cons(x,NIL)
#define consd(x)	cons(NIL,x)

/* set element of a vector */ /* Voodoo */
#define stuff_fixnum(arg, ind, val) ((arg)->n_vdata[ind])->n_fixnum = (val)
#define stuff_flonum(arg, ind, val) ((arg)->n_vdata[ind])->n_flonum = (val)

/* argument list parsing macros */
#define xlgetarg()	(testarg(nextarg()))
#define xllastarg()	{if (xlargc != 0) xltoomany();}
#define testarg(e)	(moreargs() ? (e) : xltoofew())
#define typearg(tp)	(tp(*xlargv) ? nextarg() : xlbadtype(*xlargv))
#define nextarg()	(--xlargc, *xlargv++)
#define moreargs()	(xlargc > 0)

/* macros to get arguments of a particular type */
#define xlgacons()	(testarg(typearg(consp)))
#define xlgalist()	(testarg(typearg(listp)))
#define xlgasymbol()	(testarg(typearg(symbolp)))
#define xlgastring()	(testarg(typearg(stringp)))
#define xlgaobject()	(testarg(typearg(objectp)))
#define xlgafixnum()	(testarg(typearg(fixp)))
#define xlgaflonum()	(testarg(typearg(floatp)))
#define xlgachar()	(testarg(typearg(charp)))
#define xlgavector()	(testarg(typearg(vectorp)))
#define xlgastream()	(testarg(typearg(streamp)))
#define xlgaustream()	(testarg(typearg(ustreamp)))
#define xlgaclosure()	(testarg(typearg(closurep)))
#define xlgastruct()	(testarg(typearg(structp)))

#ifndef OPTIMAL		/* Voodoo */
#define xlsetjmp(context) 		setjmp(context)
#define xllongjmp(context, mask) 	longjmp(context, mask)
#else
#define xlsetjmp(context) 		0
#define xllongjmp(context, mask) \
{ \
    xlfatal("can't recover, bye..."); \
    exit(0); \
    }
#endif

/* function definition structure */
typedef struct {
    char *fd_name;	/* function name */
    int fd_type;	/* function type */
    LVAL (*fd_subr)();	/* function entry point */
} FUNDEF;

/* execution context flags */
#define CF_GO		0x0001
#define CF_RETURN	0x0002
#define CF_THROW	0x0004
#define CF_ERROR	0x0008
#define CF_CLEANUP	0x0010
#define CF_CONTINUE	0x0020
#define CF_TOPLEVEL	0x0040
#define CF_BRKLEVEL	0x0080
#define CF_UNWIND	0x0100

/* execution context */
typedef struct context {
    int c_flags;			/* context type flags */
    LVAL c_expr;			/* expression (type dependant) */
    jmp_buf c_jmpbuf;			/* longjmp context */
    struct context *c_xlcontext;	/* old value of xlcontext */
    LVAL **c_xlstack;			/* old value of xlstack */
    LVAL *c_xlargv;			/* old value of xlargv */
    int c_xlargc;			/* old value of xlargc */
    LVAL *c_xlfp;			/* old value of xlfp */
    LVAL *c_xlsp;			/* old value of xlsp */
    LVAL c_xlenv;			/* old value of xlenv */
    LVAL c_xlfenv;			/* old value of xlfenv */
    LVAL c_xldenv;			/* old value of xldenv */
} CONTEXT;

/* external variables */
extern LVAL **xlstktop;       	/* top of the evaluation stack */
extern LVAL **xlstkbase;	/* base of the evaluation stack */
extern LVAL **xlstack;		/* evaluation stack pointer */
extern LVAL *xlargstkbase;	/* base of the argument stack */
extern LVAL *xlargstktop;	/* top of the argument stack */
extern LVAL *xlfp;		/* argument frame pointer */
extern LVAL *xlsp;		/* argument stack pointer */
extern LVAL *xlargv;		/* current argument vector */
extern int xlargc;		/* current argument count */

/* external procedure declarations */
extern LVAL xleval();		/* evaluate an expression */
extern LVAL xlapply();		/* apply a function to arguments */
extern LVAL xlsubr();		/* enter a subr/fsubr */
extern LVAL xlenter();		/* enter a symbol */
extern LVAL xlmakesym();	/* make an uninterned symbol */
extern LVAL xlgetvalue();	/* get value of a symbol (checked) */
extern LVAL xlxgetvalue();	/* get value of a symbol */
extern LVAL xlgetfunction();	/* get functional value of a symbol */
extern LVAL xlxgetfunction();	/* get functional value of a symbol (checked) */
extern LVAL xlexpandmacros();	/* expand macros in a form */
extern LVAL xlgetprop();	/* get the value of a property */
extern LVAL xlclose();		/* create a function closure */

extern void xldefine_prim();	/* load xlisp function */ /* Voodoo */

/* argument list parsing functions */
extern LVAL xlgetfile();      	/* get a file/stream argument */
extern LVAL xlgetfname();	/* get a filename argument */

/* error reporting functions (don't *really* return at all) */
extern LVAL xltoofew();		/* report "too few arguments" error */
extern LVAL xlbadtype();	/* report "bad argument type" error */


/* Include hybrid-class functions. *//* JSP */
/* (Last so you can #undef stuff.) *//* JSP */
#define MODULE_XLISP_H_GLOBALS
#include "../../xmodules.h"
#undef MODULE_XLISP_H_GLOBALS

#endif /* __XLISP_H__ */
