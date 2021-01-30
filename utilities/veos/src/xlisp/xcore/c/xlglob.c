/* -*-C-*-
********************************************************************************
*
* File:         xlglobals.c
* RCS:          $Header: xlglob.c,v 1.4 89/11/25 05:30:06 mayer Exp $
* Description:  xlisp global variables
* Author:       David Michael Betz
* Created:      
* Modified:     Sat Nov 25 05:29:22 1989 (Niels Mayer) mayer@hplnpm
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
static char rcs_identity[] = "@(#)$Header: xlglob.c,v 1.4 89/11/25 05:30:06 mayer Exp $";


#include "xlisp.h"

/* symbols */
LVAL true=NIL,obarray=NIL;
LVAL s_unbound=NIL,s_dot=NIL;
LVAL s_quote=NIL,s_function=NIL;
LVAL s_bquote=NIL,s_comma=NIL,s_comat=NIL;
LVAL s_evalhook=NIL,s_applyhook=NIL,s_tracelist;
LVAL s_lambda=NIL,s_macro=NIL;
LVAL s_send=NIL; /*91Jun15jsp*/
LVAL s_stdin=NIL,s_stdout=NIL,s_stderr=NIL,s_debugio=NIL,s_traceout=NIL;
LVAL s_rtable=NIL;
LVAL s_tracenable=NIL,s_tlimit=NIL,s_breakenable=NIL;
LVAL s_setf=NIL,s_car=NIL,s_cdr=NIL,s_nth=NIL,s_aref=NIL,s_get=NIL;
LVAL s_svalue=NIL,s_sfunction=NIL,s_splist=NIL;
LVAL s_eql=NIL,s_gcflag=NIL,s_gchook=NIL;
LVAL s_ifmt=NIL,s_ffmt=NIL;
LVAL s_1plus=NIL,s_2plus=NIL,s_3plus=NIL;
LVAL s_1star=NIL,s_2star=NIL,s_3star=NIL;
LVAL s_minus=NIL,s_printcase=NIL;

/* keywords */
LVAL k_test=NIL,k_tnot=NIL;
LVAL k_wspace=NIL,k_const=NIL,k_nmacro=NIL,k_tmacro=NIL;
LVAL k_sescape=NIL,k_mescape=NIL;
LVAL k_direction=NIL,k_input=NIL,k_output=NIL;
LVAL k_start=NIL,k_end=NIL,k_1start=NIL,k_1end=NIL;
LVAL k_2start=NIL,k_2end=NIL,k_count=NIL,k_key=NIL;
LVAL k_verbose=NIL,k_print=NIL;
LVAL k_upcase=NIL,k_downcase=NIL;

/* lambda list keywords */
LVAL lk_optional=NIL,lk_rest=NIL,lk_key=NIL,lk_aux=NIL;
LVAL lk_allow_other_keys=NIL;

/* type names */
LVAL a_subr=NIL,a_fsubr=NIL;
LVAL a_cons=NIL,a_symbol=NIL,a_fixnum=NIL,a_flonum=NIL;
LVAL a_string=NIL,a_object=NIL,a_stream=NIL,a_vector=NIL;
LVAL a_closure=NIL,a_char=NIL,a_ustream=NIL;

/* evaluation variables */
LVAL **xlstack = NULL,**xlstkbase = NULL,**xlstktop = NULL;
LVAL xlenv=NIL,xlfenv=NIL,xldenv=NIL;

/* argument stack */
LVAL *xlargstkbase = NULL;	/* argument stack base */
LVAL *xlargstktop = NULL;	/* argument stack top */
LVAL *xlfp = NULL;		/* argument frame pointer */
LVAL *xlsp = NULL;		/* argument stack pointer */
LVAL *xlargv = NULL;		/* current argument vector */
int xlargc = 0;			/* current argument count */

/* exception handling variables */
CONTEXT *xlcontext = NULL;	/* current exception handler */
CONTEXT *xltarget = NULL;	/* target context (for xljump) */
LVAL xlvalue=NIL;		/* exception value (for xljump) */
int xlmask=0;			/* exception type (for xljump) */

/* debugging variables */
int xldebug = 0;		/* debug level */
int xlsample = 0;		/* control character sample rate */
int xltrcindent = 0;		/* trace indent level */

/* gensym variables */
char gsprefix[STRMAX+1] = { 'G',0 };	/* gensym prefix string */
int gsnumber = 1;		/* gensym number */

/* i/o variables */
int xlfsize = 0;		/* flat size of current print call */
FILE *tfp = NULL;		/* transcript file pointer */

/* general purpose string buffer */
char buf[STRMAX+1] = { 0 };

/* Include hybrid-class functions: *//* JSP */
#define MODULE_XLGLOB_C_GLOBALS
#include "../../xmodules.h"
#undef MODULE_XLGLOB_C_GLOBALS

