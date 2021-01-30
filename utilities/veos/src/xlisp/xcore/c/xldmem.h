/* -*-C-*-
********************************************************************************
*
* File:         xldmem.h
* RCS:          $Header: xldmem.h,v 1.7 89/11/25 05:22:56 mayer Exp $
* Description:  dynamic memory definitions
* Author:       David Michael Betz; Niels Mayer
* Created:      
* Modified:     Sat Nov 25 05:22:46 1989 (Niels Mayer) mayer@hplnpm
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


/* small fixnum range */
#define SFIXMIN		(-128)
#define SFIXMAX		255
#define SFIXSIZE	384

/* character range */
#define CHARMIN		0
#define CHARMAX		255
#define CHARSIZE	256

/* new node access macros */
#define ntype(x)	((x)->n_type)

/* cons access macros */
#define car(x)		((x)->n_car)
#define cdr(x)		((x)->n_cdr)
#define rplaca(x,y)	((x)->n_car = (y))
#define rplacd(x,y)	((x)->n_cdr = (y))

/* symbol access macros */
#define getvalue(x)	 ((x)->n_vdata[0])
#define setvalue(x,v)	 ((x)->n_vdata[0] = (v))
#define getfunction(x)	 ((x)->n_vdata[1])
#define setfunction(x,v) ((x)->n_vdata[1] = (v))
#define getplist(x)	 ((x)->n_vdata[2])
#define setplist(x,v)	 ((x)->n_vdata[2] = (v))
#define getpname(x)	 ((x)->n_vdata[3])
#define setpname(x,v)	 ((x)->n_vdata[3] = (v))
#define SYMSIZE		4

/* closure access macros */
#define getname(x)     	((x)->n_vdata[0])
#define setname(x,v)   	((x)->n_vdata[0] = (v))
#define gettype(x)    	((x)->n_vdata[1])
#define settype(x,v)  	((x)->n_vdata[1] = (v))
#define getargs(x)     	((x)->n_vdata[2])
#define setargs(x,v)   	((x)->n_vdata[2] = (v))
#define getoargs(x)    	((x)->n_vdata[3])
#define setoargs(x,v)  	((x)->n_vdata[3] = (v))
#define getrest(x)     	((x)->n_vdata[4])
#define setrest(x,v)   	((x)->n_vdata[4] = (v))
#define getkargs(x)    	((x)->n_vdata[5])
#define setkargs(x,v)  	((x)->n_vdata[5] = (v))
#define getaargs(x)    	((x)->n_vdata[6])
#define setaargs(x,v)  	((x)->n_vdata[6] = (v))
#define getbody(x)     	((x)->n_vdata[7])
#define setbody(x,v)   	((x)->n_vdata[7] = (v))
#define xlgetenv(x)	((x)->n_vdata[8])
#define setenv(x,v)	((x)->n_vdata[8] = (v))
#define getfenv(x)	((x)->n_vdata[9])
#define setfenv(x,v)	((x)->n_vdata[9] = (v))
#define getlambda(x)	((x)->n_vdata[10])
#define setlambda(x,v)	((x)->n_vdata[10] = (v))
#define CLOSIZE		11

/* vector access macros */
#define getsz(x)	((x)->n_vsize)
#define getelement(x,i)	((x)->n_vdata[i])
#define setelement(x,i,v) ((x)->n_vdata[i] = (v))

/* object access macros */
#define getclass(x)	((x)->n_vdata[0])
#define getivar(x,i)	((x)->n_vdata[i+1])
#define setivar(x,i,v)	((x)->n_vdata[i+1] = (v))

/* subr/fsubr access macros */
#define getsubr(x)	((x)->n_subr)
#define getoffset(x)	((x)->n_offset)

/* fixnum/flonum/char access macros */
#define getfixnum(x)    ((x)->n_fixnum)
#define getflonum(x)	((x)->n_flonum)
#define setflonum(x, val)  (x)->n_flonum = (val) /* Voodoo */
#define getchcode(x)	((x)->n_chcode)

/* string access macros */
#define getstring(x)	((x)->n_string)
#define getslength(x)	((x)->n_strlen)

/* file stream access macros */
#define getfile(x)	((x)->n_fp)
#define setfile(x,v)	((x)->n_fp = (v))
#define getsavech(x)	((x)->n_savech)
#define setsavech(x,v)	((x)->n_savech = (v))

/* unnamed stream access macros */
#define gethead(x)	((x)->n_car)
#define sethead(x,v)	((x)->n_car = (v))
#define gettail(x)	((x)->n_cdr)
#define settail(x,v)	((x)->n_cdr = (v))

/* node types */
#define FREE	0
#define SUBR	1
#define FSUBR	2
#define CONS	3
#define SYMBOL	4
#define FIXNUM	5
#define FLONUM	6
#define STRING	7
#define OBJECT	8
#define STREAM	9
#define VECTOR	10
#define CLOSURE	11
#define CHAR	12
#define USTREAM	13
#define STRUCT	14

/* Left the n_type definitions here rather */
/* than moving them to xwinterp.h and      */
/* gobject.h because inadvertent collisions*/
/* would be a disaster.                    */
#ifdef PROVIDE_WINTERP
#define XLTYPE_XtAccelerators		15
#define XLTYPE_XtTranslations		16
#define XLTYPE_XtCallbackList		17
#define XLTYPE_XEvent			18
#define XLTYPE_Window			19
#define XLTYPE_Pixel			20
#define XLTYPE_Pixmap			21
#define XLTYPE_XImage                   22
#define XLTYPE_XmString			23
#define XLTYPE_XmFontList		24
#define XLTYPE_caddr_t			25 /* generic pointer */
#define XLTYPE_XT_RESOURCE              26
#define XLTYPE_CALLBACKOBJ              27
#define XLTYPE_TIMEOUTOBJ               28
#define XLTYPE_PIXMAP_REFOBJ		29
#define XLTYPE_WIDGETOBJ                30
#define XLTYPE_EVHANDLEROBJ		31
#endif
#ifdef PROVIDE_XGBJ
/* Pick a number well away from the winterp progression, */
/* but not large enough to invite signed-char bugs:      */
#define GOBJECT	(97)
#endif



/* subr/fsubr node */
#define n_subr		n_info.n_xsubr.xs_subr
#define n_offset	n_info.n_xsubr.xs_offset
  
/* cons node */
#define n_car		n_info.n_xcons.xc_car
#define n_cdr		n_info.n_xcons.xc_cdr

/* fixnum node */
#define n_fixnum	n_info.n_xfixnum.xf_fixnum

/* flonum node */
#define n_flonum	n_info.n_xflonum.xf_flonum
/* character node */
#define n_chcode	n_info.n_xchar.xc_chcode

/* string node */
#define n_string	n_info.n_xstring.xs_string
#define n_strlen	n_info.n_xstring.xs_length

/* stream node */
#define n_fp		n_info.n_xstream.xs_fp
#define n_savech	n_info.n_xstream.xs_savech

/* vector/object node */
#define n_vsize		n_info.n_xvector.xv_size
#define n_vdata		n_info.n_xvector.xv_data

/* node structure */
typedef struct node {
    char n_type;		/* type of node */
    char n_flags;		/* flag bits */
    union ninfo { 		/* value */

	struct xsubr {		/* subr/fsubr node */
	    struct node *(*xs_subr)();	/* function pointer */
	    int xs_offset;		/* offset into funtab */
	} n_xsubr;
	struct xcons {		/* cons node */
	    struct node *xc_car;	/* the car pointer */
	    struct node *xc_cdr;	/* the cdr pointer */
	} n_xcons;
	struct xfixnum {	/* fixnum node */
	    FIXTYPE xf_fixnum;		/* fixnum value */
	} n_xfixnum;
	struct xflonum {	/* flonum node */
	    FLOTYPE xf_flonum;		/* flonum value */
	} n_xflonum;
	struct xchar {		/* character node */
	    int xc_chcode;		/* character code */
	} n_xchar;
	struct xstring {	/* string node */
	    int xs_length;		/* string length */
	    unsigned char *xs_string;	/* string pointer */
	} n_xstring;
	struct xstream { 	/* stream node */
	    FILE *xs_fp;		/* the file pointer */
	    int xs_savech;		/* lookahead character */
	} n_xstream;
	struct xvector {	/* vector/object/symbol/structure node */
	    int xv_size;		/* vector size */
	    struct node **xv_data;	/* vector data */
	} n_xvector;
/* Include hybrid-class functions: *//* JSP */
#define MODULE_XLDMEM_H_NINFO
#include "../../xmodules.h"
#undef MODULE_XLDMEM_H_NINFO

    } n_info;
} *LVAL;

/* memory segment structure definition */
typedef struct segment {
    int sg_size;
    struct segment *sg_next;
    struct node sg_nodes[1];
} SEGMENT;

/* memory allocation functions */
extern LVAL cons();		/* (cons x y) */
extern LVAL cvsymbol();       	/* convert a string to a symbol */
extern LVAL cvstring();       	/* convert a string */
extern LVAL cvfile();		/* convert a FILE * to a file */
extern LVAL cvsubr();		/* convert a function to a subr/fsubr */
extern LVAL cvfixnum();       	/* convert a fixnum */
extern LVAL cvflonum();       	/* convert a flonum */
extern LVAL cvchar();		/* convert a character */

extern LVAL newstring();	/* create a new string */
extern LVAL newvector();	/* create a new vector */
extern LVAL newobject();	/* create a new object */
extern LVAL newclosure();	/* create a new closure */
extern LVAL newustream();	/* create a new unnamed stream */
extern LVAL newstruct();	/* create a new structure */

extern LVAL s_self,k_new,k_isnew; /* Symbol SELF, keywords :ISNEW :NEW *//*JSP*/
extern LVAL cls_class,cls_object; /* Class objects for CLASS and OBJECT*//*JSP*/

/* Include hybrid-class functions: *//* JSP */
#define MODULE_XLDMEM_H_GLOBALS
#include "../../xmodules.h"
#undef MODULE_XLDMEM_H_GLOBALS



